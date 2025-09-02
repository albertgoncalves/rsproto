use std::collections::{HashMap, HashSet};
use std::fmt;
use std::mem;

type LabelArgs<'a> = (&'a str, Vec<&'a str>);

#[derive(Clone)]
enum Value<'a> {
    Ident(&'a str),
    Int(i64),
}

#[derive(Clone)]
enum Inst<'a> {
    Label(&'a str),
    Jump(LabelArgs<'a>),
    Branch(Value<'a>, LabelArgs<'a>, LabelArgs<'a>),
    Let(&'a str, Value<'a>),
    Call(Option<&'a str>, &'a str, Vec<Value<'a>>),
    Return(Option<Value<'a>>),
}

struct Block<'a>(&'a str, Vec<Inst<'a>>);

fn write_value(f: &mut fmt::Formatter<'_>, value: &Value<'_>) -> fmt::Result {
    match value {
        Value::Ident(ident) => write!(f, "{ident}"),
        Value::Int(int) => write!(f, "{int}"),
    }
}

fn write_label_args(f: &mut fmt::Formatter<'_>, label_args: &LabelArgs<'_>) -> fmt::Result {
    write!(f, "{}", label_args.0)?;
    if label_args.1.is_empty() {
        return Ok(());
    }
    write!(f, "<{}", label_args.1[0])?;
    for arg in &label_args.1[1..] {
        write!(f, ", {arg}")?;
    }
    write!(f, ">")
}

impl fmt::Display for Inst<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    ")?;
        match self {
            Self::Label(label) => {
                write!(f, "{label}:")
            }
            Self::Jump(label_args) => {
                write!(f, "    jump ")?;
                write_label_args(f, label_args)
            }
            Self::Branch(condition, r#true, r#false) => {
                write!(f, "    branch ")?;
                write_value(f, condition)?;
                write!(f, " ? ")?;
                write_label_args(f, r#true)?;
                write!(f, " : ")?;
                write_label_args(f, r#false)
            }
            Self::Let(ident, value) => {
                write!(f, "    let {ident} = ")?;
                write_value(f, value)
            }
            Self::Call(ident, func, args) => {
                write!(f, "    ")?;
                if let Some(ident) = ident {
                    write!(f, "let {ident} = ")?;
                }
                write!(f, "call {func}")?;
                if args.is_empty() {
                    return Ok(());
                }
                write!(f, "(")?;
                write_value(f, &args[0])?;
                for arg in &args[1..] {
                    write!(f, ", ")?;
                    write_value(f, arg)?;
                }
                write!(f, ")")
            }
            Self::Return(value) => {
                write!(f, "    return")?;
                let Some(value) = value else {
                    return Ok(());
                };
                write!(f, " ")?;
                write_value(f, value)
            }
        }
    }
}

impl fmt::Display for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "    {}:", self.0)?;
        for inst in &self.1 {
            writeln!(f, "{inst}")?;
        }
        Ok(())
    }
}

fn get_blocks<'a>(insts: &[Inst<'a>]) -> Vec<Block<'a>> {
    let mut blocks: Vec<Block<'a>> = vec![];
    for (i, inst) in insts.iter().enumerate() {
        if let Inst::Label(label) = inst {
            if i != 0
                && !matches!(insts[i - 1], Inst::Branch(..) | Inst::Jump(..) | Inst::Return(..))
            {
                let j = blocks.len() - 1;
                blocks[j].1.push(Inst::Jump((*label, vec![])));
            }
            blocks.push(Block(label, vec![]));
        } else {
            let i = blocks.len() - 1;
            blocks[i].1.push(inst.clone());
        }
    }

    for block in &blocks {
        let n = block.1.len() - 1;
        for inst in &block.1[..n] {
            assert!(!matches!(
                inst,
                Inst::Label(..) | Inst::Jump(..) | Inst::Branch(..) | Inst::Return(..),
            ));
        }
        assert!(matches!(block.1[n], Inst::Jump(..) | Inst::Branch(..) | Inst::Return(..)));
    }

    blocks
}

fn insert_local<'a>(locals: &mut HashSet<&'a str>, orphans: &HashSet<&'a str>, ident: &'a str) {
    if orphans.contains(ident) {
        return;
    }
    locals.insert(ident);
}

fn insert_orphan<'a>(
    globals: &HashSet<&'a str>,
    locals: &HashSet<&'a str>,
    orphans: &mut HashSet<&'a str>,
    ident: &'a str,
) {
    if locals.contains(ident) {
        return;
    }
    if globals.contains(ident) {
        return;
    }
    orphans.insert(ident);
}

fn get_block_args<'a>(
    blocks: &[Block<'a>],
    globals: &HashSet<&'a str>,
) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut block_args: HashMap<&'a str, HashSet<&'a str>> = HashMap::new();

    for block in blocks {
        drop(block_args.insert(block.0, HashSet::new()));
    }

    let mut repeat = true;
    while repeat {
        repeat = false;
        for block in blocks {
            let mut locals: HashSet<&'a str> = HashSet::new();
            let mut orphans: HashSet<&'a str> = HashSet::new();

            for inst in &block.1 {
                match inst {
                    Inst::Label(..) => unreachable!(),
                    Inst::Let(ident, value) => {
                        if let Value::Ident(value_ident) = value {
                            insert_orphan(globals, &locals, &mut orphans, value_ident);
                        }
                        insert_local(&mut locals, &orphans, ident);
                    }
                    Inst::Call(ident, func, args) => {
                        for arg in args {
                            if let Value::Ident(arg_ident) = arg {
                                insert_orphan(globals, &locals, &mut orphans, arg_ident);
                            }
                        }
                        insert_orphan(globals, &locals, &mut orphans, func);
                        if let Some(ident) = ident {
                            insert_local(&mut locals, &orphans, ident);
                        }
                    }
                    Inst::Return(Some(Value::Ident(ident))) => {
                        insert_orphan(globals, &locals, &mut orphans, ident);
                    }
                    Inst::Jump(label_args) => {
                        assert!(label_args.1.is_empty());
                        if let Some(args) = block_args.get(&label_args.0) {
                            for arg in args {
                                insert_orphan(globals, &locals, &mut orphans, arg);
                            }
                        }
                    }
                    Inst::Branch(condition, r#true, r#false) => {
                        if let Value::Ident(ident) = condition {
                            insert_orphan(globals, &locals, &mut orphans, ident);
                        }

                        assert!(r#true.1.is_empty());
                        if let Some(args) = block_args.get(&r#true.0) {
                            for arg in args {
                                insert_orphan(globals, &locals, &mut orphans, arg);
                            }
                        }

                        assert!(r#false.1.is_empty());
                        if let Some(args) = block_args.get(&r#false.0) {
                            for arg in args {
                                insert_orphan(globals, &locals, &mut orphans, arg);
                            }
                        }
                    }
                    Inst::Return(..) => (),
                }
            }

            let args: &mut HashSet<&'a str> = block_args.get_mut(&block.0).unwrap();
            for ident in orphans {
                repeat |= args.insert(ident);
            }
        }
    }

    block_args
}

fn get_insts(blocks: Vec<Block<'_>>) -> Vec<Inst<'_>> {
    let mut insts = vec![];
    for block in blocks {
        insts.push(Inst::Label(block.0));
        for inst in block.1 {
            insts.push(inst);
        }
    }
    insts
}

fn set_block_args<'a>(insts: &mut [Inst<'a>], block_args: &HashMap<&'a str, HashSet<&'a str>>) {
    for inst in insts {
        match inst {
            Inst::Jump(label_args) => {
                assert!(label_args.1.is_empty());
                label_args.1.extend(block_args[&label_args.0].iter());
                label_args.1.sort_unstable();
            }
            Inst::Branch(_, r#true, r#false) => {
                assert!(r#true.1.is_empty());
                r#true.1.extend(block_args[&r#true.0].iter());
                r#true.1.sort_unstable();

                assert!(r#false.1.is_empty());
                r#false.1.extend(block_args[&r#false.0].iter());
                r#false.1.sort_unstable();
            }
            _ => (),
        }
    }
}

fn insert_expiration<'a>(
    globals: &HashSet<&'a str>,
    expirations: &mut HashMap<&'a str, usize>,
    ident: &'a str,
    i: usize,
) {
    if globals.contains(ident) {
        return;
    }
    expirations.insert(ident, i + 1);
}

fn get_expirations<'a>(
    insts: &[Inst<'a>],
    globals: &HashSet<&'a str>,
) -> HashMap<usize, Vec<&'a str>> {
    let mut expirations: HashMap<&'a str, usize> = HashMap::new();

    for (i, inst) in insts.iter().enumerate() {
        match inst {
            Inst::Jump(label_args) => {
                for arg in &label_args.1 {
                    insert_expiration(globals, &mut expirations, arg, i);
                }
            }
            Inst::Branch(condition, r#true, r#false) => {
                if let Value::Ident(ident) = condition {
                    insert_expiration(globals, &mut expirations, ident, i);
                }
                for arg in &r#true.1 {
                    insert_expiration(globals, &mut expirations, arg, i);
                }
                for arg in &r#false.1 {
                    insert_expiration(globals, &mut expirations, arg, i);
                }
            }
            Inst::Let(ident, value) => {
                if let Value::Ident(value_ident) = value {
                    insert_expiration(globals, &mut expirations, value_ident, i);
                }
                insert_expiration(globals, &mut expirations, ident, i);
            }
            Inst::Call(ident, func, args) => {
                for arg in args {
                    if let Value::Ident(arg_ident) = arg {
                        insert_expiration(globals, &mut expirations, arg_ident, i);
                    }
                }
                insert_expiration(globals, &mut expirations, func, i);
                if let Some(ident) = ident {
                    insert_expiration(globals, &mut expirations, ident, i);
                }
            }
            Inst::Return(Some(Value::Ident(ident))) => {
                insert_expiration(globals, &mut expirations, ident, i);
            }
            Inst::Label(..) | Inst::Return(..) => (),
        }
    }

    let mut inverted = HashMap::new();
    for (key, value) in expirations {
        inverted.entry(value).or_insert_with(Vec::new);
        inverted.get_mut(&value).unwrap().push(key);
    }
    inverted
}

fn get_assignments<'a>(
    insts: &[Inst<'a>],
    expirations: &HashMap<usize, Vec<&'a str>>,
    k: usize,
) -> HashMap<&'a str, usize> {
    let mut registers: Vec<usize> = (0..k).collect();
    let mut assignments: HashMap<&'a str, usize> = HashMap::new();
    let mut expired: HashSet<&'a str> = HashSet::new();
    for (i, inst) in insts.iter().enumerate() {
        if let Some(expiration) = expirations.get(&i) {
            for ident in expiration {
                if let Some(register) = assignments.get(ident) {
                    registers.push(*register);
                }
                expired.insert(ident);
            }
        }
        match inst {
            Inst::Let(ident, _) | Inst::Call(Some(ident), _, _) => {
                assert!(!expired.contains(ident));
                if assignments.contains_key(ident) {
                    continue;
                }
                if let Some(register) = registers.pop() {
                    assignments.insert(ident, register);
                }
            }
            _ => (),
        }
    }
    assignments
}

fn main() {
    let mut insts = vec![
        Inst::Label("f"),
        Inst::Let("x", Value::Int(0)),
        Inst::Let("y", Value::Int(0)),
        Inst::Label("__1__"),
        Inst::Call(Some("__2__"), "<=", vec![Value::Int(10), Value::Ident("x")]),
        Inst::Branch(Value::Ident("__2__"), ("__4__", vec![]), ("__3__", vec![])),
        Inst::Label("__3__"),
        Inst::Call(Some("y"), "+", vec![Value::Ident("x"), Value::Ident("y")]),
        Inst::Call(Some("x"), "+", vec![Value::Ident("x"), Value::Int(1)]),
        Inst::Jump(("__1__", vec![])),
        Inst::Label("__4__"),
        Inst::Call(None, "print_int", vec![Value::Ident("y")]),
        Inst::Return(None),
    ];

    let globals = HashSet::from(["<=", "+", "print_int"]);
    let blocks = get_blocks(&insts);

    let block_args = get_block_args(&blocks, &globals);
    drop(mem::replace(&mut insts, get_insts(blocks)));
    set_block_args(&mut insts, &block_args);

    let expirations = get_expirations(&insts, &globals);
    let assignments = get_assignments(&insts, &expirations, 3);
    for assignment in assignments {
        println!("{assignment:?}");
    }

    for inst in &insts {
        println!("{inst}");
    }
}
