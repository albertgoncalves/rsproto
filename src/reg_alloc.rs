use std::collections::{HashMap, HashSet};
use std::fmt;

type Var<'a> = (&'a str, u32);

type VarRange<'a> = (&'a str, u32, u32);

#[derive(Clone)]
enum Inst<'a> {
    Label(&'a str),
    PushInt(i64),
    PushLabel(&'a str, Vec<Var<'a>>),
    Load(Var<'a>),
    Store(Var<'a>),
    Greater,
    Add,
    Branch,
    Jump,
    Call,
    Return,
}

struct Block<'a> {
    label: &'a str,
    args: Vec<Var<'a>>,
    insts: Vec<Inst<'a>>,
}

fn write_var(f: &mut fmt::Formatter<'_>, var: Var<'_>) -> fmt::Result {
    write!(f, "{}.{}", var.0, var.1)
}

fn write_vars(f: &mut fmt::Formatter<'_>, vars: &[Var<'_>]) -> fmt::Result {
    let mut vars = vars.iter();
    let var = vars.next().unwrap();
    write_var(f, *var)?;
    for var in vars {
        write!(f, ", ")?;
        write_var(f, *var)?;
    }
    Ok(())
}

impl fmt::Display for Inst<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    ")?;
        match self {
            Self::Label(label) => writeln!(f, "{label}:"),
            Self::PushInt(int) => writeln!(f, "    push-int {int}"),
            Self::PushLabel(label, args) => {
                if args.is_empty() {
                    writeln!(f, "    push-label {label}")
                } else {
                    write!(f, "    push-label {label}(")?;
                    write_vars(f, args)?;
                    writeln!(f, ")")
                }
            }
            Self::Load(var) => {
                write!(f, "    load ")?;
                write_var(f, *var)?;
                writeln!(f)
            }
            Self::Store(var) => {
                write!(f, "    store ")?;
                write_var(f, *var)?;
                writeln!(f)
            }
            Self::Greater => writeln!(f, "    >"),
            Self::Add => writeln!(f, "    +"),
            Self::Call => writeln!(f, "    call"),
            Self::Jump => writeln!(f, "    jump"),
            Self::Branch => writeln!(f, "    branch"),
            Self::Return => writeln!(f, "    return"),
        }
    }
}

impl fmt::Display for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.args.is_empty() {
            writeln!(f, "    {}:", self.label)?;
        } else {
            write!(f, "    {}(", self.label)?;
            write_vars(f, &self.args)?;
            writeln!(f, "):")?;
        }
        for inst in &self.insts {
            write!(f, "{inst}")?;
        }
        Ok(())
    }
}

fn get_blocks<'a>(insts: &[Inst<'a>]) -> Vec<Block<'a>> {
    let mut blocks: Vec<Block<'a>> = vec![];
    for (i, inst) in insts.iter().enumerate() {
        if let Inst::Label(label) = inst {
            if i != 0 && !matches!(insts[i - 1], Inst::Branch | Inst::Jump | Inst::Return) {
                let j = blocks.len() - 1;
                blocks[j].insts.push(Inst::PushLabel(label, vec![]));
                blocks[j].insts.push(Inst::Jump);
            }
            blocks.push(Block {
                label,
                args: vec![],
                insts: vec![],
            });
        } else {
            let j = blocks.len() - 1;
            blocks[j].insts.push(inst.clone());
        }
    }
    blocks
}

fn get_partial_label_args<'a>(blocks: &[Block<'a>]) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut label_args: HashMap<&'a str, HashSet<&'a str>> = HashMap::new();

    for block in blocks {
        label_args.insert(block.label, HashSet::new());
    }

    let mut repeat = true;
    while repeat {
        repeat = false;
        for block in blocks {
            let mut locals: HashSet<&'a str> = HashSet::new();
            let mut orphans: HashSet<&'a str> = HashSet::new();

            for inst in &block.insts {
                match inst {
                    Inst::Store((var, _)) => {
                        if orphans.contains(var) {
                            continue;
                        }
                        let _ = locals.insert(var);
                    }
                    Inst::Load((var, _)) if !locals.contains(var) => {
                        let _ = orphans.insert(var);
                    }
                    Inst::PushLabel(label, _) => {
                        let Some(args) = label_args.get(label) else {
                            continue;
                        };
                        for arg in args {
                            if locals.contains(arg) {
                                continue;
                            }
                            orphans.insert(arg);
                        }
                    }
                    _ => (),
                }
            }

            let args: &mut HashSet<&'a str> = label_args.get_mut(block.label).unwrap();
            for var in orphans {
                repeat |= args.insert(var);
            }
        }
    }

    label_args
}

fn increment_k<'a>(blocks: &mut [Block<'a>], label_args: &HashMap<&'a str, HashSet<&'a str>>) {
    for block in &mut *blocks {
        for arg in &label_args[block.label] {
            block.args.push((arg, 0));
        }
        block.args.sort_unstable();
        for inst in &mut block.insts {
            let Inst::PushLabel(label, vec) = inst else {
                continue;
            };
            assert!(vec.is_empty());
            let Some(args) = label_args.get(label) else {
                continue;
            };
            for arg in args {
                vec.push((arg, 0));
            }
            vec.sort_unstable();
        }
    }

    let mut var_numbers: HashMap<&'a str, u32> = HashMap::new();
    for block in blocks {
        for arg in &mut block.args {
            let k = var_numbers.get(arg.0).map_or(1, |k| k + 1);
            arg.1 = k;
            var_numbers.insert(arg.0, k);
        }
        for inst in &mut block.insts {
            match inst {
                Inst::Load((var, k)) => {
                    assert!(*k == 0);
                    *k = *var_numbers.get(var).unwrap();
                }
                Inst::Store((var, k0)) => {
                    assert!(*k0 == 0);
                    let k1 = var_numbers.get(var).map_or(1, |k1| k1 + 1);
                    var_numbers.insert(var, k1);
                    *k0 = k1;
                }
                Inst::PushLabel(_, args) => {
                    for arg in args {
                        assert!(arg.1 == 0);
                        arg.1 = var_numbers[arg.0];
                    }
                }
                _ => (),
            }
        }
    }
}

fn get_label_args<'a>(blocks: &[Block<'a>]) -> HashMap<&'a str, Vec<Var<'a>>> {
    let mut label_args: HashMap<&'a str, Vec<Var<'a>>> = HashMap::new();
    for block in blocks {
        label_args.insert(block.label, block.args.clone());
    }
    label_args
}

fn push_pairs<'a>(pairs: &mut Vec<VarRange<'a>>, args_from: &[Var<'a>], args_to: &[Var<'a>]) {
    for arg_from in args_from {
        for arg_to in args_to {
            if arg_from.0 != arg_to.0 {
                continue;
            }
            assert!(arg_from.1 != arg_to.1);
            if arg_from.1 < arg_to.1 {
                pairs.push((arg_from.0, arg_from.1, arg_to.1));
            } else {
                pairs.push((arg_from.0, arg_to.1, arg_from.1));
            }
            break;
        }
    }
}

fn get_pairs<'a>(
    blocks: &mut [Block<'a>],
    label_args: &HashMap<&'a str, Vec<Var<'a>>>,
) -> Vec<VarRange<'a>> {
    let mut pairs: Vec<VarRange<'a>> = vec![];
    for block in blocks {
        let i = block.insts.len() - 1;
        match block.insts.get(i) {
            Some(Inst::Jump) => {
                let Some(Inst::PushLabel(label, args_from)) = block.insts.get(i - 1) else {
                    unreachable!()
                };
                push_pairs(&mut pairs, args_from, &label_args[label]);
            }
            Some(Inst::Branch) => {
                for j in (i - 2)..i {
                    let Some(Inst::PushLabel(label, args_from)) = block.insts.get(j) else {
                        unreachable!()
                    };
                    push_pairs(&mut pairs, args_from, &label_args[label]);
                }
            }
            None => unreachable!(),
            _ => (),
        }
    }
    pairs
}

fn get_groups<'a>(pairs: &[VarRange<'a>]) -> Vec<VarRange<'a>> {
    let mut groups: Vec<VarRange<'a>> = vec![];
    for pair in pairs {
        assert!(pair.1 <= pair.2);
        let mut found = false;
        for group in &mut groups {
            assert!(group.1 < group.2);
            if pair.0 != group.0 {
                continue;
            }
            if !((group.1 <= pair.2) && (pair.1 <= group.2)) {
                continue;
            }
            group.1 = std::cmp::min(group.1, pair.1);
            group.2 = std::cmp::max(group.2, pair.2);
            found = true;
            break;
        }
        if !found {
            groups.push(*pair);
        }
    }
    groups
}

fn update_k<'a>(groups: &[VarRange<'a>], arg: &mut Var<'a>) {
    for group in groups {
        if arg.0 != group.0 {
            continue;
        }
        let k = arg.1;
        if !((group.1 <= k) && (k <= group.2)) {
            continue;
        }
        arg.1 = group.1;
        break;
    }
}

fn shrink_k<'a>(blocks: &mut [Block<'a>], groups: &[VarRange<'a>]) {
    for block in blocks {
        for arg in &mut block.args {
            update_k(groups, arg);
        }
        for inst in &mut block.insts {
            match inst {
                Inst::Load(arg) | Inst::Store(arg) => {
                    update_k(groups, arg);
                }
                Inst::PushLabel(_, args) => {
                    for arg in args {
                        update_k(groups, arg);
                    }
                }
                _ => (),
            }
        }
    }
}

fn main() {
    let insts = [
        Inst::Label("f"),
        Inst::PushInt(0),
        Inst::Store(("x", 0)),
        Inst::PushInt(0),
        Inst::Store(("y", 0)),
        Inst::Label("__0__"),
        Inst::Load(("x", 0)),
        Inst::PushInt(10),
        Inst::Greater,
        Inst::PushLabel("__1__", vec![]),
        Inst::PushLabel("__2__", vec![]),
        Inst::Branch,
        Inst::Label("__2__"),
        Inst::Load(("x", 0)),
        Inst::Load(("y", 0)),
        Inst::Add,
        Inst::Store(("y", 0)),
        Inst::Load(("x", 0)),
        Inst::PushInt(1),
        Inst::Add,
        Inst::Store(("x", 0)),
        Inst::PushLabel("__0__", vec![]),
        Inst::Jump,
        Inst::Label("__1__"),
        Inst::Load(("y", 0)),
        Inst::PushLabel("print", vec![]),
        Inst::Call,
        Inst::Return,
    ];

    let mut blocks = get_blocks(&insts);

    let partial_label_args = get_partial_label_args(&blocks[..]);
    increment_k(&mut blocks, &partial_label_args);

    let label_args = get_label_args(&blocks[..]);
    let pairs = get_pairs(&mut blocks, &label_args);
    let groups = get_groups(&pairs);
    shrink_k(&mut blocks, &groups);

    println!();
    for block in blocks {
        println!("{block}");
    }
}
