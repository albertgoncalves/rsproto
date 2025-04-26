use std::collections::HashMap;
use std::convert::TryInto;
use std::str;

#[derive(Clone, Debug)]
struct MemberField<'a> {
    parent: &'a str,
    child: &'a str,
    r#type: &'a str,
}

#[derive(Clone, Debug)]
struct MemberMethod<'a> {
    parent: &'a str,
    child: &'a str,
    r#type: &'a str,
    // TODO: Need a function to extract these two fields from the `type` field.
    arg_types: Vec<&'a str>,
    return_type: &'a str,
}

#[derive(Debug)]
enum Code<'a> {
    Line(u16),

    Arg(&'a str, &'a str),
    LocalEnd(&'a str),

    Label(&'a str),

    Inst(Inst<'a>),
}

#[derive(Clone, Debug)]
enum Inst<'a> {
    IConstN(i32),
    IStoreN(&'a str),
    ILoadN(&'a str),
    IfICmpGE(&'a str),
    GetStatic(MemberField<'a>),
    Ldc(&'a str),
    InvokeVirtual(MemberMethod<'a>),
    IInc(&'a str, i32),
    Goto(&'a str),
    Return,
}

impl Inst<'_> {
    const fn size_bytes(&self) -> u16 {
        match self {
            Inst::IConstN(..) | Inst::Return | Inst::IStoreN(..) | Inst::ILoadN(..) => 1,
            Inst::Ldc(..) => 2,
            Inst::IfICmpGE(..)
            | Inst::GetStatic(..)
            | Inst::InvokeVirtual(..)
            | Inst::IInc(..)
            | Inst::Goto(..) => 3,
        }
    }
}

#[derive(Debug)]
struct Line {
    pc: u16,
    line: u16,
}

#[derive(Clone, Debug)]
struct Local<'a> {
    name: &'a str,
    slot: usize,
    pc: u16,
    length: u16,
    signature: &'a str,
}

#[derive(Debug, Default)]
struct Method<'a> {
    labels: HashMap<&'a str, (usize, u16)>,
    insts: Vec<Inst<'a>>,
    args: Vec<(&'a str, &'a str)>,
    locals: Vec<Local<'a>>,
    lines: Vec<Line>,
}

#[derive(Debug)]
struct StackMap<'a> {
    pc: u16,
    stack: Vec<&'a str>,
    locals: Vec<(&'a str, &'a str)>,
}

impl<'a> Method<'a> {
    fn new(codes: &[Code<'a>]) -> Self {
        let mut method = Method::default();
        let mut locals: HashMap<&'a str, (usize, u16, &'a str)> = HashMap::new();

        let mut i = 0;
        let mut pc = 0;
        for code in codes {
            match code {
                Code::Line(line) => method.lines.push(Line { pc, line: *line }),
                Code::Arg(name, signature) => {
                    method.args.push((name, signature));
                    assert!(locals.insert(name, (locals.len(), pc, signature)).is_none());
                }
                Code::LocalEnd(name) => {
                    let local = locals.remove(name).unwrap();
                    let length = pc - local.1;
                    assert!(length != 0);
                    method.locals.push(Local {
                        name,
                        slot: local.0,
                        pc: local.1,
                        length,
                        signature: local.2,
                    });
                }
                Code::Label(label) => {
                    method.labels.insert(label, (i, pc));
                }
                Code::Inst(inst @ Inst::IStoreN(name)) => {
                    method.insts.push(inst.clone());
                    i += 1;
                    pc += inst.size_bytes();
                    assert!(locals.insert(name, (locals.len(), pc, "I")).is_none());
                }
                Code::Inst(inst) => {
                    method.insts.push(inst.clone());
                    i += 1;
                    pc += inst.size_bytes();
                }
            }
        }

        for (name, local) in locals {
            let length = pc - local.1;
            assert!(length != 0);
            method.locals.push(Local {
                name,
                slot: local.0,
                pc: local.1,
                length,
                signature: local.2,
            });
        }

        method.locals.sort_by_key(|local| (local.pc, local.slot));
        method
    }

    fn simulate_stack(&self) -> Vec<StackMap<'a>> {
        let mut snapshots = vec![None; self.insts.len()];
        let mut stack = vec![];
        let mut locals = vec![];

        let mut local_starts: Vec<Local<'a>> = Vec::with_capacity(self.locals.len());
        let mut local_ends: Vec<(u16, &'a str)> = Vec::with_capacity(self.locals.len());

        for local in self.locals.iter().rev() {
            local_starts.push(local.clone());
        }

        let mut indices = vec![(0, 0)];
        while let Some((i, pc)) = indices.pop() {
            if let Some(other) = &snapshots[i] {
                assert_eq!((pc, stack.clone(), locals.clone()), *other, "{i}");
                continue;
            }

            while let Some(local) = local_starts.pop_if(|local| local.pc == pc) {
                assert!(locals.len() == local.slot);
                locals.push((local.name, local.signature));
                local_ends.push((local.pc + local.length, local.name));
            }

            while let Some(local) = local_ends.pop_if(|local| local.0 == pc) {
                assert!(locals.pop().unwrap().0 == local.1);
            }

            snapshots[i] = Some((pc, stack.clone(), locals.clone()));

            match &self.insts[i] {
                inst @ (Inst::IConstN(..) | Inst::ILoadN(..)) => {
                    stack.push("I");
                    indices.push((i + 1, pc + inst.size_bytes()));
                }
                inst @ Inst::IStoreN(..) => {
                    assert_eq!(stack.pop(), Some("I"));
                    indices.push((i + 1, pc + inst.size_bytes()));
                }
                inst @ Inst::IfICmpGE(label) => {
                    assert_eq!(stack.pop(), Some("I"));
                    assert_eq!(stack.pop(), Some("I"));
                    indices.push(self.labels[label]);
                    indices.push((i + 1, pc + inst.size_bytes()));
                }
                inst @ Inst::GetStatic(field) => {
                    stack.push(field.r#type);
                    indices.push((i + 1, pc + inst.size_bytes()));
                }
                inst @ Inst::Ldc(..) => {
                    stack.push("Ljava/lang/String;");
                    indices.push((i + 1, pc + inst.size_bytes()));
                }
                inst @ Inst::InvokeVirtual(member) => {
                    let mut arg_types = member.arg_types.clone();
                    while let Some(arg_type) = arg_types.pop() {
                        assert_eq!(arg_type, stack.pop().unwrap());
                    }
                    let stack_top = stack.pop().unwrap();
                    let bytes = stack_top.as_bytes();
                    assert!(bytes[0] == b'L', "{}", stack_top);
                    assert!(bytes[bytes.len() - 1] == b';', "{}", stack_top);
                    assert_eq!(
                        unsafe { str::from_utf8_unchecked(&bytes[1..bytes.len() - 1]) },
                        member.parent,
                    );
                    if member.return_type != "V" {
                        stack.push(member.return_type);
                    }
                    indices.push((i + 1, pc + inst.size_bytes()));
                }
                inst @ Inst::IInc(..) => {
                    indices.push((i + 1, pc + inst.size_bytes()));
                }
                Inst::Goto(label) => {
                    indices.push(self.labels[label]);
                }
                Inst::Return => {
                    stack.clear();
                }
            }
        }

        let mut stack_maps = Vec::with_capacity(self.insts.len());
        for snapshot in snapshots {
            let (pc, stack, locals) = snapshot.unwrap();
            stack_maps.push(StackMap { pc, stack, locals });
        }
        stack_maps
    }

    fn get_constant_pool(&self) -> ConstantPool<'a> {
        let mut constant_pool = ConstantPool::new();
        for inst in &self.insts {
            match inst {
                Inst::IConstN(..) | Inst::IfICmpGE(..) | Inst::Goto(..) | Inst::Return => (),
                Inst::IStoreN(name) => {
                    constant_pool.insert_utf8(name);
                }
                Inst::ILoadN(name) | Inst::IInc(name, _) => {
                    assert!(constant_pool.keys.contains_key(&ConstantKey::Utf8(name)));
                }
                Inst::GetStatic(field) => {
                    constant_pool.insert_field(field.parent, field.child, field.r#type);
                }
                Inst::Ldc(string) => {
                    constant_pool.insert_string(string);
                }
                Inst::InvokeVirtual(method) => {
                    constant_pool.insert_method(method.parent, method.child, method.r#type);
                }
            }
        }
        constant_pool
    }
}

#[derive(Debug)]
enum ConstantValue<'a> {
    Utf8(&'a str),
    String(u16),
    Class(u16),
    NameAndType(u16, u16),
    Field(u16, u16),
    Method(u16, u16),
}

#[derive(Debug, Eq, Hash, PartialEq)]
enum ConstantKey<'a> {
    Utf8(&'a str),
    String(&'a str),
    Class(&'a str),
    NameAndType(&'a str, &'a str),
    Field(&'a str, &'a str, &'a str),
    Method(&'a str, &'a str, &'a str),
}

#[derive(Debug)]
struct ConstantPool<'a> {
    keys: HashMap<ConstantKey<'a>, u16>,
    values: Vec<ConstantValue<'a>>,
}

impl<'a> ConstantPool<'a> {
    fn new() -> Self {
        Self {
            keys: HashMap::new(),
            values: Vec::new(),
        }
    }

    fn insert_utf8(&mut self, utf8: &'a str) -> u16 {
        let key = ConstantKey::Utf8(utf8);
        let cached = self.keys.get(&key);
        match cached {
            Some(index) => *index,
            None => {
                let index = self.values.len().try_into().unwrap();
                self.values.push(ConstantValue::Utf8(utf8));
                assert!(self.keys.insert(key, index).is_none());
                index
            }
        }
    }

    fn insert_string(&mut self, string: &'a str) -> u16 {
        let key = ConstantKey::String(string);
        let cached = self.keys.get(&key);
        match cached {
            Some(index) => *index,
            None => {
                let child_index = self.insert_utf8(string);
                let parent_index = self.values.len().try_into().unwrap();
                self.values.push(ConstantValue::String(child_index));
                assert!(self.keys.insert(key, parent_index).is_none());
                parent_index
            }
        }
    }

    fn insert_class(&mut self, class: &'a str) -> u16 {
        let key = ConstantKey::Class(class);
        let cached = self.keys.get(&key);
        match cached {
            Some(index) => *index,
            None => {
                let child_index = self.insert_utf8(class);
                let parent_index = self.values.len().try_into().unwrap();
                self.values.push(ConstantValue::Class(child_index));
                assert!(self.keys.insert(key, parent_index).is_none());
                parent_index
            }
        }
    }

    fn insert_name_and_type(&mut self, name: &'a str, r#type: &'a str) -> u16 {
        let key = ConstantKey::NameAndType(name, r#type);
        let cached = self.keys.get(&key);
        match cached {
            Some(index) => *index,
            None => {
                let name_index = self.insert_utf8(name);
                let type_index = self.insert_utf8(r#type);
                let parent_index = self.values.len().try_into().unwrap();
                self.values
                    .push(ConstantValue::NameAndType(name_index, type_index));
                assert!(self.keys.insert(key, parent_index).is_none());
                parent_index
            }
        }
    }

    fn insert_field(&mut self, parent: &'a str, child: &'a str, r#type: &'a str) -> u16 {
        let key = ConstantKey::Field(parent, child, r#type);
        let cached = self.keys.get(&key);
        match cached {
            Some(index) => *index,
            None => {
                let class_index = self.insert_class(parent);
                let name_and_type_index = self.insert_name_and_type(child, r#type);
                let parent_index = self.values.len().try_into().unwrap();
                self.values
                    .push(ConstantValue::Field(class_index, name_and_type_index));
                assert!(self.keys.insert(key, parent_index).is_none());
                parent_index
            }
        }
    }

    fn insert_method(&mut self, parent: &'a str, child: &'a str, r#type: &'a str) -> u16 {
        let key = ConstantKey::Method(parent, child, r#type);
        let cached = self.keys.get(&key);
        match cached {
            Some(index) => *index,
            None => {
                let class_index = self.insert_class(parent);
                let name_and_type_index = self.insert_name_and_type(child, r#type);
                let parent_index = self.values.len().try_into().unwrap();
                self.values
                    .push(ConstantValue::Method(class_index, name_and_type_index));
                assert!(self.keys.insert(key, parent_index).is_none());
                parent_index
            }
        }
    }
}

fn main() {
    let codes = [
        Code::Arg("args", "[Ljava/lang/String;"),
        Code::Line(3),
        Code::Inst(Inst::IConstN(0)),
        Code::Inst(Inst::IStoreN("i")),
        Code::Label("loop"),
        Code::Inst(Inst::ILoadN("i")),
        Code::Inst(Inst::IConstN(3)),
        Code::Inst(Inst::IfICmpGE("break")),
        Code::Line(4),
        Code::Inst(Inst::GetStatic(MemberField {
            parent: "java/lang/System",
            child: "out",
            r#type: "Ljava/io/PrintStream;",
        })),
        Code::Inst(Inst::Ldc("Hello, world!")),
        Code::Inst(Inst::InvokeVirtual(MemberMethod {
            parent: "java/io/PrintStream",
            child: "println",
            r#type: "(Ljava/lang/String;)V",
            arg_types: vec!["Ljava/lang/String;"],
            return_type: "V",
        })),
        Code::Line(3),
        Code::Inst(Inst::IInc("i", 1)),
        Code::Inst(Inst::Goto("loop")),
        Code::LocalEnd("i"),
        Code::Line(6),
        Code::Label("break"),
        Code::Inst(Inst::Return),
    ];

    let method = Method::new(&codes);
    println!("{method:#?}");

    let stack_maps = method.simulate_stack();
    for stack_map in stack_maps {
        println!("{stack_map:?}");
    }

    let constant_pool = method.get_constant_pool();
    for (i, constant_value) in constant_pool.values.iter().enumerate() {
        println!("{i}: {constant_value:?}");
    }
}
