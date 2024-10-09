// NOTE: See `https://gist.github.com/kseo/9383472`.

use std::collections::{HashMap, HashSet};
use std::fmt;

enum Term<'a> {
    Int(i64),
    Bool(bool),
    Ident(&'a str),
    Access(Box<Term<'a>>, &'a str),
    Lambda(Vec<&'a str>, Box<Term<'a>>),
    Apply(Vec<Term<'a>>),
    Let(&'a str, Box<(Term<'a>, Term<'a>)>),
    LetRecs(Vec<(&'a str, Term<'a>)>, Box<Term<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
enum Type<'a> {
    Var(u32),
    Op(&'a str, Vec<Type<'a>>),
    Dict(HashMap<&'a str, Type<'a>>, Option<u32>),
}

#[derive(Debug, PartialEq)]
enum Error {
    Arity,
    Infinite,
    Key,
    Mismatch,
    Undefined,
}

impl fmt::Display for Term<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Bool(r#bool) => write!(f, "{bool}"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Access(term, ident) => write!(f, "{term}.{ident}"),
            Self::Lambda(args, term) => {
                write!(f, "\\{}", args[0])?;
                for arg in &args[1..] {
                    write!(f, ", {arg}")?;
                }
                write!(f, " -> {term}")
            }
            Self::Apply(func_args) if func_args.is_empty() => unreachable!(),
            Self::Apply(func_args) if func_args.len() == 1 => {
                write!(f, "{}()", func_args[0])
            }
            Self::Apply(func_args) => {
                write!(f, "{}({}", func_args[0], func_args[1])?;
                for arg in &func_args[2..] {
                    write!(f, ", {arg}")?;
                }
                write!(f, ")")
            }
            Self::Let(ident, value_body) => {
                let (value, body) = value_body.as_ref();
                write!(f, "let {ident} = {value} in {body}")
            }
            Self::LetRecs(bindings, body) => {
                assert!(!bindings.is_empty());
                write!(f, "let {} = {}", bindings[0].0, bindings[0].1)?;
                for binding in &bindings[1..] {
                    write!(f, " and {} = {}", binding.0, binding.1)?;
                }
                write!(f, " in {body}")
            }
        }
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(k) => write!(f, "__{k}__"),
            Self::Op(op, types) if types.is_empty() => write!(f, "{op}"),
            Self::Op("fn", types) if types.is_empty() => unreachable!(),
            Self::Op("fn", types) if types.len() == 1 => write!(f, "(() => {})", types[0]),
            Self::Op("fn", types) => {
                let n = types.len();
                assert!(1 < n);

                write!(f, "(({}", types[0])?;
                for r#type in &types[1..(n - 1)] {
                    write!(f, ", {type}")?;
                }
                write!(f, ") -> {})", types[n - 1])
            }
            Self::Op("tuple", types) => {
                assert!(types.len() == 2);
                write!(f, "({}, {})", types[0], types[1])
            }
            Self::Op(..) => todo!(),
            Self::Dict(dict, _) if dict.is_empty() => write!(f, "{{}}"),
            Self::Dict(dict, _) => {
                let rows: Vec<_> = dict.iter().collect();
                write!(f, "{{{}: {}", rows[0].0, rows[0].1)?;
                for (ident, r#type) in &rows[1..] {
                    write!(f, ", {ident}: {type}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Default)]
struct State<'a> {
    k: u32,
    links: HashMap<u32, Type<'a>>,
}

#[derive(Default)]
struct Context<'a> {
    state: State<'a>,
    generics: HashMap<u32, u32>,
    non_generics: HashSet<u32>,
    env: Vec<(&'a str, Type<'a>)>,
}

impl<'a> State<'a> {
    fn next_var(&mut self) -> (u32, Type<'a>) {
        let k = self.k;
        self.k += 1;
        (k, Type::Var(k))
    }

    fn prune(&mut self, old_type: Type<'a>) -> Type<'a> {
        match old_type {
            Type::Var(k) => {
                let Some(new_type) = self.links.remove(&k) else {
                    return old_type;
                };
                let new_type = self.prune(new_type.clone());
                self.links.insert(k, new_type.clone());
                new_type
            }
            Type::Op(op, old_types) => Type::Op(
                op,
                old_types
                    .into_iter()
                    .map(|old_type| self.prune(old_type))
                    .collect(),
            ),
            Type::Dict(old_dict, Some(k)) => {
                let mut new_dict = match self.prune(Type::Var(k)) {
                    Type::Dict(new_dict, _) => new_dict,
                    Type::Var(..) => HashMap::new(),
                    Type::Op(..) => unreachable!(),
                };
                for (ident, old_type) in old_dict {
                    assert!(new_dict.insert(ident, self.prune(old_type)).is_none());
                }
                Type::Dict(new_dict, Some(k))
            }
            Type::Dict(old_dict, None) => Type::Dict(
                old_dict
                    .into_iter()
                    .map(|(ident, old_type)| (ident, self.prune(old_type)))
                    .collect(),
                None,
            ),
        }
    }

    fn occurs(&mut self, r#type: Type<'a>, other: u32) -> bool {
        match self.prune(r#type) {
            Type::Var(k) if k == other => true,
            Type::Var(..) => false,
            Type::Op(_, types) => {
                for r#type in types {
                    if self.occurs(r#type, other) {
                        return true;
                    }
                }
                false
            }
            Type::Dict(dict, k) => {
                if let Some(k) = k {
                    if self.occurs(Type::Var(k), other) {
                        return true;
                    }
                }
                for (_, r#type) in dict {
                    if self.occurs(r#type, other) {
                        return true;
                    }
                }
                false
            }
        }
    }
}

impl<'a> Context<'a> {
    fn is_generic(&mut self, other: u32) -> bool {
        for non_generic in &self.non_generics {
            if self.state.occurs(Type::Var(*non_generic), other) {
                return false;
            }
        }
        true
    }

    fn fresh(&mut self, old_type: Type<'a>) -> Type<'a> {
        let old_type = self.state.prune(old_type);
        match old_type {
            Type::Var(old_k) => {
                if !self.is_generic(old_k) {
                    return old_type;
                }
                if let Some(new_k) = self.generics.get(&old_k) {
                    return Type::Var(*new_k);
                }
                let (new_k, new_type) = self.state.next_var();
                self.generics.insert(old_k, new_k);
                new_type
            }
            Type::Op(op, types) => Type::Op(
                op,
                types
                    .into_iter()
                    .map(|old_type| self.fresh(old_type))
                    .collect(),
            ),
            Type::Dict(dict, k) => Type::Dict(
                dict.into_iter()
                    .map(|(ident, old_type)| (ident, self.fresh(old_type)))
                    .collect(),
                k,
            ),
        }
    }

    fn type_to_dict(&mut self, key: &'a str, r#type: Type<'a>) -> Type<'a> {
        let k = self.state.next_var().0;
        Type::Dict([(key, r#type)].into(), Some(k))
    }

    fn unify(&mut self, left_type: Type<'a>, right_type: Type<'a>) -> Result<(), Error> {
        let left_type = self.state.prune(left_type);
        let right_type = self.state.prune(right_type);
        match (left_type, right_type) {
            (Type::Var(left_k), Type::Var(right_k)) if left_k == right_k => Ok(()),
            (Type::Var(k), right_type) | (right_type, Type::Var(k)) => {
                if self.state.occurs(right_type.clone(), k) {
                    return Err(Error::Infinite);
                }
                self.state.links.insert(k, right_type);
                Ok(())
            }
            (Type::Op(..), Type::Dict(..)) | (Type::Dict(..), Type::Op(..)) => Err(Error::Mismatch),
            (Type::Op(left_op, _), Type::Op(right_op, _)) if left_op != right_op => {
                Err(Error::Mismatch)
            }
            (Type::Op(_, left_types), Type::Op(_, right_types))
                if left_types.len() != right_types.len() =>
            {
                Err(Error::Arity)
            }
            (Type::Op(_, left_types), Type::Op(_, right_types)) => {
                for (left_type, right_type) in left_types.into_iter().zip(right_types) {
                    self.unify(left_type, right_type)?;
                }
                Ok(())
            }
            (Type::Dict(mut left_dict, left_k), Type::Dict(mut right_dict, right_k)) => {
                let left_keys: HashSet<&'a str> = left_dict.keys().copied().collect();
                let right_keys: HashSet<&'a str> = right_dict.keys().copied().collect();
                for key in left_keys.union(&right_keys) {
                    let left_type = left_dict.remove(key);
                    let right_type = right_dict.remove(key);
                    match (left_type, right_type) {
                        (Some(left_type), None) => {
                            let Some(right_k) = right_k else {
                                return Err(Error::Key);
                            };
                            let dict = self.type_to_dict(key, left_type);
                            self.unify(Type::Var(right_k), dict)?;
                        }
                        (None, Some(right_type)) => {
                            let Some(left_k) = left_k else {
                                return Err(Error::Key);
                            };
                            let dict = self.type_to_dict(key, right_type);
                            self.unify(Type::Var(left_k), dict)?;
                        }
                        (Some(left_type), Some(right_type)) => {
                            self.unify(left_type, right_type)?;
                        }
                        (None, None) => unreachable!(),
                    }
                }
                Ok(())
            }
        }
    }

    fn infer(&mut self, term: &Term<'a>) -> Result<Type<'a>, Error> {
        match term {
            Term::Int(..) => Ok(Type::Op("int", vec![])),
            Term::Bool(..) => Ok(Type::Op("bool", vec![])),
            Term::Ident(ident) => {
                let Some(i) = self.env.iter().rposition(|(other, _)| ident == other) else {
                    return Err(Error::Undefined);
                };
                self.generics.clear();
                Ok(self.fresh(self.env[i].1.clone()))
            }
            Term::Access(term, ident) => {
                let term_type = self.infer(term)?;
                let ident_type = self.state.next_var().1;

                let dict = self.type_to_dict(ident, ident_type.clone());
                self.unify(term_type, dict)?;

                Ok(ident_type)
            }
            Term::Apply(func_args) if func_args.is_empty() => unreachable!(),
            Term::Apply(func_args) if func_args.len() == 1 => {
                let func_type = self.infer(&func_args[0])?;
                let ret_type = self.state.next_var().1;

                self.unify(Type::Op("fn", vec![ret_type.clone()]), func_type)?;

                Ok(self.state.prune(ret_type))
            }
            Term::Apply(func_args) => {
                let func_type = self.infer(&func_args[0])?;
                let mut op_types = Vec::with_capacity(func_args.len());

                for arg in &func_args[1..] {
                    op_types.push(self.infer(arg)?);
                }
                let ret_type = self.state.next_var().1;
                op_types.push(ret_type.clone());

                self.unify(Type::Op("fn", op_types), func_type)?;

                Ok(self.state.prune(ret_type))
            }
            Term::Lambda(args, term) => {
                let mut arg_vars = Vec::with_capacity(args.len());
                let mut func_types = Vec::with_capacity(args.len() + 1);
                for arg in args {
                    let (arg_k, arg_type) = self.state.next_var();
                    self.non_generics.insert(arg_k);
                    self.env.push((arg, arg_type.clone()));
                    arg_vars.push((arg, arg_k));
                    func_types.push(arg_type);
                }

                func_types.push(self.infer(term)?);
                let func_type = Type::Op("fn", func_types);

                for (arg, arg_k) in arg_vars.into_iter().rev() {
                    assert!(*arg == self.env.pop().unwrap().0);
                    assert!(self.non_generics.remove(&arg_k));
                }

                Ok(self.state.prune(func_type))
            }
            Term::Let(ident, value_body) => {
                let (value, body) = value_body.as_ref();

                let value_type = self.infer(value)?;
                self.env.push((ident, value_type));
                let body_type = self.infer(body)?;

                assert!(*ident == self.env.pop().unwrap().0);

                Ok(self.state.prune(body_type))
            }
            Term::LetRecs(bindings, body) => {
                let mut k_removes = Vec::with_capacity(bindings.len());
                let mut values = Vec::with_capacity(bindings.len());

                for (ident, value) in bindings {
                    let (k, r#type) = self.state.next_var();
                    k_removes.push(k);
                    self.non_generics.insert(k);
                    self.env.push((ident, r#type.clone()));
                    values.push((value, r#type));
                }
                for (value, r#type) in values {
                    let value_type = self.infer(value)?;
                    self.unify(value_type, r#type)?;
                }
                for k in k_removes {
                    assert!(self.non_generics.remove(&k));
                }

                let body_type = self.infer(body)?;

                for (ident, _) in bindings.iter().rev() {
                    assert!(*ident == self.env.pop().unwrap().0);
                }

                Ok(self.state.prune(body_type))
            }
        }
    }
}

fn main() {
    let mut context = Context::default();
    let a = context.state.next_var().1;
    let b = context.state.next_var().1;
    context.env.push((
        "pair",
        Type::Op(
            "fn",
            vec![a.clone(), b.clone(), Type::Op("tuple", vec![a, b])],
        ),
    ));

    let term = Term::Lambda(
        vec!["x"],
        Box::new(Term::Apply(vec![
            Term::Ident("pair"),
            Term::Access(Box::new(Term::Ident("x")), "y"),
            Term::Access(Box::new(Term::Ident("x")), "z"),
        ])),
    );

    print!("{term}");

    let r#type = context.infer(&term).unwrap();
    println!(" :: {type}\n");

    for (k, r#type) in &context.state.links {
        println!("{k:>4}: {type}");
    }
    println!();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn infer_ok_0() {
        let mut context = Context::default();

        let a = context.state.next_var().1;
        let b = context.state.next_var().1;
        context.env.push((
            "pair",
            Type::Op(
                "fn",
                vec![
                    a.clone(),
                    Type::Op("fn", vec![b.clone(), Type::Op("tuple", vec![a, b])]),
                ],
            ),
        ));

        let term = Term::Let(
            "f",
            Box::new((
                Term::Lambda(vec!["x"], Box::new(Term::Ident("x"))),
                Term::Apply(vec![
                    Term::Apply(vec![
                        Term::Ident("pair"),
                        Term::Apply(vec![Term::Ident("f"), Term::Int(-123)]),
                    ]),
                    Term::Apply(vec![Term::Ident("f"), Term::Bool(true)]),
                ]),
            )),
        );

        let expected = Ok(Type::Op(
            "tuple",
            vec![Type::Op("int", vec![]), Type::Op("bool", vec![])],
        ));

        assert!(context.infer(&term) == expected);
    }

    #[test]
    fn infer_ok_1() {
        let mut context = Context::default();

        let term = Term::Let(
            "g",
            Box::new((
                Term::Lambda(vec!["f"], Box::new(Term::Int(-1))),
                Term::Apply(vec![Term::Ident("g"), Term::Ident("g")]),
            )),
        );

        assert!(context.infer(&term) == Ok(Type::Op("int", vec![])));
    }

    #[test]
    fn infer_ok_2() {
        let mut context = Context::default();

        let term = Term::Lambda(
            vec!["f"],
            Box::new(Term::Lambda(
                vec!["g"],
                Box::new(Term::Lambda(
                    vec!["x"],
                    Box::new(Term::Apply(vec![
                        Term::Ident("g"),
                        Term::Apply(vec![Term::Ident("f"), Term::Ident("x")]),
                    ])),
                )),
            )),
        );

        let expected = Ok(Type::Op(
            "fn",
            vec![
                Type::Op("fn", vec![Type::Var(2), Type::Var(3)]),
                Type::Op(
                    "fn",
                    vec![
                        Type::Op("fn", vec![Type::Var(3), Type::Var(4)]),
                        Type::Op("fn", vec![Type::Var(2), Type::Var(4)]),
                    ],
                ),
            ],
        ));

        assert!(context.infer(&term) == expected);
    }

    #[test]
    fn infer_ok_3() {
        let mut context = Context::default();
        let a = context.state.next_var().1;
        let b = context.state.next_var().1;
        context.env.push((
            "pair",
            Type::Op(
                "fn",
                vec![
                    a.clone(),
                    Type::Op("fn", vec![b.clone(), Type::Op("tuple", vec![a, b])]),
                ],
            ),
        ));

        let term = Term::LetRecs(
            vec![
                (
                    "f",
                    Term::Lambda(
                        vec!["x"],
                        Box::new(Term::Apply(vec![Term::Ident("g"), Term::Ident("x")])),
                    ),
                ),
                (
                    "g",
                    Term::Lambda(
                        vec!["x"],
                        Box::new(Term::Apply(vec![Term::Ident("f"), Term::Ident("x")])),
                    ),
                ),
            ],
            Box::new(Term::Apply(vec![
                Term::Apply(vec![
                    Term::Ident("pair"),
                    Term::Apply(vec![Term::Ident("g"), Term::Bool(true)]),
                ]),
                Term::Apply(vec![Term::Ident("f"), Term::Int(-1)]),
            ])),
        );

        let expected = Ok(Type::Op("tuple", vec![Type::Var(8), Type::Var(9)]));

        assert!(context.infer(&term) == expected);
    }

    #[test]
    fn infer_ok_4() {
        let mut context = Context::default();
        let a = context.state.next_var().1;
        let b = context.state.next_var().1;
        context.env.push((
            "pair",
            Type::Op(
                "fn",
                vec![
                    a.clone(),
                    Type::Op("fn", vec![b.clone(), Type::Op("tuple", vec![a, b])]),
                ],
            ),
        ));

        let term = Term::LetRecs(
            vec![
                (
                    "f",
                    Term::Lambda(
                        vec!["x"],
                        Box::new(Term::Apply(vec![Term::Ident("g"), Term::Ident("x")])),
                    ),
                ),
                (
                    "g",
                    Term::Lambda(
                        vec!["x"],
                        Box::new(Term::Apply(vec![Term::Ident("f"), Term::Ident("x")])),
                    ),
                ),
            ],
            Box::new(Term::Let(
                "x",
                Box::new((
                    Term::Lambda(
                        vec!["y"],
                        Box::new(Term::Apply(vec![Term::Ident("f"), Term::Int(0)])),
                    ),
                    Term::Apply(vec![
                        Term::Apply(vec![Term::Ident("pair"), Term::Ident("g")]),
                        Term::Ident("x"),
                    ]),
                )),
            )),
        );

        let expected = Ok(Type::Op(
            "tuple",
            vec![
                Type::Op("fn", vec![Type::Var(14), Type::Var(15)]),
                Type::Op("fn", vec![Type::Var(17), Type::Var(18)]),
            ],
        ));

        assert!(context.infer(&term) == expected);
    }

    #[test]
    fn infer_ok_5() {
        let mut context = Context::default();
        let a = context.state.next_var().1;
        let b = context.state.next_var().1;
        context.env.push((
            "pair",
            Type::Op(
                "fn",
                vec![
                    a.clone(),
                    Type::Op("fn", vec![b.clone(), Type::Op("tuple", vec![a, b])]),
                ],
            ),
        ));

        let term = Term::Lambda(
            vec!["g"],
            Box::new(Term::Let(
                "f",
                Box::new((
                    Term::Lambda(vec!["x"], Box::new(Term::Ident("g"))),
                    Term::Apply(vec![
                        Term::Apply(vec![
                            Term::Ident("pair"),
                            Term::Apply(vec![Term::Ident("f"), Term::Int(-1)]),
                        ]),
                        Term::Apply(vec![Term::Ident("f"), Term::Bool(false)]),
                    ]),
                )),
            )),
        );

        let expected = Ok(Type::Op(
            "fn",
            vec![
                Type::Var(5),
                Type::Op("tuple", vec![Type::Var(5), Type::Var(5)]),
            ],
        ));

        assert!(context.infer(&term) == expected);
    }

    #[test]
    fn infer_ok_6() {
        let mut context = Context::default();
        let a = context.state.next_var().1;
        let b = context.state.next_var().1;
        context.env.push((
            "pair",
            Type::Op(
                "fn",
                vec![
                    a.clone(),
                    Type::Op("fn", vec![b.clone(), Type::Op("tuple", vec![a, b])]),
                ],
            ),
        ));

        let term = Term::Lambda(
            vec!["x"],
            Box::new(Term::Apply(vec![
                Term::Apply(vec![
                    Term::Ident("pair"),
                    Term::Access(Box::new(Term::Ident("x")), "y"),
                ]),
                Term::Access(Box::new(Term::Ident("x")), "z"),
            ])),
        );

        let expected = Ok(Type::Op(
            "fn",
            vec![
                Type::Dict([("y", Type::Var(3)), ("z", Type::Var(4))].into(), Some(6)),
                Type::Op("tuple", vec![Type::Var(3), Type::Var(4)]),
            ],
        ));

        assert!(context.infer(&term) == expected);
    }

    #[test]
    fn infer_ok_7() {
        let mut context = Context::default();
        context.env.push((
            "x",
            Type::Dict(
                [
                    ("a", Type::Op("int", vec![])),
                    ("b", Type::Op("bool", vec![])),
                ]
                .into(),
                None,
            ),
        ));

        let term = Term::Apply(vec![
            Term::Lambda(
                vec!["x"],
                Box::new(Term::Access(Box::new(Term::Ident("x")), "a")),
            ),
            Term::Ident("x"),
        ]);

        assert!(context.infer(&term) == Ok(Type::Op("int", vec![])));
    }

    #[test]
    fn infer_ok_8() {
        let mut context = Context::default();
        let x = context.state.next_var().1;
        context.env.push(("x", x));

        let term = Term::Let(
            "f",
            Box::new((
                Term::Lambda(vec!["a", "b", "c"], Box::new(Term::Ident("a"))),
                Term::Apply(vec![
                    Term::Ident("f"),
                    Term::Ident("x"),
                    Term::Int(-1),
                    Term::Bool(false),
                ]),
            )),
        );

        assert!(context.infer(&term) == Ok(Type::Var(4)));
    }

    #[test]
    fn infer_err_undefined() {
        let mut context = Context::default();

        assert!(context.infer(&Term::Ident("x")) == Err(Error::Undefined));
    }

    #[test]
    fn infer_err_infinite_0() {
        let mut context = Context::default();

        let term = Term::Lambda(
            vec!["f"],
            Box::new(Term::Apply(vec![Term::Ident("f"), Term::Ident("f")])),
        );

        assert!(context.infer(&term) == Err(Error::Infinite));
    }

    #[test]
    fn infer_err_infinite_1() {
        let mut context = Context::default();

        let term = Term::Lambda(
            vec!["y"],
            Box::new(Term::Apply(vec![
                Term::Ident("y"),
                Term::Lambda(
                    vec!["z"],
                    Box::new(Term::Apply(vec![Term::Ident("y"), Term::Ident("z")])),
                ),
            ])),
        );

        assert!(context.infer(&term) == Err(Error::Infinite));
    }

    #[test]
    fn infer_err_mismatch() {
        let mut context = Context::default();
        context.env.push((
            "f",
            Type::Op("fn", vec![Type::Op("int", vec![]), Type::Op("int", vec![])]),
        ));

        let term = Term::Apply(vec![Term::Ident("f"), Term::Bool(true)]);

        assert!(context.infer(&term) == Err(Error::Mismatch));
    }

    #[test]
    fn infer_err_arity_0() {
        let mut context = Context::default();

        let term = Term::Let(
            "f",
            Box::new((
                Term::Lambda(vec!["a", "b"], Box::new(Term::Ident("a"))),
                Term::Apply(vec![Term::Ident("f"), Term::Int(-1)]),
            )),
        );

        assert!(context.infer(&term) == Err(Error::Arity));
    }

    #[test]
    fn infer_err_arity_1() {
        let mut context = Context::default();

        let term = Term::Let(
            "f",
            Box::new((
                Term::Lambda(vec!["a"], Box::new(Term::Ident("a"))),
                Term::Apply(vec![Term::Ident("f"), Term::Bool(false), Term::Int(-1)]),
            )),
        );

        assert!(context.infer(&term) == Err(Error::Arity));
    }

    #[test]
    fn infer_err_key() {
        let mut context = Context::default();
        context.env.push((
            "x",
            Type::Dict([("y", Type::Op("int", vec![]))].into(), None),
        ));

        let term = Term::Access(Box::new(Term::Ident("x")), "z");

        assert!(context.infer(&term) == Err(Error::Key));
    }
}
