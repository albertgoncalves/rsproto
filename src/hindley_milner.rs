// NOTE: See `https://gist.github.com/kseo/9383472`.

use std::collections::{HashMap, HashSet};
use std::fmt;

enum Term<'a> {
    Int(i64),
    Bool(bool),
    Ident(&'a str),
    Lambda(&'a str, Box<Term<'a>>),
    Apply(Box<(Term<'a>, Term<'a>)>),
    Let(&'a str, Box<(Term<'a>, Term<'a>)>),
    LetRecs(Vec<(&'a str, Term<'a>)>, Box<Term<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
enum Type<'a> {
    Var(u32),
    Op(&'a str, Vec<Type<'a>>),
}

#[derive(Debug, PartialEq)]
enum Error {
    Undefined,
    Infinite,
    Op,
    Arity,
}

impl fmt::Display for Term<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Bool(r#bool) => write!(f, "{bool}"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Lambda(arg, term) => write!(f, "\\{arg} -> {term}"),
            Self::Apply(func_arg) => {
                let (func, arg) = func_arg.as_ref();
                write!(f, "({func} {arg})")
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
            Self::Op("fn", types) => {
                assert!(types.len() == 2);
                write!(f, "({} -> {})", types[0], types[1])
            }
            Self::Op("tuple", types) => {
                assert!(types.len() == 2);
                write!(f, "({}, {})", types[0], types[1])
            }
            Self::Op(..) => todo!(),
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

    fn prune(&mut self, r#type: Type<'a>) -> Type<'a> {
        match r#type {
            Type::Var(k) => {
                let Some(r#type) = self.links.remove(&k) else {
                    return r#type;
                };
                let r#type = self.prune(r#type);
                self.links.insert(k, r#type.clone());
                r#type
            }
            Type::Op(op, types) => Type::Op(
                op,
                types.into_iter().map(|r#type| self.prune(r#type)).collect(),
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
        }
    }

    fn ident_to_type(&mut self, ident: &'a str) -> Result<Type<'a>, Error> {
        let Some(i) = self.env.iter().rposition(|(other, _)| ident == *other) else {
            return Err(Error::Undefined);
        };
        self.generics.clear();
        Ok(self.fresh(self.env[i].1.clone()))
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
            (Type::Op(left_op, _), Type::Op(right_op, _)) if left_op != right_op => Err(Error::Op),
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
        }
    }

    fn term_to_type(&mut self, term: &Term<'a>) -> Result<Type<'a>, Error> {
        match term {
            Term::Int(_) => Ok(Type::Op("int", vec![])),
            Term::Bool(_) => Ok(Type::Op("bool", vec![])),
            Term::Ident(ident) => self.ident_to_type(ident),
            Term::Apply(func_arg) => {
                let (func, arg) = func_arg.as_ref();

                let func_type = self.term_to_type(func)?;
                let arg_type = self.term_to_type(arg)?;
                let ret_type = self.state.next_var().1;

                self.unify(Type::Op("fn", vec![arg_type, ret_type.clone()]), func_type)?;

                Ok(self.state.prune(ret_type))
            }
            Term::Lambda(arg, term) => {
                let (arg_k, arg_type) = self.state.next_var();
                self.non_generics.insert(arg_k);
                self.env.push((arg, arg_type.clone()));

                let term_type = self.term_to_type(term)?;
                let func_type = Type::Op("fn", vec![arg_type, term_type]);

                assert!(*arg == self.env.pop().unwrap().0);
                assert!(self.non_generics.remove(&arg_k));

                Ok(self.state.prune(func_type))
            }
            Term::Let(ident, value_body) => {
                let (value, body) = value_body.as_ref();

                let value_type = self.term_to_type(value)?;
                self.env.push((ident, value_type));
                let body_type = self.term_to_type(body)?;

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
                    let value_type = self.term_to_type(value)?;
                    self.unify(value_type, r#type)?;
                }
                for k in k_removes {
                    assert!(self.non_generics.remove(&k));
                }

                let body_type = self.term_to_type(body)?;

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
            vec![
                a.clone(),
                Type::Op("fn", vec![b.clone(), Type::Op("tuple", vec![a, b])]),
            ],
        ),
    ));

    let term = Term::Let(
        "f",
        Box::new((
            Term::Lambda("x", Box::new(Term::Ident("x"))),
            Term::Apply(Box::new((
                Term::Apply(Box::new((
                    Term::Ident("pair"),
                    Term::Apply(Box::new((Term::Ident("f"), Term::Int(-123)))),
                ))),
                Term::Apply(Box::new((Term::Ident("f"), Term::Bool(true)))),
            ))),
        )),
    );

    let r#type = context.term_to_type(&term).unwrap();
    println!("{term} : {type}");

    for (k, r#type) in &context.state.links {
        println!("{k:>3}: {type}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn term_to_type_ok_0() {
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
                Term::Lambda("x", Box::new(Term::Ident("x"))),
                Term::Apply(Box::new((
                    Term::Apply(Box::new((
                        Term::Ident("pair"),
                        Term::Apply(Box::new((Term::Ident("f"), Term::Int(-123)))),
                    ))),
                    Term::Apply(Box::new((Term::Ident("f"), Term::Bool(true)))),
                ))),
            )),
        );

        let expected = Ok(Type::Op(
            "tuple",
            vec![Type::Op("int", vec![]), Type::Op("bool", vec![])],
        ));

        assert!(context.term_to_type(&term) == expected);
    }

    #[test]
    fn term_to_type_ok_1() {
        let mut context = Context::default();

        let term = Term::Let(
            "g",
            Box::new((
                Term::Lambda("f", Box::new(Term::Int(-1))),
                Term::Apply(Box::new((Term::Ident("g"), Term::Ident("g")))),
            )),
        );

        assert!(context.term_to_type(&term) == Ok(Type::Op("int", vec![])));
    }

    #[test]
    fn term_to_type_ok_2() {
        let mut context = Context::default();

        let term = Term::Lambda(
            "f",
            Box::new(Term::Lambda(
                "g",
                Box::new(Term::Lambda(
                    "x",
                    Box::new(Term::Apply(Box::new((
                        Term::Ident("g"),
                        Term::Apply(Box::new((Term::Ident("f"), Term::Ident("x")))),
                    )))),
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

        assert!(context.term_to_type(&term) == expected);
    }

    #[test]
    fn term_to_type_ok_3() {
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
                        "x",
                        Box::new(Term::Apply(Box::new((Term::Ident("g"), Term::Ident("x"))))),
                    ),
                ),
                (
                    "g",
                    Term::Lambda(
                        "x",
                        Box::new(Term::Apply(Box::new((Term::Ident("f"), Term::Ident("x"))))),
                    ),
                ),
            ],
            Box::new(Term::Apply(Box::new((
                Term::Apply(Box::new((
                    Term::Ident("pair"),
                    Term::Apply(Box::new((Term::Ident("g"), Term::Bool(true)))),
                ))),
                Term::Apply(Box::new((Term::Ident("f"), Term::Int(-1)))),
            )))),
        );

        let expected = Ok(Type::Op("tuple", vec![Type::Var(8), Type::Var(9)]));

        assert!(context.term_to_type(&term) == expected);
    }

    #[test]
    fn term_to_type_ok_4() {
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
                        "x",
                        Box::new(Term::Apply(Box::new((Term::Ident("g"), Term::Ident("x"))))),
                    ),
                ),
                (
                    "g",
                    Term::Lambda(
                        "x",
                        Box::new(Term::Apply(Box::new((Term::Ident("f"), Term::Ident("x"))))),
                    ),
                ),
            ],
            Box::new(Term::Let(
                "x",
                Box::new((
                    Term::Lambda(
                        "y",
                        Box::new(Term::Apply(Box::new((Term::Ident("f"), Term::Int(0))))),
                    ),
                    Term::Apply(Box::new((
                        Term::Apply(Box::new((Term::Ident("pair"), Term::Ident("g")))),
                        Term::Ident("x"),
                    ))),
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

        assert!(context.term_to_type(&term) == expected);
    }

    #[test]
    fn term_to_type_ok_5() {
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
            "g",
            Box::new(Term::Let(
                "f",
                Box::new((
                    Term::Lambda("x", Box::new(Term::Ident("g"))),
                    Term::Apply(Box::new((
                        Term::Apply(Box::new((
                            Term::Ident("pair"),
                            Term::Apply(Box::new((Term::Ident("f"), Term::Int(-1)))),
                        ))),
                        Term::Apply(Box::new((Term::Ident("f"), Term::Bool(false)))),
                    ))),
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

        assert!(context.term_to_type(&term) == expected);
    }

    #[test]
    fn term_to_type_err_undefined() {
        let mut context = Context::default();

        assert!(context.term_to_type(&Term::Ident("x")) == Err(Error::Undefined));
    }

    #[test]
    fn term_to_type_err_infnite() {
        let mut context = Context::default();

        let term = Term::Lambda(
            "f",
            Box::new(Term::Apply(Box::new((Term::Ident("f"), Term::Ident("f"))))),
        );

        assert!(context.term_to_type(&term) == Err(Error::Infinite));
    }

    #[test]
    fn term_to_type_err_op() {
        let mut context = Context::default();
        context.env.push((
            "f",
            Type::Op("fn", vec![Type::Op("int", vec![]), Type::Op("int", vec![])]),
        ));

        let term = Term::Apply(Box::new((Term::Ident("f"), Term::Bool(true))));

        assert!(context.term_to_type(&term) == Err(Error::Op));
    }

    #[test]
    fn term_to_type_err_arity() {
        let mut context = Context::default();
        context.env.push((
            "f",
            Type::Op(
                "fn",
                vec![
                    Type::Op("int", vec![]),
                    Type::Op("int", vec![]),
                    Type::Op("int", vec![]),
                ],
            ),
        ));

        let term = Term::Apply(Box::new((Term::Ident("f"), Term::Int(-1))));

        assert!(context.term_to_type(&term) == Err(Error::Arity));
    }
}
