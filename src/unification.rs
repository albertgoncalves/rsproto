use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::str::Chars;

/* NOTE: See `https://arxiv.org/abs/cs/0603080v1`. */

#[derive(Copy, Clone, Debug, PartialEq)]
enum Type {
    Var(char),
    Const(char),
}

#[derive(Debug, PartialEq)]
enum Expr {
    Atom(Type),
    Func(char, Vec<Expr>),
}

type RowIndex = usize;

#[derive(Debug)]
struct Row {
    index: RowIndex,
    functor: char,
    type_: Type,
    arity: usize,
    components: Vec<RowIndex>,
}

type Vars = HashMap<char, RowIndex>;

type Bindings = HashMap<RowIndex, RowIndex>;

fn get_func(chars: &mut Peekable<Chars<'_>>) -> Expr {
    if let Some(functor) = chars.next() {
        let mut components: Vec<Expr> = Vec::new();
        while let Some(c) = chars.peek() {
            match c {
                ')' => {
                    let _: Option<char> = chars.next();
                    return Expr::Func(functor, components);
                }
                _ if c.is_whitespace() => {
                    let _: Option<char> = chars.next();
                }
                _ => components.push(get_expr(chars)),
            }
        }
    }
    panic!()
}

fn get_expr(chars: &mut Peekable<Chars<'_>>) -> Expr {
    if let Some(c) = chars.next() {
        match c {
            '(' => return get_func(chars),
            _ if c.is_alphabetic() && c.is_uppercase() => {
                return Expr::Atom(Type::Var(c))
            }
            _ if c.is_alphabetic() && c.is_lowercase() => {
                return Expr::Atom(Type::Const(c))
            }
            _ => (),
        }
    }
    panic!()
}

fn parse(string: &str) -> Expr {
    let mut chars: Peekable<Chars<'_>> = string.chars().peekable();
    get_expr(&mut chars)
}

fn set_row(rows: &mut Vec<Row>, vars: &mut Vars, expr: &Expr) -> RowIndex {
    match expr {
        Expr::Atom(t) => {
            let index: RowIndex = rows.len();
            let functor: char = match t {
                Type::Var(f) => {
                    if let Some(index) = vars.get(f) {
                        return *index;
                    } else {
                        let _: Option<RowIndex> = vars.insert(*f, index);
                    }
                    *f
                }
                Type::Const(f) => *f,
            };
            rows.push(Row {
                index,
                functor,
                type_: *t,
                arity: 0,
                components: Vec::new(),
            });
        }
        Expr::Func(f, exprs) => {
            let n: usize = exprs.len();
            let mut components: Vec<usize> = Vec::with_capacity(n);
            for i in (0..n).rev() {
                components.push(set_row(rows, vars, &exprs[i]));
            }
            components.reverse();
            rows.push(Row {
                index: rows.len(),
                functor: *f,
                type_: Type::Const(*f),
                arity: n,
                components,
            });
        }
    }
    rows.len() - 1
}

fn deref(bindings: &Bindings, mut index: RowIndex) -> RowIndex {
    let mut prev_indices: HashSet<RowIndex> = HashSet::new();
    while let Some(next_index) = bindings.get(&index) {
        index = *next_index;
        if prev_indices.contains(&index) {
            return index;
        }
        let _: bool = prev_indices.insert(index);
    }
    index
}

fn get_term(rows: &[Row], bindings: &Bindings, index: RowIndex) -> String {
    let row: &Row = &rows[deref(bindings, index)];
    if row.arity == 0 {
        return row.functor.to_string();
    }
    let mut components: Vec<String> = Vec::with_capacity(row.components.len());
    for i in &row.components {
        let deref_i: RowIndex = deref(bindings, *i);
        if index != deref_i {
            components.push(get_term(rows, bindings, deref_i));
        }
    }
    let components: String = components.join(" ");
    let mut s: String = String::with_capacity(components.len() + 4);
    s.push('(');
    s.push(row.functor);
    s.push(' ');
    s.push_str(&components);
    s.push(')');
    s
}

fn unify(
    rows: &[Row],
    bindings: &mut Bindings,
    init_x: RowIndex,
    init_y: RowIndex,
) -> bool {
    let mut stack_x: Vec<RowIndex> = vec![init_x];
    let mut stack_y: Vec<RowIndex> = vec![init_y];

    macro_rules! const_var {
        ($index:expr, $row_const:expr, $row_var:expr $(,)?) => {{
            if let Some(bound_index) =
                bindings.get(&$row_var.index).map(|x| *x)
            {
                let deref_index: RowIndex = deref(bindings, bound_index);
                match rows[deref_index].type_ {
                    Type::Const(_) => {
                        stack_x.push($index);
                        stack_y.push(deref_index);
                    }
                    Type::Var(_) => {
                        let _: Option<RowIndex> =
                            bindings.insert(deref_index, $index);
                    }
                }
            } else {
                let _: Option<RowIndex> =
                    bindings.insert($row_var.index, $row_const.index);
            }
        }};
    }

    while let (Some(i), Some(j)) = (stack_x.pop(), stack_y.pop()) {
        let row_i: &Row = &rows[i];
        let row_j: &Row = &rows[j];
        match (row_i.type_, row_j.type_) {
            (Type::Const(f_i), Type::Const(f_j)) => {
                if (f_i == f_j) && (row_i.arity == row_j.arity) {
                    for c in &row_i.components {
                        stack_x.push(*c);
                    }
                    for c in &row_j.components {
                        stack_y.push(*c);
                    }
                } else {
                    return false;
                }
            }
            (Type::Const(_), Type::Var(_)) => const_var!(i, row_i, row_j),
            (Type::Var(_), Type::Const(_)) => const_var!(j, row_j, row_i),
            (Type::Var(_), Type::Var(_)) => {
                match (bindings.get(&i), bindings.get(&j)) {
                    (None, None) => {
                        let _: Option<RowIndex> = bindings.insert(i, j);
                    }
                    (None, Some(_)) => {
                        let _: Option<RowIndex> = bindings.insert(i, j);
                    }
                    (Some(_), None) => {
                        let _: Option<RowIndex> = bindings.insert(j, i);
                    }
                    (Some(bound_i), Some(bound_j)) => {
                        stack_x.push(deref(bindings, *bound_i));
                        stack_y.push(deref(bindings, *bound_j));
                    }
                }
            }
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::{
        get_term, parse, set_row, unify, Bindings, Expr, Row, RowIndex, Type,
        Vars,
    };
    use std::collections::HashMap;

    macro_rules! func {
        ($x:expr, $xs:expr $(,)?) => {
            Expr::Func($x, $xs)
        };
    }

    macro_rules! atom {
        (Var, $x:expr $(,)?) => {
            Expr::Atom(Type::Var($x))
        };
        (Const, $x:expr $(,)?) => {
            Expr::Atom(Type::Const($x))
        };
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("(p Z (h Z W) (f W))"),
            func!(
                'p',
                vec![
                    atom!(Var, 'Z'),
                    func!('h', vec![atom!(Var, 'Z'), atom!(Var, 'W')]),
                    func!('f', vec![atom!(Var, 'W')]),
                ],
            ),
        );
        assert_eq!(
            parse("(p (f X) (h Y (f a)) Y)"),
            func!(
                'p',
                vec![
                    func!('f', vec![atom!(Var, 'X')]),
                    func!(
                        'h',
                        vec![
                            atom!(Var, 'Y'),
                            func!('f', vec![atom!(Const, 'a')])
                        ],
                    ),
                    atom!(Var, 'Y'),
                ],
            ),
        );
    }

    fn unify_and_assert(x: &str, y: &str, pairs: &[(char, &str)]) {
        let mut rows: Vec<Row> = Vec::new();
        let mut vars: Vars = HashMap::new();
        let init_y: RowIndex = set_row(&mut rows, &mut vars, &parse(y));
        let init_x: RowIndex = set_row(&mut rows, &mut vars, &parse(x));
        let mut bindings: Bindings = HashMap::new();
        assert!(unify(&rows, &mut bindings, init_x, init_y));
        for (a, b) in pairs.iter() {
            assert_eq!(get_term(&rows, &bindings, *vars.get(a).unwrap()), *b);
        }
    }

    #[test]
    fn test_unify_0() {
        unify_and_assert("(f A)", "(f B)", &vec![('A', "B")]);
    }

    #[test]
    fn test_unify_1() {
        unify_and_assert("(f A B)", "(f B A)", &vec![('A', "B"), ('B', "A")]);
    }

    #[test]
    fn test_unify_2() {
        unify_and_assert(
            "(p Z (h Z W) (f W))",
            "(p (f X) (h Y (f a)) Y)",
            &vec![
                ('W', "(f a)"),
                ('X', "(f a)"),
                ('Y', "(f (f a))"),
                ('Z', "(f (f a))"),
            ],
        );
    }

    #[test]
    fn test_unify_3() {
        unify_and_assert(
            "(f U (g W X) W b)",
            "(f (g (h a Z) W) U (h a Y) Y)",
            &vec![('W', "(h a b)"), ('X', "(h a b)"), ('Y', "b"), ('Z', "b")],
        )
    }

    #[test]
    fn test_unify_4() {
        unify_and_assert(
            "(f X (g Y))",
            "(f (g Z) W)",
            &vec![('X', "(g Z)"), ('W', "(g Y)")],
        )
    }
}
