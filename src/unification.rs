use std::collections::HashMap;

/* NOTE: See `https://arxiv.org/abs/cs/0603080v1`. */

#[derive(Copy, Clone, Debug)]
enum Type {
    Var(char),
    Const(char),
}

#[derive(Debug)]
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

fn set_row(
    rows: &mut Vec<Row>,
    vars: &mut HashMap<char, RowIndex>,
    expr: &Expr,
) -> RowIndex {
    match expr {
        Expr::Atom(t) => {
            let n: RowIndex = rows.len();
            let functor: char = match t {
                Type::Var(f) => {
                    if let Some(index) = vars.get(f) {
                        return *index;
                    } else {
                        let _: Option<RowIndex> = vars.insert(*f, n);
                    }
                    *f
                }
                Type::Const(f) => *f,
            };
            rows.push(Row {
                index: n,
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

fn deref(
    bindings: &HashMap<RowIndex, RowIndex>,
    mut index: RowIndex,
) -> RowIndex {
    while let Some(next_index) = bindings.get(&index) {
        index = *next_index;
    }
    index
}

fn get_term(
    rows: &[Row],
    bindings: &HashMap<RowIndex, RowIndex>,
    index: RowIndex,
) -> String {
    let row: &Row = &rows[deref(bindings, index)];
    if row.arity == 0 {
        return row.functor.to_string();
    }
    let mut components: Vec<String> = Vec::new();
    for i in &row.components {
        let deref_i: RowIndex = deref(bindings, *i);
        if index != deref_i {
            components.push(get_term(rows, bindings, deref_i));
        }
    }
    let components: String = components.join(", ");
    let mut s: String = String::with_capacity(components.len() + 3);
    s.push(row.functor);
    s.push('(');
    s.push_str(&components);
    s.push(')');
    s
}

fn unify(
    rows: &[Row],
    bindings: &mut HashMap<RowIndex, RowIndex>,
    init_x: RowIndex,
    init_y: RowIndex,
) -> bool {
    let mut stack_x: Vec<RowIndex> = Vec::new();
    let mut stack_y: Vec<RowIndex> = Vec::new();

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

    stack_x.push(init_x);
    stack_y.push(init_y);
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
    use super::{get_term, set_row, unify, Expr, Row, RowIndex, Type};
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
    fn test() {
        let x: Expr = func!(
            'p',
            vec![
                atom!(Var, 'Z'),
                func!('h', vec![atom!(Var, 'Z'), atom!(Var, 'W')]),
                func!('f', vec![atom!(Var, 'W')]),
            ],
        );
        let y: Expr = func!(
            'p',
            vec![
                func!('f', vec![atom!(Var, 'X')]),
                func!(
                    'h',
                    vec![atom!(Var, 'Y'), func!('f', vec![atom!(Const, 'a')])],
                ),
                atom!(Var, 'Y'),
            ],
        );
        let mut rows: Vec<Row> = Vec::new();
        let mut vars: HashMap<char, RowIndex> = HashMap::new();
        let init_y: RowIndex = set_row(&mut rows, &mut vars, &y);
        let init_x: RowIndex = set_row(&mut rows, &mut vars, &x);
        let mut bindings: HashMap<RowIndex, RowIndex> = HashMap::new();

        macro_rules! get_unifier {
            ($x:expr $(,)?) => {
                get_term(&rows, &bindings, *vars.get(&$x).unwrap())
            };
        }

        if unify(&rows, &mut bindings, init_x, init_y) {
            assert_eq!(get_unifier!('W'), "f(a)");
            assert_eq!(get_unifier!('X'), "f(a)");
            assert_eq!(get_unifier!('Y'), "f(f(a))");
            assert_eq!(get_unifier!('Z'), "f(f(a))");
        } else {
            assert!(false);
        }
    }
}
