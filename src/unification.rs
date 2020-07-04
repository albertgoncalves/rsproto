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
                components: vec![],
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

fn main() {
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
    let _i_y: RowIndex = set_row(&mut rows, &mut vars, &y);
    let _i_x: RowIndex = set_row(&mut rows, &mut vars, &x);
    println!("{:#?}", rows);
}
