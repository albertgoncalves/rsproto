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

fn deref(
    bindings: &HashMap<RowIndex, RowIndex>,
    mut index: RowIndex,
) -> RowIndex {
    while let Some(next_index) = bindings.get(&index) {
        index = *next_index;
    }
    index
}

fn unify(
    rows: &[Row],
    bindings: &mut HashMap<RowIndex, RowIndex>,
    i_x: RowIndex,
    i_y: RowIndex,
) -> Option<Vec<bool>> {
    let n: usize = rows.len();
    let mut mgu: Vec<bool> = vec![false; n];
    let mut stack_x: Vec<RowIndex> = Vec::with_capacity(n);
    let mut stack_y: Vec<RowIndex> = Vec::with_capacity(n);
    stack_x.push(i_x);
    stack_y.push(i_y);
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
                    return None;
                }
            }
            (Type::Var(_), Type::Const(_)) => {
                let mut insert: Option<(RowIndex, RowIndex)> = None;
                if let Some(bound_index) = bindings.get(&row_i.index) {
                    match rows[deref(bindings, *bound_index)].type_ {
                        Type::Const(_) => {
                            stack_x.push(i);
                            stack_y.push(*bound_index);
                        }
                        Type::Var(_) => insert = Some((i, *bound_index)),
                    }
                } else {
                    insert = Some((row_i.index, row_j.index));
                    mgu[j] = true;
                }
                if let Some((k, v)) = insert {
                    let _: Option<RowIndex> = bindings.insert(k, v);
                }
            }
            (Type::Const(_), Type::Var(_)) => {
                let mut insert: Option<(RowIndex, RowIndex)> = None;
                if let Some(bound_index) = bindings.get(&row_j.index) {
                    match rows[deref(bindings, *bound_index)].type_ {
                        Type::Const(_) => {
                            stack_x.push(*bound_index);
                            stack_y.push(j);
                        }
                        Type::Var(_) => insert = Some((j, *bound_index)),
                    }
                } else {
                    insert = Some((row_j.index, row_i.index));
                    mgu[i] = true;
                }
                if let Some((k, v)) = insert {
                    let _: Option<RowIndex> = bindings.insert(k, v);
                }
            }
            (Type::Var(_), Type::Var(_)) => {
                match (bindings.get(&i), bindings.get(&j)) {
                    (None, None) => {
                        let _: Option<RowIndex> = bindings.insert(j, i);
                        mgu[i] = true;
                    }
                    (None, Some(_)) => {
                        let _: Option<RowIndex> = bindings.insert(j, i);
                        mgu[i] = true;
                    }
                    (Some(_), None) => {
                        let _: Option<RowIndex> = bindings.insert(i, j);
                        mgu[j] = true;
                    }
                    (Some(bound_i), Some(bound_j)) => {
                        stack_x.push(*bound_i);
                        stack_y.push(*bound_j);
                    }
                }
            }
        }
    }
    Some(mgu)
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
    let i_y: RowIndex = set_row(&mut rows, &mut vars, &y);
    let i_x: RowIndex = set_row(&mut rows, &mut vars, &x);
    let mut bindings: HashMap<RowIndex, RowIndex> = HashMap::new();
    if let Some(mgu) = unify(&rows, &mut bindings, i_x, i_y) {
        for (i, b) in mgu.iter().enumerate() {
            if *b {
                println!("{:#?}", rows[i])
            }
        }
    }
    println!("{:#?}", bindings)
}
