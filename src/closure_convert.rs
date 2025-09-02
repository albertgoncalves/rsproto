use std::collections::{HashSet, VecDeque};
use std::fmt;
use std::mem;

#[derive(Clone, Debug)]
enum Stmt<'a> {
    Void(Expr<'a>),
    Let(&'a str, Expr<'a>),
    Set(Expr<'a>, Expr<'a>),
    Return(Expr<'a>),
    CompilerLet(usize, Expr<'a>),
}

#[derive(Clone, Debug)]
enum Expr<'a> {
    Int(i64),
    Var(&'a str),
    Call(Vec<Expr<'a>>),
    Func(Vec<&'a str>, VecDeque<Stmt<'a>>),
    Array(Vec<Expr<'a>>),
    Access(Box<Expr<'a>>, usize),
    CompilerVar(usize),
}

fn display_stmt(stmt: &Stmt<'_>, f: &mut fmt::Formatter<'_>, pad: usize) -> fmt::Result {
    write!(f, "{:pad$}", "")?;
    match stmt {
        Stmt::Void(expr) => display_expr(expr, f, pad),
        Stmt::Let(var, value) => {
            write!(f, "var {var} = ")?;
            display_expr(value, f, pad)
        }
        Stmt::Set(target, value) => {
            display_expr(target, f, pad)?;
            write!(f, " = ")?;
            display_expr(value, f, pad)
        }
        Stmt::Return(expr) => {
            write!(f, "return ")?;
            display_expr(expr, f, pad)
        }
        Stmt::CompilerLet(k, expr) => {
            write!(f, "var __{k}__ = ")?;
            display_expr(expr, f, pad)
        }
    }?;
    writeln!(f, ";")
}

fn display_expr(expr: &Expr<'_>, f: &mut fmt::Formatter<'_>, pad: usize) -> fmt::Result {
    match expr {
        Expr::Int(int) => write!(f, "{int}"),
        Expr::Var(var) => write!(f, "{var}"),
        Expr::Call(exprs) => {
            if matches!(exprs[0], Expr::Var("+")) {
                assert!(exprs.len() == 3);
                display_expr(&exprs[1], f, pad)?;
                write!(f, " + ")?;
                display_expr(&exprs[2], f, pad)
            } else {
                display_expr(&exprs[0], f, pad)?;
                write!(f, "(")?;
                let n = exprs.len();
                if 1 < n {
                    for expr in &exprs[1..n - 1] {
                        display_expr(expr, f, pad)?;
                        write!(f, ", ")?;
                    }
                    display_expr(&exprs[n - 1], f, pad)?;
                }
                write!(f, ")")
            }
        }
        Expr::Func(args, stmts) => {
            write!(f, "(function(")?;
            let n = args.len();
            if 0 < n {
                for arg in &args[..n - 1] {
                    write!(f, "{arg}, ")?;
                }
                write!(f, "{}", args[n - 1])?;
            }
            writeln!(f, ") {{")?;
            for stmt in stmts {
                display_stmt(stmt, f, pad + 4)?;
            }
            write!(f, "{:pad$}}})", "")
        }
        Expr::Array(exprs) => {
            write!(f, "[")?;
            let n = exprs.len();
            if 0 < n {
                for expr in &exprs[..n - 1] {
                    display_expr(expr, f, pad)?;
                    write!(f, ", ")?;
                }
                display_expr(&exprs[n - 1], f, pad)?;
            }
            write!(f, "]")
        }
        Expr::Access(expr, index) => {
            display_expr(expr, f, pad)?;
            write!(f, "[{index}]")
        }
        Expr::CompilerVar(k) => write!(f, "__{k}__"),
    }
}

impl fmt::Display for Stmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_stmt(self, f, 0)
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_expr(self, f, 0)
    }
}

#[derive(Debug)]
struct State<'a> {
    locals: Vec<HashSet<&'a str>>,
    args: Vec<HashSet<&'a str>>,
    frees: Vec<Vec<&'a str>>,
    functions: VecDeque<Stmt<'a>>,
}

impl<'a> State<'a> {
    fn stack_len(&self) -> usize {
        let n = self.locals.len();
        assert!(n == self.args.len());
        assert!(n == self.frees.len());
        n
    }

    fn closure_convert_stmt(&mut self, stmt: &mut Stmt<'a>, globals: &HashSet<&'a str>) {
        match stmt {
            Stmt::Void(expr) | Stmt::Return(expr) => self.closure_convert_expr(expr, globals),
            Stmt::Let(var, value) => {
                self.closure_convert_expr(value, globals);
                let n = self.stack_len();
                self.locals[n - 1].insert(var);
            }
            Stmt::Set(target, value) => {
                self.closure_convert_expr(value, globals);
                self.closure_convert_expr(target, globals);
            }
            Stmt::CompilerLet(..) => unreachable!(),
        }
    }

    fn closure_convert_stmts(
        &mut self,
        stmts: &mut VecDeque<Stmt<'a>>,
        globals: &HashSet<&'a str>,
    ) {
        let n = self.stack_len();
        let mut args: Vec<&'a str> = vec![];
        let mut escapes: HashSet<&'a str> = HashSet::new();
        for stmt in &mut *stmts {
            self.closure_convert_stmt(stmt, globals);
            let mut frees: Vec<&'a str> = vec![];
            for free in &self.frees[n - 1] {
                if self.locals[n - 1].contains(free) {
                    escapes.insert(free);
                } else if self.args[n - 1].contains(free) {
                    args.push(free);
                    escapes.insert(free);
                } else {
                    frees.push(free);
                }
            }
            drop(mem::replace(&mut self.frees[n - 1], frees));
        }
        local_escape_stmts(stmts, &escapes);
        for arg in args {
            stmts.push_front(Stmt::Set(Expr::Var(arg), Expr::Array(vec![Expr::Var(arg)])));
        }
    }

    fn closure_convert_expr(&mut self, expr: &mut Expr<'a>, globals: &HashSet<&'a str>) {
        match expr {
            Expr::Int(..) => (),
            Expr::Var(var) => {
                let n = self.stack_len();

                if !globals.contains(var)
                    && !self.locals[n - 1].contains(var)
                    && !self.args[n - 1].contains(var)
                {
                    if !self.frees[n - 1].contains(var) {
                        self.frees[n - 1].push(var);
                    }

                    let mut swapped = Expr::Int(0);
                    mem::swap(expr, &mut swapped);
                    *expr = Expr::Access(Box::new(swapped), 0);
                }
            }
            Expr::Call(exprs) | Expr::Array(exprs) => self.closure_convert_exprs(exprs, globals),
            Expr::Access(expr, _) => self.closure_convert_expr(expr, globals),
            Expr::Func(args, stmts) => {
                self.locals.push(HashSet::new());
                self.args.push(HashSet::new());
                self.frees.push(vec![]);

                let n = self.stack_len();
                for arg in &mut *args {
                    self.args[n - 1].insert(arg);
                }

                self.closure_convert_stmts(stmts, globals);

                args.push("__env__");

                self.locals.pop().unwrap();
                self.args.pop().unwrap();
                let frees = self.frees.pop().unwrap();

                let n = self.stack_len();
                let mut closed = vec![];
                for (i, free) in frees.into_iter().enumerate() {
                    self.frees[n - 1].push(free);
                    closed.push(Expr::Var(free));
                    stmts.push_front(Stmt::Let(
                        free,
                        Expr::Access(Box::new(Expr::Var("__env__")), i),
                    ));
                }

                let mut swapped = Expr::Int(0);
                mem::swap(expr, &mut swapped);
                *expr = Expr::Call(vec![Expr::Var("__closure__"), swapped, Expr::Array(closed)]);
            }
            Expr::CompilerVar(..) => unreachable!(),
        }
    }

    fn closure_convert_exprs(&mut self, exprs: &mut [Expr<'a>], globals: &HashSet<&'a str>) {
        for expr in exprs {
            self.closure_convert_expr(expr, globals);
        }
    }

    fn function_lift_stmt(&mut self, stmt: &mut Stmt<'a>) {
        match stmt {
            Stmt::Void(expr) | Stmt::Return(expr) => self.function_lift_expr(expr),
            Stmt::Let(_, value) => self.function_lift_expr(value),
            Stmt::Set(target, value) => {
                self.function_lift_expr(value);
                self.function_lift_expr(target);
            }
            Stmt::CompilerLet(..) => unreachable!(),
        }
    }

    fn function_lift_stmts(&mut self, stmts: &mut VecDeque<Stmt<'a>>) {
        for stmt in stmts {
            self.function_lift_stmt(stmt);
        }
    }

    fn function_lift_expr(&mut self, expr: &mut Expr<'a>) {
        match expr {
            Expr::Int(..) | Expr::Var(..) => (),
            Expr::Call(exprs) | Expr::Array(exprs) => self.function_lift_exprs(exprs),
            Expr::Access(expr, _) => self.function_lift_expr(expr),
            Expr::Func(_, stmts) => {
                self.function_lift_stmts(stmts);

                let k = self.functions.len();
                let swapped = Expr::CompilerVar(k);
                self.functions.push_back(Stmt::CompilerLet(k, mem::replace(expr, swapped)));
            }
            Expr::CompilerVar(..) => unreachable!(),
        }
    }

    fn function_lift_exprs(&mut self, exprs: &mut [Expr<'a>]) {
        for expr in exprs {
            self.function_lift_expr(expr);
        }
    }
}

fn local_escape_stmt<'a>(stmt: &mut Stmt<'a>, escapes: &HashSet<&'a str>) {
    match stmt {
        Stmt::Void(expr) | Stmt::Return(expr) => local_escape_expr(expr, escapes),
        Stmt::Let(var, value) => {
            local_escape_expr(value, escapes);
            if escapes.contains(var) {
                let mut swapped = Expr::Int(0);
                mem::swap(value, &mut swapped);
                *value = Expr::Array(vec![swapped]);
            }
        }
        Stmt::Set(target, value) => {
            local_escape_expr(value, escapes);
            local_escape_expr(target, escapes);
        }
        Stmt::CompilerLet(..) => unreachable!(),
    }
}

fn local_escape_stmts<'a>(stmts: &mut VecDeque<Stmt<'a>>, escapes: &HashSet<&'a str>) {
    for stmt in stmts {
        local_escape_stmt(stmt, escapes);
    }
}

fn local_escape_expr<'a>(expr: &mut Expr<'a>, escapes: &HashSet<&'a str>) {
    match expr {
        Expr::Int(..) | Expr::Func(..) => (),
        Expr::Var(var) => {
            if escapes.contains(var) {
                let mut swapped = Expr::Int(0);
                mem::swap(expr, &mut swapped);
                *expr = Expr::Array(vec![swapped]);
            }
        }
        Expr::Call(exprs) => {
            if !matches!(exprs[0], Expr::Var("__closure__")) {
                local_escape_exprs(exprs, escapes);
            }
        }
        Expr::Array(exprs) => local_escape_exprs(exprs, escapes),
        Expr::Access(expr, _) => local_escape_expr(expr, escapes),
        Expr::CompilerVar(..) => unreachable!(),
    }
}

fn local_escape_exprs<'a>(exprs: &mut [Expr<'a>], escapes: &HashSet<&'a str>) {
    for expr in exprs {
        local_escape_expr(expr, escapes);
    }
}

fn call_convert_stmt(stmt: &mut Stmt<'_>, globals: &HashSet<&'_ str>) {
    match stmt {
        Stmt::Void(expr) | Stmt::Return(expr) | Stmt::CompilerLet(_, expr) => {
            call_convert_expr(expr, globals);
        }
        Stmt::Let(_, value) => call_convert_expr(value, globals),
        Stmt::Set(target, value) => {
            call_convert_expr(value, globals);
            call_convert_expr(target, globals);
        }
    }
}

fn call_convert_stmts(stmts: &mut VecDeque<Stmt<'_>>, globals: &HashSet<&'_ str>) {
    for stmt in stmts {
        call_convert_stmt(stmt, globals);
    }
}

fn call_convert_expr(expr: &mut Expr<'_>, globals: &HashSet<&'_ str>) {
    match expr {
        Expr::Int(..) | Expr::Var(..) | Expr::CompilerVar(..) => (),
        Expr::Call(exprs) => {
            call_convert_exprs(exprs, globals);
            match &mut exprs[0] {
                Expr::Var("__closure__") => {
                    let mut exprs = mem::take(exprs);
                    exprs.remove(0);
                    *expr = Expr::Array(exprs);
                }
                Expr::Var(var) if globals.contains(var) => (),
                Expr::Array(exprs) => {
                    assert!(exprs.len() == 2);
                    *expr = Expr::Call(mem::take(exprs));
                }
                _ => {
                    let mut swapped = Expr::Int(0);
                    mem::swap(&mut exprs[0], &mut swapped);
                    exprs[0] = Expr::Access(Box::new(swapped.clone()), 0);
                    exprs.push(Expr::Access(Box::new(swapped), 1));
                }
            }
        }
        Expr::Array(exprs) => call_convert_exprs(exprs, globals),
        Expr::Access(expr, _) => call_convert_expr(expr, globals),
        Expr::Func(_, stmts) => call_convert_stmts(stmts, globals),
    }
}

fn call_convert_exprs(exprs: &mut [Expr<'_>], globals: &HashSet<&'_ str>) {
    for expr in exprs {
        call_convert_expr(expr, globals);
    }
}

fn main() {
    let mut stmt = Stmt::Void(Expr::Call(vec![Expr::Func(
        vec![],
        VecDeque::from([
            Stmt::Let(
                "counter",
                Expr::Func(
                    vec!["k"],
                    VecDeque::from([Stmt::Return(Expr::Call(vec![Expr::Func(
                        vec![],
                        VecDeque::from([
                            Stmt::Let("n", Expr::Int(0)),
                            Stmt::Return(Expr::Func(
                                vec![],
                                VecDeque::from([
                                    Stmt::Let("m", Expr::Var("n")),
                                    Stmt::Set(
                                        Expr::Var("n"),
                                        Expr::Call(vec![
                                            Expr::Var("+"),
                                            Expr::Var("n"),
                                            Expr::Var("k"),
                                        ]),
                                    ),
                                    Stmt::Set(
                                        Expr::Var("k"),
                                        Expr::Call(vec![
                                            Expr::Var("+"),
                                            Expr::Var("k"),
                                            Expr::Int(1),
                                        ]),
                                    ),
                                    Stmt::Return(Expr::Var("m")),
                                ]),
                            )),
                        ]),
                    )]))]),
                ),
            ),
            Stmt::Let(
                "instances",
                Expr::Array(vec![
                    Expr::Call(vec![Expr::Var("counter"), Expr::Int(1)]),
                    Expr::Call(vec![Expr::Var("counter"), Expr::Int(2)]),
                ]),
            ),
            Stmt::Void(Expr::Call(vec![
                Expr::Var("console.log"),
                Expr::Call(vec![Expr::Access(Box::new(Expr::Var("instances")), 0)]),
                Expr::Call(vec![Expr::Access(Box::new(Expr::Var("instances")), 1)]),
            ])),
            Stmt::Void(Expr::Call(vec![
                Expr::Var("console.log"),
                Expr::Call(vec![Expr::Access(Box::new(Expr::Var("instances")), 0)]),
                Expr::Call(vec![Expr::Access(Box::new(Expr::Var("instances")), 1)]),
            ])),
            Stmt::Void(Expr::Call(vec![
                Expr::Var("console.log"),
                Expr::Call(vec![Expr::Access(Box::new(Expr::Var("instances")), 0)]),
                Expr::Call(vec![Expr::Access(Box::new(Expr::Var("instances")), 1)]),
            ])),
        ]),
    )]));

    eprintln!("{stmt}");

    let mut state = State {
        locals: vec![HashSet::new()],
        args: vec![HashSet::new()],
        frees: vec![vec![]],
        functions: VecDeque::new(),
    };

    let globals = HashSet::from(["+", "console.log"]);

    state.closure_convert_stmt(&mut stmt, &globals);
    state.function_lift_stmt(&mut stmt);

    call_convert_stmt(&mut stmt, &globals);
    call_convert_stmts(&mut state.functions, &globals);

    println!("\"use strict\";\n");
    for stmt in state.functions {
        println!("{stmt}");
    }
    println!("{stmt}");
}
