use std::mem;
use std::thread;
use std::time;

union Internal<A, B> {
    args: std::mem::ManuallyDrop<A>,
    result: std::mem::ManuallyDrop<B>,
}

struct Lazy<A, B> {
    eval: fn(&mut Lazy<A, B>) -> B,
    func: fn(A) -> B,
    internal: Internal<A, B>,
}

impl<A, B: Clone> Lazy<A, B> {
    fn new(func: fn(A) -> B, args: A) -> Self {
        Self {
            eval: Self::before,
            func,
            internal: Internal {
                args: mem::ManuallyDrop::new(args),
            },
        }
    }

    fn before(&mut self) -> B {
        unsafe {
            // TODO: Are we leaking any memory?
            let args = mem::ManuallyDrop::take(&mut self.internal.args);
            let result = (self.func)(args);
            *self.internal.result = result.clone();
            self.eval = Self::after;
            result
        }
    }

    fn after(&mut self) -> B {
        unsafe { (*self.internal.result).clone() }
    }

    fn call(&mut self) -> B {
        (self.eval)(self)
    }
}

fn main() {
    println!("{}", mem::size_of::<Lazy<(&str, i64), i64>>());

    let mut lazy = Lazy::new(
        |(message, x)| {
            println!("{message}");
            thread::sleep(time::Duration::from_millis(500));
            x
        },
        ("?!", -123),
    );

    for _ in 0..5 {
        println!("{}", lazy.call());
    }
}
