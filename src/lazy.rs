use std::mem;
use std::thread;
use std::time;

struct Closure<A, B> {
    func: fn(A) -> B,
    args: A,
}

union Internal<A, B> {
    closure: std::mem::ManuallyDrop<Closure<A, B>>,
    result: std::mem::ManuallyDrop<B>,
}

struct Lazy<A, B> {
    eval: fn(&mut Lazy<A, B>) -> B,
    internal: Internal<A, B>,
}

impl<A, B: Clone> Lazy<A, B> {
    fn new(func: fn(A) -> B, args: A) -> Self {
        Self {
            eval: Self::before,
            internal: Internal {
                closure: mem::ManuallyDrop::new(Closure { func, args }),
            },
        }
    }

    fn before(&mut self) -> B {
        unsafe {
            // TODO: Are we leaking any memory?
            let closure = mem::ManuallyDrop::take(&mut self.internal.closure);
            let result = (closure.func)(closure.args);
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
