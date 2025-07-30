use std::mem;
use std::thread;
use std::time;

union Internal<'a, T> {
    closure: std::mem::ManuallyDrop<Box<dyn FnOnce() -> T + 'a>>,
    value: mem::ManuallyDrop<T>,
}

struct Lazy<'a, T> {
    eval: fn(&mut Lazy<'a, T>) -> T,
    internal: Internal<'a, T>,
}

impl<'a, T: Clone> Lazy<'a, T> {
    fn new(closure: Box<dyn FnOnce() -> T + 'a>) -> Self {
        Self {
            internal: Internal {
                closure: mem::ManuallyDrop::new(closure),
            },
            eval: Lazy::before,
        }
    }

    fn before(&mut self) -> T {
        unsafe {
            // TODO: Are we leaking any memory?
            let closure = mem::ManuallyDrop::take(&mut self.internal.closure);
            let value = closure();
            self.eval = Lazy::after;
            *self.internal.value = value.clone();
            value
        }
    }

    fn after(&mut self) -> T {
        unsafe { (*self.internal.value).clone() }
    }

    fn call(&mut self) -> T {
        (self.eval)(self)
    }
}

fn main() {
    println!("{}", mem::size_of::<Lazy<'_, i32>>());

    let mut x: i32 = -111;
    let mut lazy = Lazy::new(Box::new(|| {
        println!("!");
        thread::sleep(time::Duration::from_millis(500));
        x += 555;
        x
    }));

    for _ in 0..5 {
        println!("{}", lazy.call());
    }
}
