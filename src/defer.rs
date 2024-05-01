struct Defer<F: FnMut()>(F);

impl<F: FnMut()> Drop for Defer<F> {
    fn drop(&mut self) {
        (self.0)();
    }
}

macro_rules! defer {
    ($expr:expr) => {
        let __defer__ = Defer(|| {
            $expr;
        });
    };
}

fn main() {
    defer!(println!("1"));
    println!("0");
    panic!();
}
