use std::fmt;

struct MyStruct<T>(T);

impl<T> MyStruct<T> {
    const fn new(x: T) -> Self {
        Self(x)
    }
}

impl<T> Drop for MyStruct<T> {
    fn drop(&mut self) {
        println!("Calling `drop`");
    }
}

impl<T> fmt::Display for MyStruct<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn main() {
    let my_struct = MyStruct::new(-123);
    println!("{my_struct}");
}
