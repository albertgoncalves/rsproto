static mut EXAMPLE: i32 = 0;

const fn example(ref1: &mut i32, ref2: &mut i32) -> i32 {
    *ref1 = 1;
    *ref2 = 2;
    *ref1
}

fn main() {
    use std::hint::black_box;
    let ref1 = black_box(unsafe {
        #[allow(static_mut_refs)]
        &mut EXAMPLE
    });
    let ref2 = black_box(unsafe {
        #[allow(static_mut_refs)]
        &mut EXAMPLE
    });
    println!("{}", example(ref1, ref2));
}
