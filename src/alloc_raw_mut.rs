use std::mem;
use std::ptr;

#[derive(Clone)]
struct Item<T> {
    value: T,
    next: *mut Item<T>,
}

struct Array<T, const N: usize> {
    items: [T; N],
    len: usize,
}

fn alloc<T, const N: usize>(array: &mut Array<T, N>) -> *mut T {
    assert!(array.len < N);
    let item = ptr::addr_of_mut!(array.items[array.len]);
    array.len += 1;
    item
}

fn main() {
    let mut array: Array<Item<i32>, 10> = Array {
        items: unsafe { mem::zeroed() },
        len: 0,
    };
    let items = [alloc(&mut array), alloc(&mut array)];

    unsafe {
        (*items[0]).value = -123;
        (*items[0]).next = items[1];

        (*items[1]).value = 456;
        (*items[1]).next = items[0];
    }

    let mut item: *mut Item<_> = items[0];
    unsafe {
        for _ in 0..10 {
            let value = (*item).value;
            println!("{value}");
            item = (*item).next;
            (*item).value += value;
        }
    }
}
