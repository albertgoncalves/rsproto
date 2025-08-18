use std::env;
use std::fs::File;
use std::io::Error;
use std::io::Write;
use std::mem::size_of;
use std::path::Path;
use std::slice::from_raw_parts;

const CAFEBABE: u32 = 0xCAFE_BABE;

const unsafe fn get_u8_slice<T: Sized>(x: &T) -> &[u8] {
    let x: *const T = x;
    unsafe { from_raw_parts(x.cast::<u8>(), size_of::<T>()) }
}

#[repr(C, packed(2))]
struct T {
    a: u32,
    b: u16,
    c: u16,
    d: u16,
    e: u16,
}

fn main() -> Result<(), Error> {
    let x: T = T {
        a: CAFEBABE.to_be(),
        b: 0x0001_u16.to_be(),
        c: 0x0010_u16.to_be(),
        d: 0x0100_u16.to_be(),
        e: 0x1000_u16.to_be(),
    };
    let bytes: &[u8] = unsafe { get_u8_slice(&x) };
    let mut file: File = File::create(
        Path::new(&env::var("WD").map_err(Error::other)?)
            .join("out")
            .join("struct.bin"),
    )?;
    file.write_all(bytes)?;
    file.write_all(&[0, 0, 0, 0])?;
    file.write_all(b"Hello, world!")
}
