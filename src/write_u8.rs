use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

const BYTES: [u8; 13] = [
    0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21,
];

fn main() {
    let mut file: File =
        File::create(Path::new(&env::var("WD").unwrap()).join("out").join("u8.bin")).unwrap();
    file.write_all(&BYTES).unwrap();
}
