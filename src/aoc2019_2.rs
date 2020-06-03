use std::env;
use std::fs;
use std::ops::{Add, Mul};
use std::path::Path;

/* NOTE: See `https://adventofcode.com/2019/day/2`. */

fn tokenize(source: &str) -> Vec<u32> {
    source.split(",").map(|x| x.parse().unwrap()).collect()
}

enum OpCode {
    Add,
    Mul,
    Halt,
    Unknown,
}

impl From<u32> for OpCode {
    fn from(orig: u32) -> Self {
        match orig {
            1 => OpCode::Add,
            2 => OpCode::Mul,
            99 => OpCode::Halt,
            _ => OpCode::Unknown,
        }
    }
}

macro_rules! bin_op {
    ($i:expr, $program:expr, $fn:expr $(,)?) => {{
        let a: u32 = $program[$program[$i + 1] as usize];
        let b: u32 = $program[$program[$i + 2] as usize];
        $program[$program[$i + 3] as usize] = $fn(a, b);
        $i += 4;
    }};
}

fn eval(program: &mut [u32]) {
    let n: usize = program.len();
    let mut i: usize = 0;
    while i < n {
        match program[i].into() {
            OpCode::Add => bin_op!(i, program, Add::add),
            OpCode::Mul => bin_op!(i, program, Mul::mul),
            OpCode::Halt => return,
            OpCode::Unknown => panic!(),
        }
    }
}

fn main() {
    let source: String = fs::read_to_string(
        Path::new(&env::var("WD").unwrap())
            .join("data")
            .join("aoc2019_2.txt"),
    )
    .unwrap();
    let program: Vec<u32> = tokenize(&source.trim_end());
    {
        let mut program: Vec<u32> = program.clone();
        program[1] = 12;
        program[2] = 2;
        eval(&mut program);
        println!("{}", program[0]);
    }
    {
        for a in 0..100 {
            for b in 0..100 {
                let mut program: Vec<u32> = program.clone();
                program[1] = a;
                program[2] = b;
                eval(&mut program);
                if program[0] == 19690720 {
                    println!("{}", (100 * a) + b);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{eval, tokenize};

    macro_rules! test {
        ($test:ident, $source:expr, $result:expr $(,)?) => {
            #[test]
            fn $test() {
                let mut program: Vec<u32> = tokenize($source);
                eval(&mut program);
                assert_eq!(program[0], $result);
            }
        };
    }

    test!(case_1, "1,0,0,0,99", 2);
    test!(case_2, "1,1,1,4,99,5,6,0,99", 30);
    test!(case_3, "1,9,10,3,2,3,11,0,99,30,40,50", 3500);
}
