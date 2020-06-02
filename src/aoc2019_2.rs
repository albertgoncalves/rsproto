use std::ops;

/* NOTE: See `https://adventofcode.com/2019/day/2`. */

fn tokenize(string: &str) -> Vec<u32> {
    string.split(",").map(|x| x.parse().unwrap()).collect()
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

macro_rules! pop_4 {
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
    loop {
        if n <= i {
            return;
        }
        match program[i].into() {
            OpCode::Add => pop_4!(i, program, ops::Add::add),
            OpCode::Mul => pop_4!(i, program, ops::Mul::mul),
            OpCode::Halt => return,
            OpCode::Unknown => panic!(),
        }
    }
}

const SOURCE: &str =
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,5,19,23,1,23,5,27,2,27,10,31,\
     1,5,31,35,2,35,6,39,1,6,39,43,2,13,43,47,2,9,47,51,1,6,51,55,1,55,9,59,2,\
     6,59,63,1,5,63,67,2,67,13,71,1,9,71,75,1,75,9,79,2,79,10,83,1,6,83,87,1,\
     5,87,91,1,6,91,95,1,95,13,99,1,10,99,103,2,6,103,107,1,107,5,111,1,111,\
     13,115,1,115,13,119,1,13,119,123,2,123,13,127,1,127,6,131,1,131,9,135,1,\
     5,135,139,2,139,6,143,2,6,143,147,1,5,147,151,1,151,2,155,1,9,155,0,99,2,\
     14,0,0";

fn main() {
    let program: Vec<u32> = tokenize(SOURCE);
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
        ($test:ident, $string:expr, $result:expr $(,)?) => {
            #[test]
            fn $test() {
                let mut program: Vec<u32> = tokenize($string);
                eval(&mut program);
                assert_eq!(program[0], $result);
            }
        };
    }

    test!(case_1, "1,0,0,0,99", 2);
    test!(case_2, "1,1,1,4,99,5,6,0,99", 30);
    test!(case_3, "1,9,10,3,2,3,11,0,99,30,40,50", 3500);
}
