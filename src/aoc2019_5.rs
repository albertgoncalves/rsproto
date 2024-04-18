use std::convert::TryInto;
use std::env;
use std::fs;
use std::ops::{Add, Mul};
use std::path::Path;

/* NOTE: See `https://adventofcode.com/2019/day/5`. */

fn tokenize(source: &str) -> Vec<i32> {
    source.split(',').map(|x| x.parse().unwrap()).collect()
}

fn index(program: &[i32], i: usize, n: usize) -> usize {
    let mode: bool = match n {
        1 => (program[i] / 100) % 10 == 0,
        2 => (program[i] / 1000) % 10 == 0,
        3 => (program[i] / 10000) % 10 == 0,
        _ => unreachable!(),
    };
    if mode {
        program[i + n].try_into().unwrap()
    } else {
        i + n
    }
}

macro_rules! bin_op {
    ($program:expr, $i:expr, $fn:expr $(,)?) => {{
        let a: i32 = $program[index($program, $i, 1)];
        let b: i32 = $program[index($program, $i, 2)];
        $program[index($program, $i, 3)] = $fn(a, b);
        $i += 4;
    }};
}

macro_rules! jump {
    ($program:expr, $i:expr, $fn:expr $(,)?) => {{
        if $fn(&$program[index($program, $i, 1)], &0) {
            $i = $program[index($program, $i, 2)] as usize
        } else {
            $i += 3
        }
    }};
}

macro_rules! cond {
    ($program:expr, $i:expr, $fn:expr $(,)?) => {{
        let a: i32 = $program[index($program, $i, 1)];
        let b: i32 = $program[index($program, $i, 2)];
        if $fn(&a, &b) {
            $program[index($program, $i, 3)] = 1
        } else {
            $program[index($program, $i, 3)] = 0
        }
        $i += 4;
    }};
}

fn eval(program: &mut [i32], input: &mut Vec<i32>) -> Option<i32> {
    let mut output: Option<i32> = None;
    let n: usize = program.len();
    let mut i: usize = 0;
    while i < n {
        match program[i] % 100 {
            1 => bin_op!(program, i, i32::add),
            2 => bin_op!(program, i, i32::mul),
            3 => {
                program[index(program, i, 1)] = input.pop().unwrap();
                i += 2;
            }
            4 => {
                let value: i32 = program[index(program, i, 1)];
                output = Some(value);
                println!("{value:?}");
                i += 2;
            }
            5 => jump!(program, i, i32::ne),
            6 => jump!(program, i, i32::eq),
            7 => cond!(program, i, i32::lt),
            8 => cond!(program, i, i32::eq),
            99 => return output,
            _ => panic!(),
        }
    }
    None
}

fn main() {
    let source: String = fs::read_to_string(
        Path::new(&env::var("WD").unwrap())
            .join("data")
            .join("aoc2019_5.txt"),
    )
    .unwrap();
    let mut program: Vec<i32> = tokenize(source.trim_end());
    let _: Option<i32> = eval(&mut program.clone(), &mut vec![1]);
    let _: Option<i32> = eval(&mut program, &mut vec![5]);
}

#[cfg(test)]
mod tests {
    use super::{eval, tokenize};

    macro_rules! test {
        ($test:ident, $source:expr, $input:expr, $result:expr $(,)?) => {
            #[test]
            fn $test() {
                let mut program: Vec<i32> = tokenize($source);
                assert_eq!(eval(&mut program, &mut $input), $result);
            }
        };
    }

    test!(case_1_1, "3,9,8,9,10,9,4,9,99,-1,8", vec![7], Some(0));
    test!(case_1_2, "3,9,8,9,10,9,4,9,99,-1,8", vec![8], Some(1));
    test!(case_1_3, "3,9,8,9,10,9,4,9,99,-1,8", vec![9], Some(0));
    test!(case_2_1, "3,9,7,9,10,9,4,9,99,-1,8", vec![7], Some(1));
    test!(case_2_2, "3,9,7,9,10,9,4,9,99,-1,8", vec![8], Some(0));
    test!(case_2_3, "3,9,7,9,10,9,4,9,99,-1,8", vec![9], Some(0));
    test!(case_3_1, "3,3,1108,-1,8,3,4,3,99", vec![7], Some(0));
    test!(case_3_2, "3,3,1108,-1,8,3,4,3,99", vec![8], Some(1));
    test!(case_3_3, "3,3,1108,-1,8,3,4,3,99", vec![9], Some(0));
    test!(case_4_1, "3,3,1107,-1,8,3,4,3,99", vec![7], Some(1));
    test!(case_4_2, "3,3,1107,-1,8,3,4,3,99", vec![8], Some(0));
    test!(case_4_3, "3,3,1107,-1,8,3,4,3,99", vec![9], Some(0));
}
