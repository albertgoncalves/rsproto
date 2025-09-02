#[derive(Clone, Copy, PartialEq, Debug)]
enum Reg {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Dst {
    Reg(Reg),
    StackAddr(usize),
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Src {
    Dst(Dst),
    Int(i64),
}

#[derive(PartialEq, Debug)]
enum Inst {
    Mov(Dst, Src),
    StackPush(Src),
    StackPop(Dst),
}

#[derive(Debug, Default)]
struct State {
    alive: Vec<Dst>,
    insts: Vec<Inst>,
}

const CALLER_SAVED: [Reg; 9] = [
    Reg::Rax,
    Reg::Rdi,
    Reg::Rdx,
    Reg::Rsi,
    Reg::Rcx,
    Reg::R8,
    Reg::R9,
    Reg::R10,
    Reg::R11,
];
const ARGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];
const RETS: [Reg; 2] = [Reg::Rax, Reg::Rdx];

impl State {
    fn spill(&mut self, reg: Reg) {
        if !self.alive.contains(&Dst::Reg(reg)) {
            return;
        }

        let mut found = 0;
        for dst in &mut self.alive {
            match dst {
                Dst::StackAddr(i) => {
                    *i += 1;
                }
                Dst::Reg(other) if reg == *other => {
                    *dst = Dst::StackAddr(0);
                    found += 1;
                }
                Dst::Reg(_) => {}
            }
        }
        assert!(found == 1);

        self.insts.push(Inst::StackPush(Src::Dst(Dst::Reg(reg))));
    }

    fn available(&self, regs: &[Reg]) -> Option<Reg> {
        for reg in regs {
            if !self.alive.contains(&Dst::Reg(*reg)) {
                return Some(*reg);
            }
        }
        None
    }

    fn oldest(&self) -> Option<Reg> {
        for dst in &self.alive {
            if let Dst::Reg(reg) = dst {
                return Some(*reg);
            }
        }
        None
    }

    fn alloc(&mut self, regs: &[Reg], src: Src) {
        assert!(!regs.is_empty());
        let dst = Dst::Reg(self.available(regs).map_or_else(
            || {
                let reg = self.oldest().unwrap();
                self.spill(reg);
                reg
            },
            |reg| reg,
        ));
        self.alive.push(dst);
        self.insts.push(Inst::Mov(dst, src));
    }

    fn swap(&mut self, i: usize) {
        let j = self.alive.len() - 1;
        let i = j - i;
        self.alive.swap(i, j);
    }

    fn drop(&mut self, n: usize) {
        self.alive.truncate(self.alive.len() - n);
    }

    fn call(&mut self, args: usize, rets: usize) {
        for i in 0..args {
            let j = self.alive.len() - (i + 1);

            let reg = ARGS[args - (i + 1)];
            let dst = Dst::Reg(reg);

            if self.alive[j] == dst {
                continue;
            }

            self.spill(reg);

            self.insts.push(Inst::Mov(dst, Src::Dst(self.alive[j])));
            self.alive[j] = dst;
        }

        self.drop(args);
        for reg in &CALLER_SAVED {
            self.spill(*reg);
        }

        for reg in &RETS[..rets] {
            self.alive.push(Dst::Reg(*reg));
        }
    }
}

fn main() {
    let mut state = State::default();
    state.alloc(&CALLER_SAVED, Src::Int(0));
    state.alloc(&CALLER_SAVED, Src::Int(1));
    state.alloc(&CALLER_SAVED, Src::Dst(state.alive[0]));
    state.alloc(&CALLER_SAVED, Src::Dst(state.alive[1]));

    state.call(2, 1);

    for inst in state.insts {
        println!("{inst:?}");
    }
    println!("\n{:?}", state.alive);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn spill_0() {
        let mut state = State::default();

        for reg in &CALLER_SAVED[..3] {
            state.alive.push(Dst::Reg(*reg));
        }
        for reg in &ARGS[..3] {
            state.spill(*reg);
        }

        assert_eq!(state.alive, vec![Dst::Reg(Reg::Rax), Dst::StackAddr(1), Dst::StackAddr(0)]);
        assert_eq!(
            state.insts,
            vec![
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rdi))),
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rdx))),
            ],
        );
    }

    #[test]
    fn alloc_0() {
        let mut state = State::default();

        let regs = [Reg::Rax];
        state.alloc(&regs, Src::Int(0));
        state.alloc(&regs, Src::Int(1));
        state.alloc(&regs, Src::Int(2));

        assert_eq!(state.alive, vec![Dst::StackAddr(1), Dst::StackAddr(0), Dst::Reg(Reg::Rax)]);
        assert_eq!(
            state.insts,
            vec![
                Inst::Mov(Dst::Reg(Reg::Rax), Src::Int(0)),
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rax))),
                Inst::Mov(Dst::Reg(Reg::Rax), Src::Int(1)),
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rax))),
                Inst::Mov(Dst::Reg(Reg::Rax), Src::Int(2)),
            ],
        );
    }

    #[test]
    fn call_0() {
        let mut state = State::default();
        state.call(0, 0);
        assert!(state.alive.is_empty());
        assert!(state.insts.is_empty());
    }

    #[test]
    fn call_1() {
        let mut state = State::default();

        for reg in &CALLER_SAVED[..3] {
            state.alive.push(Dst::Reg(*reg));
        }
        state.call(1, 1);

        assert_eq!(state.alive, vec![Dst::StackAddr(0), Dst::StackAddr(1), Dst::Reg(Reg::Rax)]);
        assert_eq!(
            state.insts,
            vec![
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rdi))),
                Inst::Mov(Dst::Reg(Reg::Rdi), Src::Dst(Dst::Reg(Reg::Rdx))),
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rax))),
            ],
        );
    }

    #[test]
    fn call_2() {
        let mut state = State::default();

        state.alloc(&CALLER_SAVED, Src::Int(0));
        state.alloc(&CALLER_SAVED, Src::Int(1));
        state.alloc(&CALLER_SAVED, Src::Dst(state.alive[0]));
        state.alloc(&CALLER_SAVED, Src::Dst(state.alive[1]));
        state.call(2, 1);

        assert_eq!(state.alive, vec![Dst::StackAddr(0), Dst::StackAddr(1), Dst::Reg(Reg::Rax)]);
        assert_eq!(
            state.insts,
            vec![
                Inst::Mov(Dst::Reg(Reg::Rax), Src::Int(0)),
                Inst::Mov(Dst::Reg(Reg::Rdi), Src::Int(1)),
                Inst::Mov(Dst::Reg(Reg::Rdx), Src::Dst(Dst::Reg(Reg::Rax))),
                Inst::Mov(Dst::Reg(Reg::Rsi), Src::Dst(Dst::Reg(Reg::Rdi))),
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rdi))),
                Inst::Mov(Dst::Reg(Reg::Rdi), Src::Dst(Dst::Reg(Reg::Rdx))),
                Inst::StackPush(Src::Dst(Dst::Reg(Reg::Rax))),
            ],
        );
    }

    #[test]
    fn call_3() {
        let mut state = State::default();
        state.call(0, 2);
        assert_eq!(state.alive, vec![Dst::Reg(Reg::Rax), Dst::Reg(Reg::Rdx)]);
        assert!(state.insts.is_empty());
    }
}
