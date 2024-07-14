// NOTE: See `https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c`.
// NOTE: See `https://prng.di.unimi.it/`.
// NOTE: See `https://github.com/lemire/testingRNG`.
// NOTE: See `https://stats.stackexchange.com/questions/337927/is-pcg-random-number-generator-as-good-as-claimed`.

struct PcgRng {
    state: u64,
    increment: u64,
}

impl PcgRng {
    #[allow(clippy::unreadable_literal)]
    const fn new() -> Self {
        Self {
            state: 9600629759793949339,
            increment: 15726070495360670683,
        }
    }

    fn seed(&mut self, state: u64, increment: u64) {
        self.state = 0;
        self.increment = increment.wrapping_shl(1) | 1;
        Random::<u32>::uniform(self);
        self.state += state;
        Random::<u32>::uniform(self);
    }
}

trait Random<T> {
    fn uniform(&mut self) -> T;
    fn uniform_bounded(&mut self, bound: T) -> T;
}

impl Random<u32> for PcgRng {
    #[allow(clippy::cast_possible_truncation, clippy::unreadable_literal)]
    fn uniform(&mut self) -> u32 {
        let state = self.state;
        self.state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(self.increment | 1);
        ((state.wrapping_shr(18) ^ state).wrapping_shr(27) as u32)
            .rotate_right(state.wrapping_shr(59) as u32)
    }

    fn uniform_bounded(&mut self, bound: u32) -> u32 {
        let threshold: u32 = bound.wrapping_neg() % bound;
        loop {
            let x = self.uniform();
            if threshold <= x {
                return x % bound;
            }
        }
    }
}

fn main() {
    let mut rng = PcgRng::new();
    println!("{}", rng.uniform());
    println!("{}", rng.uniform());
    println!("{}", rng.uniform_bounded(123));
}
