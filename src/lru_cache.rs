/* NOTE: See `https://www.mattkeeter.com/blog/2022-10-04-ssra/`. */

#[derive(Debug)]
struct LruCache<const N: usize> {
    items: [(usize, usize); N],
    top: usize,
}

impl<const N: usize> LruCache<N> {
    fn new() -> Self {
        let mut cache = Self { items: [(0, 0); N], top: 0 };

        cache.items[0].0 = N - 1;
        cache.items[0].1 = 1 % N;

        for i in 1..N {
            cache.items[i].0 = i - 1;
            cache.items[i].1 = (i + 1) % N;
        }

        cache
    }

    const fn poke(&mut self, i: usize) {
        if i == self.top {
            return;
        }

        if self.items[self.top].0 != i {
            {
                let prev = self.items[i].0;
                let next = self.items[i].1;
                self.items[prev].1 = next;
                self.items[next].0 = prev;
            }
            {
                let next = self.top;
                let prev = self.items[next].0;
                self.items[prev].1 = i;
                self.items[next].0 = i;
                self.items[i] = (prev, next);
            }
        }

        self.top = i;
    }

    const fn pop(&mut self) -> usize {
        let prev = self.items[self.top].0;
        self.top = prev;
        prev
    }

    const fn serialize(&self) -> [usize; N] {
        let mut i = self.top;
        let mut j = 0;
        let mut indices = [0; N];

        loop {
            indices[j] = i;
            i = self.items[i].1;

            if i == self.top {
                return indices;
            }

            j += 1;
        }
    }
}

fn main() {
    let mut cache = LruCache::<4>::new();

    cache.poke(1);
    cache.poke(3);
    cache.poke(2);
    cache.poke(0);
    cache.pop();

    println!("{:?}", cache.serialize());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        let cache = LruCache::<4>::new();

        assert_eq!(cache.serialize(), [0, 1, 2, 3]);
    }

    #[test]
    fn poke() {
        let mut cache = LruCache::<4>::new();

        cache.poke(1);
        cache.poke(3);
        cache.poke(2);
        cache.poke(0);
        cache.poke(2);

        assert_eq!(cache.serialize(), [2, 0, 3, 1]);
    }

    #[test]
    fn pop() {
        let mut cache = LruCache::<4>::new();

        cache.poke(2);
        cache.pop();

        assert_eq!(cache.serialize(), [3, 2, 0, 1]);
    }
}
