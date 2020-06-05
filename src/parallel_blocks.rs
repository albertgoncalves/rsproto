use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

const BUFFER_WIDTH: usize = 20;
const BUFFER_HEIGHT: usize = 20;
const BUFFER_CAP: usize = BUFFER_WIDTH * BUFFER_HEIGHT;

const N: usize = 3;
const BLOCK_WIDTH: usize = BUFFER_WIDTH / N;
const BLOCK_HEIGHT: usize = BUFFER_HEIGHT / N;
const X_BLOCKS: usize = (BUFFER_WIDTH + BLOCK_WIDTH - 1) / BLOCK_WIDTH;
const Y_BLOCKS: usize = (BUFFER_HEIGHT + BLOCK_HEIGHT - 1) / BLOCK_HEIGHT;
const BLOCKS_CAP: usize = X_BLOCKS * Y_BLOCKS;

const THREAD_CAP: usize = 3;

#[derive(Clone, Copy, Debug)]
struct XY {
    x: usize,
    y: usize,
}

#[derive(Clone, Copy, Debug)]
struct Work {
    start: XY,
    end: XY,
}

static INDEX: AtomicUsize = AtomicUsize::new(0);

fn set_buffer(buffer: &mut [u8], works: &[Work]) {
    loop {
        let index: usize = INDEX.fetch_add(1, Ordering::SeqCst);
        if BLOCKS_CAP <= index {
            return;
        }
        let work: Work = works[index];
        for y in work.start.y..work.end.y {
            let offset: usize = y * BUFFER_WIDTH;
            for x in work.start.x..work.end.x {
                buffer[x + offset] = index as u8;
            }
        }
        thread::sleep(Duration::from_millis(100))
    }
}

fn main() {
    let mut buffer: [u8; BUFFER_CAP] = [0; BUFFER_CAP];
    let mut works: Vec<Work> = Vec::with_capacity(BLOCKS_CAP);
    for y in 0..Y_BLOCKS {
        for x in 0..X_BLOCKS {
            let start: XY = XY {
                x: x * BLOCK_WIDTH,
                y: y * BLOCK_HEIGHT,
            };
            works.push(Work {
                start,
                end: XY {
                    x: (start.x + BLOCK_WIDTH).min(BUFFER_WIDTH),
                    y: (start.y + BLOCK_HEIGHT).min(BUFFER_HEIGHT),
                },
            });
        }
    }
    let mut handles: Vec<JoinHandle<()>> = Vec::with_capacity(THREAD_CAP);
    for _ in 0..THREAD_CAP {
        /* NOTE: This is a hack merely to get around the restrictions of
         * `*mut` and `*const`. Neither implements `Sync` or `Send`, so the
         * compiler will complain when we try and toss them into
         * `thread::spawn`. Whatever, man.
         */
        let mut buffer: AtomicPtr<[u8; BUFFER_CAP]> =
            AtomicPtr::new(&mut buffer);
        let mut works: AtomicPtr<Vec<Work>> = AtomicPtr::new(&mut works);
        unsafe {
            handles.push(thread::spawn(move || {
                set_buffer(&mut **buffer.get_mut(), &**works.get_mut());
            }));
        }
    }
    for handle in handles {
        handle.join().unwrap();
    }
    for y in 0..BUFFER_HEIGHT {
        let offset: usize = y * BUFFER_WIDTH;
        for x in 0..BUFFER_WIDTH {
            print!("{:>3}", buffer[x + offset]);
        }
        println!();
    }
}