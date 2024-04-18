use std::convert::TryFrom;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

const BUFFER_WIDTH: usize = 17;
const BUFFER_HEIGHT: usize = 17;
const BUFFER_CAP: usize = BUFFER_WIDTH * BUFFER_HEIGHT;

const N: usize = 4;
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
struct Block {
    start: XY,
    end: XY,
}

struct ThreadPtr<T>(*const T);

unsafe impl<T> Send for ThreadPtr<T> {}
unsafe impl<T> Sync for ThreadPtr<T> {}

struct ThreadMutPtr<T>(*mut T);

unsafe impl<T> Send for ThreadMutPtr<T> {}
unsafe impl<T> Sync for ThreadMutPtr<T> {}

static INDEX: AtomicUsize = AtomicUsize::new(0);

fn set_buffer(buffer: &mut [u8], block: Block, value: u8) {
    for y in block.start.y..block.end.y {
        let offset: usize = y * BUFFER_WIDTH;
        for x in block.start.x..block.end.x {
            buffer[x + offset] = value;
        }
    }
    thread::sleep(Duration::from_millis(100));
}

fn main() {
    let mut buffer: [u8; BUFFER_CAP] = [0; BUFFER_CAP];
    let mut blocks: Vec<Block> = Vec::with_capacity(BLOCKS_CAP);
    for y in 0..Y_BLOCKS {
        for x in 0..X_BLOCKS {
            let start: XY = XY {
                x: x * BLOCK_WIDTH,
                y: y * BLOCK_HEIGHT,
            };
            blocks.push(Block {
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
        let buffer: ThreadMutPtr<[u8; BUFFER_CAP]> = ThreadMutPtr(&mut buffer);
        let blocks: ThreadPtr<Vec<Block>> = ThreadPtr(&blocks);
        unsafe {
            handles.push(thread::spawn(move || {
                let _ = (&buffer, &blocks);
                let buffer: &mut [u8] = &mut (*buffer.0);
                let blocks: &Vec<Block> = &(*blocks.0);
                loop {
                    let index: usize = INDEX.fetch_add(1, Ordering::SeqCst);
                    if BLOCKS_CAP <= index {
                        return;
                    }
                    set_buffer(buffer, blocks[index], u8::try_from(index).unwrap());
                }
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
