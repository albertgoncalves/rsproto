use std::io;
use std::io::Write;
use std::sync;
use std::thread;
use std::time;

fn main() {
    let parent = sync::Arc::new((sync::Mutex::new(false), sync::Condvar::new()));
    let child = sync::Arc::clone(&parent);

    thread::spawn(move || {
        let (mutex, cvar) = &*child;
        thread::sleep(time::Duration::from_millis(500));
        *mutex.lock().unwrap() = true;
        cvar.notify_one();
    });

    let (mutex, cvar) = &*parent;
    let mut flag = mutex.lock().unwrap();
    let duration = time::Duration::from_millis(100);

    loop {
        print!(".");
        io::stdout().flush().unwrap();

        let result = cvar.wait_timeout(flag, duration).unwrap();
        flag = result.0;
        if *flag {
            println!();
            break;
        }
    }

    println!("Done!");
}
