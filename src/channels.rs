use std::sync;
use std::thread;
use std::time;

fn main() {
    let (sender, receiver) = sync::mpsc::channel();

    thread::spawn(move || {
        thread::sleep(time::Duration::from_secs(1));
        sender.send(|| println!("Done!")).unwrap();
    });

    receiver.recv().unwrap()();
}
