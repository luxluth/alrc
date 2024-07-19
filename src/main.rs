use std::{env::args, io::Read, process::exit};

fn main() {
    let mut args = args();
    let prog = args.next().expect("No program found! Os level error");
    if let Some(file_path) = args.next() {
        let path = std::path::Path::new(&file_path);
        let mut f = std::fs::File::open(path).unwrap();
        let mut buf = String::new();
        f.read_to_string(&mut buf).unwrap();

        for line in alrc::AdvancedLrc::parse(&buf).unwrap().lines {
            println!("{line:?}");
        }
    } else {
        eprintln!("[ERROR] No lyrics file provided");
        eprintln!("{prog} <lyrics.lrc>");
        exit(0x03)
    }
}
