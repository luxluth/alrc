use alrc::AdvancedLrc;

const INPUT: &str = include_str!("./sample.lrc");

fn main() {
    if let Ok(lyrics) = AdvancedLrc::parse(INPUT) {
        eprintln!("{lyrics:?}");
    }
}
