#![warn(missing_docs)]
//! # Advanced Lyrics File
//!
//! What does this add to the `lrc` base format ?
//!
//! * Named Markers
//! * Vocals
//! * Instrumental Line Check
//! * Custom Tags
//!
//! Those new additions don’t by any mean break previous specification of the
//! format. The compatibility with `A2 extension` is preserved and is even
//! supported in the vocals. Chained timestamps are also supported.
//!
//! ## Named Markers
//!
//! Those aim to provide a context for the lyric line.
//! An use case can be for the singer that sing the line
//!
//! **Named Marker Example**
//!
//! ```lrc
//! {@singer:The Name Of The Singer}
//! ```
//!
//! The marker will be represented with the key `singer` and
//! value `The Name Of The Singer`
//!
//! Each time the a new Markers is encounter by the parser the current marker
//! value is changed.
//!
//! **Named Marker Change Example**
//!
//! ```lrc
//! {@singer:Childish Gambino}
//!
//! [00:15.84] Cody LaRae
//! [00:19.30] He had a break
//! [00:22.75] He's findin' out
//! [00:25.92] That nobody gives a fuck
//! [00:29.68] I did my job
//! [00:32.91] I paid my dues
//! [00:36.13] Love is for fools
//! [00:39.33] 'Cause nobody gives a fuck
//!
//! {@singer:VOCALS}
//!
//! [00:45.54] (No one, no one)
//!
//! ...
//! ```
//!
//! ## Vocals
//!
//! Vocals are background voices other than the main artist voice.
//! Most of the times, they are represented between parenthesis to signify
//! that they are not importants.
//!
//! **Example of vocals**
//!
//! ```lrc
//! ...
//!
//! [00:52.71] {#vocal:No one} nobody gives a fuck
//! ...
//! ```
//!
//! A single single line can have multiples vocals
//!
//! ## Instrumental Line Check
//!
//! Any line that contains only `#INSTRUMENTAL` is consider as a line where
//! there is a long instrumental pause.
//!
//! ```lrc
//! [00:50.07] #INSTRUMENTAL
//! ```
//!
//! ## Custom Tags
//!
//! Now with this parser, tags doesn’t matter to it. Therfore, you can create any
//! tag that suite your needs.

#![doc(issue_tracker_base_url = "https://github.com/luxluth/alrc/issues/")]

use lazy_static::lazy_static;
use nom::{
    character::complete::{char, digit1},
    combinator::{map_res, opt},
    sequence::{preceded, tuple},
    IResult,
};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, time::Duration, usize};

lazy_static! {
    static ref VOCAL_RE: regex::Regex = regex::Regex::new(r"\{#vocal:(.*?)\}").unwrap();
}

/// HashMap that store key value of and attribute
pub type Metadata = HashMap<String, String>;

/// Marker are used to denote a line
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone)]
pub enum Marker {
    /// Named marker
    Named(String, String),
    /// Empty marker
    Empty,
}

impl std::fmt::Display for Marker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Marker::Named(key, value) => {
                write!(f, "[MARKER:Named({})] {}", key, value)
            }
            Marker::Empty => {
                write!(f, "[MARKER:Empty]")
            }
        }
    }
}

/// A chunk of the base text that is timed
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Syllable {
    /// Syllable text
    pub text: String,
    /// The time at the syllable is pronouunced
    pub start: RawTimestamp,
}

fn make_time(arr: &mut impl Iterator<Item = char>) -> RawTimestamp {
    let mut text = String::new();
    for c in arr {
        if c == '>' {
            text.push('>');
            break;
        } else {
            text.push(c);
        }
    }

    RawTimestamp::parse(&text, '<', '>').unwrap().1
}

impl Syllable {
    fn parse_many(input: &str, start_time: RawTimestamp) -> Vec<Self> {
        let mut text_iter = input.chars().peekable();
        let mut current_text = String::new();
        let mut syllables: Vec<Syllable> = vec![];
        let mut current_time = start_time;

        while let Some(character) = text_iter.peek() {
            if *character == '<' {
                if !current_text.is_empty() {
                    syllables.push(Syllable {
                        text: current_text,
                        start: current_time,
                    });
                    current_text = String::new();
                }
                current_time = make_time(&mut text_iter);
            } else {
                current_text.push(text_iter.next().unwrap());
            }
        }

        if !current_text.is_empty() {
            syllables.push(Syllable {
                text: current_text,
                start: current_time,
            });
        }

        syllables
    }
}

trait ToString {
    fn to_string(&self) -> String;
}

impl ToString for Vec<Syllable> {
    fn to_string(&self) -> String {
        let mut out = String::new();
        for s in self {
            out.push_str(&s.text);
        }
        out
    }
}

impl Vocal {
    fn parse(input: &str, start_time: RawTimestamp) -> Self {
        let syllables = Syllable::parse_many(input, start_time);
        Self {
            text: syllables.to_string(),
            time: start_time,
            syllables,
        }
    }
}

/// Vocal Container
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Vocal {
    /// Global vocal text
    pub text: String,
    /// Time at which start the vocals
    pub time: RawTimestamp,
    /// syllables from text
    pub syllables: Vec<Syllable>,
}

/// Vocal Container
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Line {
    /// [Marker]
    pub marker: Marker,
    /// The [Syllable] array
    pub syllables: Vec<Syllable>,
    /// The text representing the line
    pub text: String,
    /// Lyric line time start
    pub time: RawTimestamp,
    /// The [Vocal] array
    pub vocals: Vec<Vocal>,
    /// Line location
    pub ln: usize,
    /// Determine if a line is instrumental
    pub is_instrumental: bool,
}

/// Parsed lyrics conatiner
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct AdvancedLrc {
    /// Lyrics [Metadata]
    pub metadata: Metadata,
    /// Parsed [Line] array
    pub lines: Vec<Line>,
}

impl AdvancedLrc {
    /// Parse lrc text
    pub fn parse(input: &str) -> Result<Self, String> {
        let lines: Vec<&str> = input.lines().collect();
        let mut meta = Metadata::new();
        let mut parsed_lines: Vec<Line> = vec![];
        let mut current_marker = Marker::Empty;
        let mut ln: usize = 1;

        for line in lines {
            let line = line.trim();
            if line.is_empty() {
                ln += 1;
                continue;
            }
            if line.starts_with('[') && line.ends_with(']') {
                let (key, value) = line
                    .trim_start_matches('[')
                    .trim_end_matches(']')
                    .split_once(':')
                    .unwrap();
                if key.chars().next().unwrap().is_ascii_digit() {
                    parsed_lines.extend(AdvancedLrc::parse_line(line, current_marker.clone(), ln)?);
                } else {
                    meta.insert(key.to_string(), value.to_string());
                }
            } else if line.starts_with('{') && line.ends_with('}') {
                let (marker, value) = line
                    .trim_start_matches('{')
                    .trim_end_matches('}')
                    .split_once(':')
                    .unwrap();
                if marker.starts_with('@') {
                    current_marker =
                        Marker::Named(marker.replace('@', "").to_string(), value.to_string());
                }
            } else if line.starts_with('#') {
                ln += 1;
                continue;
            } else {
                parsed_lines.extend(AdvancedLrc::parse_line(line, current_marker.clone(), ln)?);
            }

            ln += 1;
        }

        Ok(Self {
            metadata: meta,
            lines: parsed_lines,
        })
    }

    fn parse_line(text: &str, current_marker: Marker, ln: usize) -> Result<Vec<Line>, String> {
        let (times, remaining) = RawTimestamp::parse_many(text, '[', ']');

        let mut lines: Vec<Line> = Vec::with_capacity(times.len());

        for time in times {
            let mut text = remaining.trim().to_string();
            let mut vocals: Vec<Vocal> = vec![];
            let text_clone = text.clone();
            for cap in VOCAL_RE.captures_iter(&text_clone) {
                let vocal_text = cap.get(1).unwrap().as_str();
                text = text
                    .replace(&format!("{{#vocal:{vocal_text}}}"), "")
                    .trim()
                    .to_string();
                vocals.push(Vocal::parse(vocal_text, time));
            }

            let syllables = Syllable::parse_many(&text, time);
            let mut is_instrumental = false;
            if text == String::from("#INSTRUMENTAL") {
                is_instrumental = true;
            };

            lines.push(Line {
                marker: current_marker.clone(),
                syllables,
                text,
                time,
                vocals,
                ln,
                is_instrumental,
            });
        }

        Ok(lines)
    }
}

/// Simple Timestamp
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct RawTimestamp {
    /// Timestamp minutes
    pub minutes: u8,
    /// Timestamp seconds
    pub seconds: u8,
    /// Timestamp millis
    pub millis: Option<u8>,
}

impl RawTimestamp {
    fn parse(input: &str, start: char, end: char) -> IResult<&str, RawTimestamp> {
        let (input, (_, minutes, _, seconds, millis, _)) = tuple((
            char(start),
            parse_minutes,
            char(':'),
            parse_seconds,
            parse_millis,
            char(end),
        ))(input)?;

        Ok((
            input,
            RawTimestamp {
                minutes,
                seconds,
                millis,
            },
        ))
    }

    /// Convert timestamp to standard duration
    pub fn to_duration(&self) -> Duration {
        Duration::from_millis(
            (((self.minutes as u64) * 60 + self.seconds as u64) * 1000)
                + self.millis.unwrap_or(0) as u64,
        )
    }

    fn parse_many(input: &str, start: char, end: char) -> (Vec<RawTimestamp>, &str) {
        let mut remaining = input;
        let mut timestamps = Vec::new();

        while let Ok((new_remaining, timestamp)) = RawTimestamp::parse(remaining, start, end) {
            timestamps.push(timestamp);
            remaining = new_remaining;
        }

        (timestamps, remaining)
    }
}

impl std::fmt::Display for RawTimestamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}ms", self.to_duration().as_millis())
    }
}

fn parse_u8(input: &str) -> IResult<&str, u8> {
    map_res(digit1, str::parse)(input)
}
fn parse_minutes(input: &str) -> IResult<&str, u8> {
    parse_u8(input)
}
fn parse_seconds(input: &str) -> IResult<&str, u8> {
    parse_u8(input)
}
fn parse_millis(input: &str) -> IResult<&str, Option<u8>> {
    opt(preceded(char('.'), parse_u8))(input)
}
