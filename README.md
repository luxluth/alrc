# Advanced Lyrics File

What does this add to the `lrc` base format ?

- Named Markers
- Vocals
- Instrumental Line Check
- Custom Tags

Those new additions don’t by any mean break previous specification of the
format. The compatibility with `A2 extension` is preserved and is even
supported in the vocals. Chained timestamps are also supported.

## Installation

```bash
cargo add alrc
```

> Use the `serde` feature to add support for serde serialization and deserialization


## Named Markers

Those aim to provide a context for the lyric line.
An use case can be for the singer that sing the line

**Named Marker Example**

```lrc
{@singer:The Name Of The Singer}
```

The marker will be represented with the key `singer` and
value `The Name Of The Singer`

Each time a new Markers is encounter by the parser the current marker
value is changed.

**Named Marker Change Example**

```lrc
{@singer:Childish Gambino}

[00:15.84] Cody LaRae
[00:19.30] He had a break
[00:22.75] He's findin' out
[00:25.92] That nobody gives a fuck
[00:29.68] I did my job
[00:32.91] I paid my dues
[00:36.13] Love is for fools
[00:39.33] 'Cause nobody gives a fuck

{@singer:VOCALS}

[00:45.54] (No one, no one)

...
```

## Vocals

Vocals are background voices other than the main artist voice.
Most of the times, they are represented between parenthesis to signify
that they are not important.

**Example of vocals**

```lrc
...

[00:52.71] {#vocal:No one} nobody gives a fuck
...
```

A single single line can have multiples vocals

## Instrumental Line Check

Any line that contains only `#INSTRUMENTAL` is consider as a line where
there is a long instrumental pause.

```lrc
[00:50.07] #INSTRUMENTAL
```

## Custom Tags

Now with this parser, tags does not matter to it. Therefore, you can create any
tag that suite your needs.
