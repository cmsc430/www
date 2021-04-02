#![crate_type = "staticlib"]
#![feature(start)] // requires nightly :(

mod types;
use types::*;

fn print_char(v: i64) {
  let codepoint : i64 = v >> CHAR_SHIFT;
  print!("#\\");
  match (codepoint) {
    0   => print!("nul"),
    8   => print!("backspace"),
    9   => print!("tab"),
    10  => print!("newline"),
    11  => print!("vtab"),
    12  => print!("page"),
    13  => print!("return"),
    32  => print!("space"),
    127 => print!("rubout"),
    _   => print_codepoint(v),
  };
}

fn print_codepoint(v: i64) {
  let codepoint : i64 = v >> CHAR_SHIFT;
  // Print using UTF-8 encoding of codepoint
  // https://en.wikipedia.org/wiki/UTF-8
  if codepoint < 128 {
    print!("{}", (char) codepoint);
  } else if (codepoint < 2048) {
    printf("%c%c",
	   (char)(codepoint >> 6) | 192,
	   ((char)codepoint & 63) | 128);
  } else if (codepoint < 65536) {
    printf("%c%c%c",
	   (char)(codepoint >> 12) | 224,
	   ((char)(codepoint >> 6) & 63) | 128,
	   ((char)codepoint & 63) | 128);
  } else {
    printf("%c%c%c%c",
	   (char)(codepoint >> 18) | 240,
	   ((char)(codepoint >> 12) & 63) | 128,
	   ((char)(codepoint >> 6) & 63) | 128,
	   ((char)codepoint & 63) | 128);
  }
}

