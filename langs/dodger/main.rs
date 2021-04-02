#![crate_type = "staticlib"]
#![feature(start)] // requires nightly :(

mod types;
use types::*;

extern "C" {
  fn entry() -> i64;
}

// You need both `start` and `no_mangle` it seems.
// `start` tells `rustc` to use this as the entry point for the
// program after the Rust environment does its thing, but `no_mangle`
// is helping the system linker that ultimately connects everything
#[start]
#[no_mangle]
fn main(argc: isize, argv: *const *const u8) -> isize {
  let res: i64 = unsafe { entry() };
  printresult(res);
  return 0;
}

fn printresult(res: i64) {

  // Is it an integer?
  if INT_TYPE_TAG == (INT_TYPE_MASK & res) {
    let ires = res >> INT_TYPE_MASK as i64;
    println!("{}", ires);

  // Is it a character?
  } else if CHAR_TYPE_TAG = (CHAR_TYPE_MASK & res) {
    print_char(res);
  } else {
    match res {
      VAL_TRUE  => println!("#t"),
      VAL_FALSE => println!("#f"),
      _         => println!("PANIC: malformed value"),
    };
  }
}
