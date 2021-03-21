#![crate_type = "staticlib"]
#![feature(start)] // requires nightly :(

mod types;
use types::*;

#[derive(Debug)]
enum Val {
  Int(i64),
  Bool(bool),
}

impl Val {
  #[inline(always)]
  fn decode(word: i64) -> Val {
    // Is it an integer?
    if INT_TYPE_TAG == (INT_TYPE_MASK & word) {
      Val::Int(word >> INT_TYPE_MASK as i64)
    } else {
      match word {
        VAL_TRUE  => Val::Bool(true),
        VAL_FALSE => Val::Bool(false),
        _         => print_panic(),
      }
    }
  }
}

#[no_mangle]
fn printresult(res: i64) {
  match Val::decode(res) {
    Val::Int(ires)   => print_int(ires),
    Val::Bool(true)  => print_true(),
    Val::Bool(false) => print_false(),
  }
}


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
  println!("{:?}", Val::decode(res));
  return 0;
}

#[no_mangle]
fn printresult_old(res: i64) {
  // Is it an integer?
  if INT_TYPE_TAG == (INT_TYPE_MASK & res) {
    let ires = res >> INT_TYPE_MASK as i64;
    print_int(ires);
  } else {
    match res {
      VAL_TRUE  => print_true(),
      VAL_FALSE => print_false(),
      _         => { print_panic(); },
    };
  }
}

#[no_mangle]
fn printresult_new(res: i64) {
  match Val::decode(res) {
    Val::Int(ires)   => print_int(ires),
    Val::Bool(true)  => print_true(),
    Val::Bool(false) => print_false(),
  }
}

#[inline(never)]
fn print_int(i: i64) {
  println!("{}", i);
}

#[inline(never)]
fn print_true() {
  println!("#t");
}

#[inline(never)]
fn print_false() {
  println!("#f");
}

#[inline(never)]
fn print_panic() -> Val {
  panic!("PANIC: malformed value")
}
