#![feature(start)] // requires nightly :(

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
//fn main() {
  let res: i64 = unsafe { entry() };
  println!("{}", res);
  return 0;
}
