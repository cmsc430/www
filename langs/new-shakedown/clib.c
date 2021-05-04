#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "types.h"

int64_t c_fun() {
  puts("Hello, from C!");
  return 8;
  //return (42 << imm_shift);
}

int64_t c_fun_arg(int64_t v) {
  printf("You gave me v = %" PRId64 "\n", v);
  return v * v;
  //return (42 << imm_shift);
}

int64_t c_fun1(int64_t x) {
  printf("You gave me x = %" PRId64 "\n", x);
  int64_t res = x * x;
  return (res << imm_shift);
}
