#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

#define fixnum_mask 1
#define fixnum_tag 0
#define fixnum_shift 1

#define boolean_mask 1
#define boolean_tag 1
#define boolean_shift 1

int64_t entry();

int main(int argc, char** argv) {
  int64_t result = entry();
  if ((result & fixnum_mask) == fixnum_tag) {
    printf("%" PRId64 "\n", result >> fixnum_shift);    
  } else if ((result & boolean_mask) == boolean_tag) {
    if (result >> boolean_shift) {
      printf("#t\n");
    } else {
      printf("#f\n");
    }
  }
  
  return 0;
}

void error() {
  printf("err");
  exit(1);
}
