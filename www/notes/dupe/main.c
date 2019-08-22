#include <stdio.h>
#include <inttypes.h>

#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2

#define boolean_tag 31
#define boolean_mask 127
#define boolean_shift 7

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
  } else {
    printf("unknown value\n");
  }
  return 0;
}
