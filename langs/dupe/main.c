#include <stdio.h>
#include <inttypes.h>
#include "types.h"

int64_t entry();
void print_result(int64_t);

int main(int argc, char** argv) {
  print_result(entry());
  return 0;
}

typedef struct {
  int64_t tag;
  int64_t val;
} Val;

Val decode(int64_t word) {
  if (int_type_tag == (int_type_mask & word)) {
    return (Val){0,word >> int_shift};
  } else {
    switch (word) {
    case val_true:
      return (Val){1,1}; break;
    case val_false:
      return (Val){1,0}; break;
    }
  }
}

void print_result_old(int64_t result) {
  if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64 "\n", result >> int_shift);
  } else {
    switch (result) {
    case val_true:
      printf("#t\n"); break;
    case val_false:
      printf("#f\n"); break;
    }
  }
}

void print_result_new(int64_t result) {
  Val d = decode(result);
  switch (d.tag) {
    case 0:
      printf("%" PRId64 "\n", d.val); break;
    case 1:
      if (d.val) {
        printf("#t\n");
      } else {
        printf("#f\n");
      } break;
  }
}
