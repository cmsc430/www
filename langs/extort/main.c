#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "types.h"

int64_t entry();
void print_result(int64_t);
void print_char(int64_t);

void error_handler() {
  printf("err\n");
  exit(1);
  printf("got here\n");
}

void (*error)(void);

void init() {
  printf("initing\n");
  error = &error_handler;
}

int main(int argc, char** argv) {
  error = &error_handler;
  //  error();
  print_result(entry());
  return 0;
}

void print_result(int64_t result) {
  if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64 "\n", result >> int_shift);
  } else if (char_type_tag == (char_type_mask & result)) {
    print_char(result);
    printf("\n");
  } else {
    switch (result) {
    case val_true:
      printf("#t\n"); break;
    case val_false:
      printf("#f\n"); break;
    case val_eof:
      printf("#<eof>\n"); break;
    case val_void:
      /* nothing */ break;
    }
  }  
}

