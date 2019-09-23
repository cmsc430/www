#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

// (define int-tag   #b0000) ;  0, 2, 4... integer
// (define box-tag   #b0001) ;  1 box
// (define pair-tag  #b0011) ;  3 pairs
// (define vect-tag  #b0101) ;  5 vectors
// (define str-tag   #b0111) ;  7 strings
// (define true-tag  #b1001) ;  9
// (define false-tag #b1011) ; 11
// (define empty-tag #b1101) ; 13
// (define char-tag  #b1111) ; 15

#define int_mask     1
#define int_shift    1

#define ptr_mask     7

#define type_int     0
#define type_box     1
#define type_pair    3
#define val_true     9
#define val_false   11
#define val_empty   13

// in bytes
#define heap_size 1000000

int64_t entry(void *);
void print_result(int64_t);

int main(int argc, char** argv) {
  void * heap = malloc(heap_size);
  int64_t result = entry(heap);
  print_result(result);
  printf("\n");
  return 0;
}

void error() {
  printf("err");
  exit(1);
}

void print_result(int64_t a) {
  if ((int_mask & a) == type_int) {
    printf("%" PRId64, a >> int_shift);
  } else if (a == val_true) {
    printf("#t");
  } else if (a == val_false) {
    printf("#f");
  } else if (a == val_empty) {
    printf("())");
  } else if ((ptr_mask & a) == type_box) {
    printf("#&");
    print_result (*((int64_t *)(a ^ type_box)));
  } else if ((ptr_mask & a) == type_pair) {
    printf("(pair)");
  }    
}
