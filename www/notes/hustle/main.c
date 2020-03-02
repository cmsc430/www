#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#define result_shift     3
#define result_type_mask ((1 << result_shift) - 1)
#define type_imm         0
#define type_box         1
#define type_pair        2

#define imm_shift      (3 + result_shift)
#define imm_type_mask  ((1 << imm_shift) - 1)
#define imm_type_int   (0 << result_shift)
#define imm_val_true   (1 << result_shift)
#define imm_val_false  (2 << result_shift)
#define imm_val_empty (3 << result_shift)

// in bytes
#define heap_size 1000000

int64_t entry(void *);
void print_result(int64_t);
void print_pair(int64_t);
void print_immediate(int64_t);

int main(int argc, char** argv) {
  void * heap = malloc(heap_size);
  int64_t result = entry(heap);  
  print_result(result);
  printf("\n");
  free(heap);
  return 0;
}

void error() {
  printf("err");
  exit(1);
}

void internal_error() {
  printf("rts-error");
  exit(1);
}

void print_result(int64_t a) {
  switch (result_type_mask & a) {
  case type_imm:
    print_immediate(a);
    break;
  case type_box:
    printf("#&");
    print_result (*((int64_t *)(a ^ type_box)));
    break;
  case type_pair:
    printf("(");
    print_pair(a);
    printf(")");
    break;
  default:
    internal_error();
  }
}
 
void print_immediate(int64_t a) {
  switch (imm_type_mask & a) {
  case imm_type_int:
    printf("%" PRId64, a >> imm_shift);
    break;
  case imm_val_true:
    printf("#t");
    break;
  case imm_val_false:
    printf("#f");
    break;
  case imm_val_empty:
    printf("()");
    break;
  default:
    break;
    internal_error();    
  }
}

void print_pair(int64_t a) {  
  int64_t car = *((int64_t *)((a + 8) ^ type_pair));
  int64_t cdr = *((int64_t *)((a + 0) ^ type_pair));
  print_result(car);
  if ((imm_type_mask & cdr) == imm_type_empty) {
    // nothing
  } else if ((result_type_mask & cdr) == type_pair) {
    printf(" ");
    print_pair(cdr);
  } else {
    printf(" . ");
    print_result(cdr);
  }
}
