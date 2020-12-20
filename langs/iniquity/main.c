#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "heap.h"

struct Ret {
  int64_t result;
  int64_t * hp;
};

struct Ret entry(int64_t *);
void print_result(int64_t);
void print_mem(int64_t *, int64_t *);
void collect_garbage(int64_t *, int64_t *, int64_t *);

// allocate 2x heap space; copy collector will copy objects
// between the two halves
int64_t heap[heap_size * 2];
char type[heap_size]; // queue of pointer type tags for GC
int from_side = -1;
// sign indicates in which direction of the midpoint is the current heap

int const true = 1;
int const false = 0;

int main(int argc, char** argv) {
  int show_heap = (argc == 2) && (strcmp(argv[1], "--show-heap") == 0);
 
  struct Ret r = entry(heap);
  print_result(r.result);
  if (r.result != val_void) printf("\n");
  if (show_heap) {
      printf("HEAP:\n");
      print_mem(from_side < 0 ? heap : heap + heap_size, r.hp);
  }
  return 0;
}

void print_char(int64_t);
void print_cons(int64_t);
void print_str(int64_t);

void print_result(int64_t result) {
  if (cons_type_tag == (ptr_type_mask & result)) {
    printf("(");
    print_cons(result);
    printf(")");
  } else if (box_type_tag == (ptr_type_mask & result)) {
    printf("#&");
    print_result (*((int64_t *)(result ^ box_type_tag)));
  } else if (str_type_tag == (ptr_type_mask & result)) {
    printf("\"");
    print_str(result);
    printf("\"");
  } else if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64, result >> int_shift);
  } else if (char_type_tag == (char_type_mask & result)) {
    print_char(result);
  } else {
    switch (result) {
    case val_true:
      printf("#t"); break;
    case val_false:
      printf("#f"); break;
    case val_eof:
      printf("#<eof>"); break;
    case val_empty:
      printf("()"); break;
    case val_void:
      /* nothing */ break;
    }
  }  
}

void print_cons(int64_t a) {  
  int64_t car = *((int64_t *)((a + 8) ^ cons_type_tag));
  int64_t cdr = *((int64_t *)((a + 0) ^ cons_type_tag));
  print_result(car);
  if (cdr == val_empty) {
    // nothing
  } else if (cons_type_tag == (ptr_type_mask & cdr)) {
    printf(" ");
    print_cons(cdr);
  } else {
    printf(" . ");
    print_result(cdr);
  }
}

void error() {
  printf("err\n");
  exit(1);
}
