#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "types.h"

// in bytes
#define heap_size 1000000

int64_t entry(void *);
void print_result(int64_t);
void print_pair(int64_t);
void print_immediate(int64_t);
void print_char(int64_t);
void print_string(int64_t);
void print_string_char(int64_t);
void print_codepoint(int64_t);


int64_t read_byte(void);

int main(int argc, char** argv) {
  void * heap = malloc(heap_size);
  int64_t result = entry(heap);
  print_result(result);
  // read_byte();
  printf("\n");
  return 0;
}

int64_t read_byte(void) {
  int64_t c = (int64_t)getc(stdin);  
  int64_t r = (c << imm_shift);
  print_result(r);
  return r;
}

void error() {
  printf("err");
  exit(1);
}

void internal_error() {
  printf("internal-error");
  exit(1);
}

void print_result(int64_t v) {
  switch (imm_type_mask & v) {
  case imm_type_tag:
    print_immediate(v);
    break;
  case box_type_tag:
    printf("#&");
    print_result (*((int64_t *)(v ^ box_type_tag)));
    break;
  case pair_type_tag:
    printf("(");
    print_pair(v);
    printf(")");
    break;
  default:
    internal_error();
  }
}

void print_immediate(int64_t v) {
  switch (int_type_mask & v) {
  case int_type_tag:
    printf("%" PRId64, v >> int_shift);
    break;    
  default:
    switch (char_type_mask & v) {
    case char_type_tag:
      print_char(v);
      break;
    default:
      switch (v) {
      case val_true:
	printf("#t");
	break;
      case val_false:
	printf("#f");
	break;
      case val_empty:
	printf("()");
	break;
      default:
	internal_error();
	break;
      }
    }
  }
}

void print_pair(int64_t v) {
  int64_t car = *((int64_t *)((v + 8) ^ pair_type_tag));
  int64_t cdr = *((int64_t *)((v + 0) ^ pair_type_tag));
  print_result(car);
  if (cdr == val_empty) {
    // nothing
  } else if ((imm_type_mask & cdr) == pair_type_tag) {
    printf(" ");
    print_pair(cdr);
  } else {
    printf(" . ");
    print_result(cdr);
  }
}

void print_char (int64_t v) {
  int64_t codepoint = v >> char_shift;
  printf("#\\");
  switch (codepoint) {
  case 0:
    printf("nul"); break;
  case 8:
    printf("backspace"); break;
  case 9:
    printf("tab"); break;
  case 10:
    printf("newline"); break;
  case 11:
    printf("vtab"); break;
  case 12:
    printf("page"); break;
  case 13:
    printf("return"); break;
  case 32:
    printf("space"); break;
  case 127:
    printf("rubout"); break;
  default:
    print_codepoint(v);
  }
}
