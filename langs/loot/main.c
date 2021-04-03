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
  
int main(int argc, char** argv) {
  void * heap = malloc(heap_size);
  int64_t result = entry(heap);
  // See if we need to print the initial tick
  if (cons_type_tag == (ptr_type_mask & result)) printf("'");
  print_result(result);
  printf("\n");
  return 0;
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
  switch (result_type_mask & v) {
  case type_imm:
    print_immediate(v);
    break;
  case type_box:
    printf("#&");
    print_result (*((int64_t *)(v ^ type_box)));
    break;
  case type_pair:
    printf("(");
    print_pair(v);
    printf(")");
    break;
  case type_string:
    printf("\"");
    print_string(v);
    printf("\"");
    break;
  case type_proc:
    printf("procedure");
    break;
  default:
    internal_error();
  }
}

void print_immediate(int64_t v) {
  switch (imm_type_mask & v) {
  case imm_type_int:
    printf("%" PRId64, v >> imm_shift);
    break;
  case imm_type_bool:
    printf("#%c", v >> imm_shift ? 't' : 'f');
    break;
  case imm_type_empty:
    printf("()");
    break;
  case imm_type_char:
    print_char(v);
  default:
    break;
    internal_error();
  }
}

void print_pair(int64_t v) {
  int64_t car = *((int64_t *)((v + 8) ^ type_pair));
  int64_t cdr = *((int64_t *)((v + 0) ^ type_pair));
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

void print_char (int64_t v) {
  int64_t codepoint = v >> imm_shift;
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

void print_string(int64_t v) {
  int64_t* str = (int64_t *)(v ^ type_string);
  int64_t len = (str[0] >> imm_shift);

  int i;
  for (i = 0; i < len; i++)
    print_string_char(str[i+1]);
}
