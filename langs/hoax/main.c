#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "types.h"
#include "values.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();
val_t *heap;

void print_result(val_t);
void print_char(val_char_t);
void print_cons(val_cons_t*);
void print_vect(val_vect_t*);

void error_exit()
{
  printf("err\n");
  exit(1);
}

void raise_error()
{
  return error_handler();
}

int main(int argc, char** argv)
{
  in = stdin;
  out = stdout;
  error_handler = &error_exit;
  heap = malloc(8 * heap_size);

  int64_t result;

  result = entry(heap);

  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  free(heap);
  return 0;
}

void print_result(val_t x)
{
  switch (val_typeof(x)) {
  case T_INT:
    printf("%" PRId64, val_unwrap_int(x));
    break;
  case T_BOOL:
    printf(val_unwrap_bool(x) ? "#t" : "#f");
    break;
  case T_CHAR:
    print_char(val_unwrap_char(x));
    break;
  case T_EOF:
    printf("#<eof>");
    break;
  case T_VOID:
    break;
  case T_EMPTY:
    printf("'()");
    break;
  case T_BOX:
    printf("#&");
    print_result(val_unwrap_box(x)->val);
    break;
  case T_CONS:
    printf("'(");
    print_cons(val_unwrap_cons(x));
    printf(")");
    break;
  case T_VECT:
    print_vect(val_unwrap_vect(x));
    break;
  case T_INVALID:
    printf("internal error");
  }
}

void print_cons(val_cons_t *cons)
{
  print_result(cons->fst);

  switch (val_typeof(cons->snd)) {
  case T_EMPTY:
    // nothing
    break;
  case T_CONS:
    printf(" ");
    print_cons(val_unwrap_cons(cons->snd));
    break;
  default:
    printf(" . ");
    print_result(cons->snd);
    break;
  }
}

void print_vect(val_vect_t *v)
{
  uint64_t i;

  if (!v) { printf("'#()"); return; }
  
  printf("'#(");
  for (i = 0; i < v->len; ++i) {
    print_result(v->elems[i]);

    if (i < v->len - 1)
      putchar(' ');
  }
  printf(")");
}
