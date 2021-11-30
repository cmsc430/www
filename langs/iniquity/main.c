#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();
val_t *heap;

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

  val_t result;

  result = entry(heap);

  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  free(heap);
  return 0;
}

const char* val_typeof_string(int64_t t) {
  switch (val_typeof(t)) {
  case T_INT: return "INT";
  case T_BOOL: return "BOOL";
  case T_CHAR: return "CHAR";
  case T_EOF: return "EOF";
  case T_VOID: return "VOID";
  case T_EMPTY: return "EMPTY";
  case T_BOX: return "BOX";
  case T_CONS: return "CONS";
  case T_VECT: return "VECT";
  case T_STR: return "STR";
  default: return "UNKNOWN";
  }
}



void print_memory(int64_t* rsp, int64_t rbp, int64_t rbx) {
  int stack_count = (rbp-(int64_t)rsp) / 8;
  int heap_count = (rbx-(int64_t)heap) / 8;

  printf("----------------------------------------------------------------\n");
  //  printf("rsp: %" PRIx64 ", rbp: %" PRIx64 ", stack count: %d, heap count: %d\n", (int64_t)rsp, rbp, stack_count, heap_count);
  int i;

  printf("STACK:\n");
  for (i = 0; i < stack_count; i++) {
    printf("[%" PRIx64 "] = %016" PRIx64 ", %s\n", (int64_t)rsp + i, rsp[i], val_typeof_string(rsp[i]));
  }
  printf("HEAP:\n");
  for (i = 0; i < heap_count; i++) {
    printf("[%" PRIx64 "] = %016" PRIx64 ", %s\n", (int64_t)heap + i, heap[i], val_typeof_string(heap[i]));
  }
}
