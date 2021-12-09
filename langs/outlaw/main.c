#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)(val_str_t* msg);
val_t *heap;

void error_exit(val_str_t* msg)
{
  if (msg) {
    print_str(msg);
  } else {
    printf("err\n");
  }
  exit(1);
}

void raise_error(val_str_t* msg)
{
  return error_handler(msg);
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
