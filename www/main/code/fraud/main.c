#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();

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
  
  val_t result;

  result = entry();
  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  return 0;
}
