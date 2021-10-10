#include <stdio.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

FILE* in;
FILE* out;

int main(int argc, char** argv)
{
  in = stdin;
  out = stdout;
  
  val_t result;

  result = entry();
  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  return 0;
}
