#include <stdio.h>
#include <inttypes.h>
#include "types.h"
#include "values.h"

val_t entry();
void print_result(val_t);
void print_char(val_char_t);

int main(int argc, char** argv)
{
  val_t result;

  result = entry();
  print_result(entry());
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
  case T_INVALID:
    printf("internal error");
  }
  printf("\n");
}
