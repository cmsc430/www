#include <stdio.h>
#include <inttypes.h>
#include "values.h"

void print_result(val_t x)
{
  switch (val_typeof(x)) {
  case T_INT:
    printf("%" PRId64, val_unwrap_int(x));
    break;
  case T_BOOL:    
    printf(val_unwrap_bool(x) ? "#t" : "#f");
    break;
  case T_INVALID:
    printf("internal error");
  }
}
