#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "runtime.h"

void print_str(val_str_t*);

void error(val_t msg) {
  print_str(val_unwrap_str(msg));
  putchar('\n');
  exit(1);
}
