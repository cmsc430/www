#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

#define typeof_mask  1
#define val_shift    1
#define type_fixnum  0
#define type_bool    1

int64_t entry();

int main(int argc, char** argv) {
  int64_t result = entry();
  switch (typeof_mask & result) {
  case type_fixnum:    
    printf("%" PRId64 "\n", result >> val_shift);
    break;
  case type_bool:
    printf("#%c\n", result >> val_shift ? 't' : 'f');
    break;  
  }
  return 0;
}

void error() {
  printf("err\n");
  exit(1);
}
