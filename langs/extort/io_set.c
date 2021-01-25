#include <stdio.h>
#include "io.h"

void set_io(const char* f_in, const char* f_out) {
  in = fopen(f_in, "r");
  out = fopen(f_out, "w");
}

void close_io() {
  fflush(out);
  fclose(in);
  fclose(out);
}
