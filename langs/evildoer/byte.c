#include <stdio.h>
#include <inttypes.h>
#include "types.h"

FILE* in;
FILE* out;

int64_t read_byte(void) {
  char c = getc(in ? in : stdin);
  return (c == EOF) ?
    val_eof :
    (int64_t)(c << int_shift);
}

int64_t write_byte(int64_t c) {
  int64_t codepoint = c >> int_shift;
  putc((char) codepoint, out ? out : stdout);
  return 0;
}

void set_io(const char* f_in, const char* f_out) {
  in = fopen(f_in, "r");
  out = fopen(f_out, "w");
}

void close_io() {
  fflush(out);
  fclose(in);
  fclose(out);
}
