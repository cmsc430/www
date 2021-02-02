#include <stdio.h>
#include <inttypes.h>
#include "types.h"
#include "runtime.h"

int64_t read_byte(void) {
  char c = getc(in);
  return (c == EOF) ?
    val_eof :
    (int64_t)(c << int_shift);
}

int64_t peek_byte(void) {
  char c = getc(in);
  ungetc(c, in);
  return (c == EOF) ?
    val_eof :
    (int64_t)(c << int_shift);
}

int64_t write_byte(int64_t c) {
  int64_t codepoint = c >> int_shift;
  putc((char) codepoint, out);
  return 0;
}
