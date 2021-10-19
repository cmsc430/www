#include <stdio.h>
#include <inttypes.h>
#include "values.h"

void print_char(val_char_t);
void print_codepoint(val_char_t);
int utf8_encode_char(val_char_t, char *);

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
  case T_EOF:
    printf("#<eof>");
    break;
  case T_VOID:
    break;    
  case T_INVALID:
    printf("internal error");
  }
}

void print_char(val_char_t c)
{
  printf("#\\");
  switch (c) {
  case 0:
    printf("nul"); break;
  case 8:
    printf("backspace"); break;
  case 9:
    printf("tab"); break;
  case 10:
    printf("newline"); break;
  case 11:
    printf("vtab"); break;
  case 12:
    printf("page"); break;
  case 13:
    printf("return"); break;
  case 32:
    printf("space"); break;
  case 127:
    printf("rubout"); break;
  default:
    print_codepoint(c);
  }
}

void print_codepoint(val_char_t c)
{
  static char buffer[5] = {0};
  utf8_encode_char(c, buffer);
  printf("%s", buffer);
}

int utf8_encode_char(val_char_t c, char *buffer)
{
  // Output to buffer using UTF-8 encoding of codepoint
  // https://en.wikipedia.org/wiki/UTF-8
  if (c < 128) {
    buffer[0] = (char) c;
    return 1;
  } else if (c < 2048) {
    buffer[0] =  (char)(c >> 6)       | 192;
    buffer[1] = ((char)       c & 63) | 128;
    return 2;
  } else if (c < 65536) {
    buffer[0] =  (char)(c >> 12)      | 224;
    buffer[1] = ((char)(c >> 6) & 63) | 128;
    buffer[2] = ((char)       c & 63) | 128;
    return 3;
  } else {
    buffer[0] =  (char)(c >> 18)       | 240;
    buffer[1] = ((char)(c >> 12) & 63) | 128;
    buffer[2] = ((char)(c >>  6) & 63) | 128;
    buffer[3] = ((char)        c & 63) | 128;
    return 4;
  }
}

