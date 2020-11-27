#include <stdio.h>
#include <inttypes.h>
#include "types.h"

void print_codepoint(int64_t);

void print_char (int64_t v) {
  int64_t codepoint = v >> char_shift;
  printf("#\\");
  switch (codepoint) {
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
    print_codepoint(v);
  }
}

void print_codepoint(int64_t v) {
  int64_t codepoint = v >> char_shift;
  // Print using UTF-8 encoding of codepoint
  // https://en.wikipedia.org/wiki/UTF-8
  if (codepoint < 128) {
    printf("%c", (char) codepoint);
  } else if (codepoint < 2048) {
    printf("%c%c",
	   (char)(codepoint >> 6) | 192,
	   ((char)codepoint & 63) | 128);
  } else if (codepoint < 65536) {
    printf("%c%c%c",
	   (char)(codepoint >> 12) | 224,
	   ((char)(codepoint >> 6) & 63) | 128,
	   ((char)codepoint & 63) | 128);
  } else {
    printf("%c%c%c%c",
	   (char)(codepoint >> 18) | 240,
	   ((char)(codepoint >> 12) & 63) | 128,
	   ((char)(codepoint >> 6) & 63) | 128,
	   ((char)codepoint & 63) | 128);
  }
}

