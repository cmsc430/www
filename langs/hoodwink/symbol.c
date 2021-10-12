#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include "values.h"

static uint64_t gensym_ctr = 0;

val_str_t *str_from_cstr(const char *);

val_symb_t *gensym(void)
{
  char s[100]; // uint64_t has maximum 20 digits
  sprintf(s, "g%" PRIu64, gensym_ctr++);
  return (val_symb_t*)str_from_cstr(s); // uninterned symbol
}

val_str_t *str_from_cstr(const char *s)
{
  int64_t len = strlen(s);
  val_str_t *str =
    malloc(sizeof(val_str_t) + len * sizeof(val_char_t));
  
  if (!str)
    return NULL;

  str->len = len;
  int i;
  for (i = 0; i < len; i++) {
    str->codepoints[i] = (val_char_t)s[i];
  }
  return str;
}
