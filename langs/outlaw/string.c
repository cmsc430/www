#include "values.h"

int string_append(const val_str_t* s1, const val_str_t* s2, val_str_t* dest)
{
  if (!s1 && !s2) { return 0; }
  int i1 = (s1 ? s1->len : 0);
  int i2 = (s2 ? s2->len : 0);
  int len = i1+i2;
  dest->len = len;
  int i;
  if (s1) {
    for (i = 0; i < s1->len; i++)
      dest->codepoints[i] = s1->codepoints[i];
  }
  if (s2) {
    for (i = 0; i < s2->len; i++)
      dest->codepoints[i1 + i] = s2->codepoints[i];
  }
  return 2+len+((len % 2) == 0 ? 0 : 1);
}
