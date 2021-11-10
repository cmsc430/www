#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include "values.h"

static uint64_t gensym_ctr = 0;

val_str_t *str_from_cstr(const char *);
int str_cmp(const val_str_t *, const val_str_t *);
val_str_t *str_dup(const val_str_t *);

// binary tree node
struct Node {
  val_symb_t* elem;
  struct Node* left;
  struct Node* right;
};

static struct Node *symbol_tbl = NULL;

val_symb_t *intern_symbol(val_symb_t* symb)
{
  struct Node **curr = &symbol_tbl;

  while (*curr) {
    struct Node *t = *curr;
    int r = str_cmp((val_str_t*)symb, (val_str_t*)t->elem);
    if (r == 0) {
      return t->elem;
    } else if (r < 0) {
      curr = &t->left;
    } else {
      curr = &t->right;
    }
  }

  // wasn't found, so insert it

  *curr = calloc(1, sizeof(struct Node));

  struct Node* t = *curr;
  t->elem = symb; // str_dup(str);

  return t->elem;
}

val_symb_t *str_to_symbol(const val_str_t *str)
{
  // str_dup needed if string is mutable
  return intern_symbol((val_symb_t*)str_dup(str));
}

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
    malloc(sizeof(int64_t) + len * sizeof(val_char_t));

  if (!str)
    return NULL;

  str->len = len;
  int i;
  for (i = 0; i < len; i++) {
    str->codepoints[i] = (val_char_t)s[i];
  }
  return str;
}

int str_cmp(const val_str_t *s1, const val_str_t *s2)
{
  int64_t len1 = s1->len;
  int64_t len2 = s2->len;

  int64_t len = len1 < len2 ? len1 : len2;
  int i;

  for (i = 0; i < len; i = i+1) {
    if (s1->codepoints[i] != s2->codepoints[i])
      return s1->codepoints[i] - s2->codepoints[i];
  }

  return len1 - len2;
}

val_str_t *str_dup(const val_str_t *s)
{
  int64_t n = s->len;
  val_str_t *d;

  d = calloc(2+n, sizeof(val_char_t));
  if (!d)
    return NULL;

  return memcpy(d, s, (2+n) * sizeof(val_char_t));
}


#ifdef CHECK
// $ gcc -DCHECK symbol.c
#include <assert.h>
int main(void)
{
  val_str_t *foo = str_from_cstr("foo");
  val_str_t *foo_ = str_from_cstr("foo");
  val_str_t *bar = str_from_cstr("bar");
  val_str_t *foo1 = str_from_cstr("foo1");
  val_str_t *foo1_ = str_from_cstr("foo1");
  val_str_t *fo = str_from_cstr("fo");
  val_str_t *fo_ = str_from_cstr("fo");

  assert(str_cmp(foo,foo_) == 0);
  assert(str_cmp(foo1,foo1_) == 0);
  assert(str_cmp(fo,fo_) == 0);

  assert(str_cmp(foo,foo1) < 0);
  assert(str_cmp(foo,foo1) == str_cmp(foo,foo1_));
  assert(str_cmp(foo,foo1) == str_cmp(foo_,foo1));
  assert(str_cmp(foo,foo1) == str_cmp(foo_,foo1_));

  assert(str_cmp(foo,fo) > 0);
  assert(str_cmp(foo,fo) == str_cmp(foo,fo_));
  assert(str_cmp(foo,fo) == str_cmp(foo_,fo));
  assert(str_cmp(foo,fo) == str_cmp(foo_,fo_));

  assert(str_cmp(foo,bar) > 0);
  assert(str_cmp(bar,foo) < 0);

  assert(str_cmp(fo,bar) > 0);
  assert(str_cmp(bar,fo) < 0);

  val_symb_t *foo_symb = (val_symb_t*)foo;
  val_symb_t *foo_symb_ = (val_symb_t*)foo_;
  assert(foo_symb != foo_symb_);
  val_symb_t *foo_symb_i  = intern_symbol(foo_symb);
  val_symb_t *foo_symb_i_ = intern_symbol(foo_symb_);
  assert(foo_symb_i == foo_symb_i_);

  return 0;
}
#endif
