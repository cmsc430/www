#include <stdlib.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include "values.h"

int symb_cmp(const val_symb_t *, const val_symb_t *);

// binary tree node
struct Node {
  val_symb_t* elem;
  struct Node* left;
  struct Node* right;
};

static struct Node *symbol_tbl = NULL;

void *my_memcpy(void *dest, const void *src, size_t n) {
  printf("memcpy(%" PRId64 ", %" PRId64 ", %" PRId64 ")", (int64_t)dest, (int64_t)src, (int64_t)n);
  void * r = memcpy(dest, src, n);
  printf(" = %" PRId64 "\n", (int64_t)r);
  return r;
}

val_symb_t *intern_symbol(val_symb_t* symb)
{
  struct Node **curr = &symbol_tbl;

  while (*curr) {
    struct Node *t = *curr;
    int r = symb_cmp(symb, t->elem);
    if (r == 0) {
      // found it, so return saved pointer
      return t->elem;
    } else if (r < 0) {
      curr = &t->left;
    } else {
      curr = &t->right;
    }
  }

  // wasn't found, so insert it and return pointer
  *curr = calloc(1, sizeof(struct Node));
  (*curr)->elem = symb;
  return (*curr)->elem;
}

int symb_cmp(const val_symb_t *s1, const val_symb_t *s2)
{
  int64_t len1 = s1->len;
  int64_t len2 = s2->len;

  int64_t len = len1 < len2 ? len1 : len2;
  int i;

  for (i = 0; i < len; i++) {
    if (s1->codepoints[i] != s2->codepoints[i])
      return s1->codepoints[i] - s2->codepoints[i];
  }

  return len1 - len2;
}
