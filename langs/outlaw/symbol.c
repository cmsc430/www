#include <stdlib.h>
#include <inttypes.h>
#include "values.h"

int symb_cmp(const val_symb_t *, const val_symb_t *);

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
  if (s1 == s2) return 0;

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
