#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "values.h"
#include "runtime.h"

const char* val_typeof_string(int64_t t) {
  switch (val_typeof(t)) {
  case T_INT: return "INT";
  case T_BOOL: return "BOOL";
  case T_CHAR: return "CHAR";
  case T_EOF: return "EOF";
  case T_VOID: return "VOID";
  case T_EMPTY: return "EMPTY";
  case T_BOX: return "BOX";
  case T_CONS: return "CONS";
  case T_VECT: return "VECT";
  case T_STR: return "STR";
  default: return "UNKNOWN";
  }
}

void step(val_t* to, val_t* from, val_t** to_curr, val_t** to_next, int count, int* t_back) {
  type_t t;
  int i;
  int size;
  val_t v;
  val_t *ptr_v;
  for (i = 0; i < count; i++) {
    v = **to_curr;
    ptr_v = val_unwrap(v);
    t = val_typeof(v);
    switch (t) {
    case T_BOX:
    case T_CONS:
    case T_VECT:
    case T_STR:
      if (ptr_v >= from && ptr_v < from + heap_size) {
	// this is a pointer to from space so we need to deal with it
	if (val_unwrap(*ptr_v) >= to &&
	    val_unwrap(*ptr_v) < to + heap_size) {
	  // it points to a fwd pointer (points in to to-space), so just set
	  // curr to what it points to.
	  **to_curr = *ptr_v;
	  *to_curr = *to_curr + 1;
	} else {
	  // copy, fwd, update
	  size = val_size(ptr_v, t);
	  types[*t_back] = t;                 // enqueue type
	  *t_back = *t_back + 1;
	  memcpy(*to_next, ptr_v, 8 * size);  // copy
	  *ptr_v = val_wrap(*to_next, t);     // fwd
	  **to_curr = val_wrap(*to_next, t);  // update
	  *to_next = *to_next + size;
	  *to_curr = *to_curr + 1;
	}
      } else {
	// looks like a pointer, but doesn't point to from-space
	// leave it alone
	*to_curr = *to_curr + 1;
      }
      break;
    default:
      // not a pointer
      *to_curr = *to_curr + 1;
    }
  }
}


int64_t* collect_garbage(int64_t* rsp, int64_t *rbp, int64_t* rbx) {
  int stack_count = rbp - rsp;

  val_t *from = heap + ((rbx < (heap + heap_size)) ? 0 : heap_size);
  val_t *to   = heap + ((rbx < (heap + heap_size)) ? heap_size : 0);

  val_t *to_next = to;
  val_t *to_curr = to;

  int t_back = 0;
  int t_front = 0;

  // Step through everything on the stack
  val_t * rsp_curr = rsp;
  step(to, from, &rsp_curr, &to_next, stack_count, &t_back);

  printf("DONE STACK\n");

  int vi;
  // now play catch up between to_curr and to_next
  while (to_curr != to_next) {
    switch (types[t_front++]) {
    case T_VECT:
      vi = to_curr[0];
      to_curr++;
      step(to, from, &to_curr, &to_next, vi, &t_back);
      break;
    case T_BOX:
      step(to, from, &to_curr, &to_next, 1, &t_back);
      break;
    case T_CONS:
      step(to, from, &to_curr, &to_next, 2, &t_back);
      break;
    case T_STR:
      to_curr = to_curr + 1 + ((*to_curr + 1) / 2);
      break;
    default:
      to_curr++;
      break;
    }
  }
  return to_next;
}


void print_memory(int64_t* rsp, int64_t* rbp, int64_t* rbx) {

  val_t* h = heap + ((rbx < (heap + heap_size)) ? 0 : heap_size);

  int stack_count = rbp - rsp;
  int heap_count  = rbx - h;

  printf("----------------------------------------------------------------\n");
  int i;

  printf("STACK:\n");
  for (i = 0; i < stack_count; i++) {
    printf("[%" PRIx64 "] = %016" PRIx64 ", %s\n",
	   (int64_t)rsp + 8*i, rsp[i], val_typeof_string(rsp[i]));
  }
  printf("HEAP:\n");
  for (i = 0; i < heap_count; i++) {
    printf("[%" PRIx64 "] = %016" PRIx64 ", %s\n",
	   (int64_t)h + 8*i, h[i], val_typeof_string(h[i]));
  }
}
