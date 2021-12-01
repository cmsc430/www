#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();
val_t *heap;
type_t *types;

void error_exit()
{
  printf("err\n");
  exit(1);
}

void raise_error()
{
  return error_handler();
}

int main(int argc, char** argv)
{
  in = stdin;
  out = stdout;
  error_handler = &error_exit;
  heap = malloc(2 * 8 * heap_size);
  types = malloc(sizeof(type_t) * heap_size);
  
  val_t result;

  result = entry(heap);

  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  free(heap);
  return 0;
}

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

void step(val_t* to, val_t** to_curr, val_t** to_next, int count, int* t_back) {
  type_t t;
  int i;
  int size;
  for (i = 0; i < count; i++) {
    t = val_typeof(**to_curr);
    switch (t) {
    case T_BOX:
    case T_CONS:
    case T_VECT:
      if (val_unwrap(*val_unwrap(**to_curr)) >= to &&
	  val_unwrap(*val_unwrap(**to_curr)) < to + heap_size) {
	// this is a fwd pointer (points in to to-space), so just set
	// curr to what it points to.
	**to_curr = *val_unwrap(**to_curr);
	*to_curr = *to_curr + 1;
      } else {
	// not a fwd pointer, copy to to_next
	size = val_size(val_unwrap(**to_curr), t);
	types[*t_back] = t;                                 // enqueue type
	*t_back = *t_back + 1;
	memcpy(*to_next, val_unwrap(**to_curr), 8 * size);  // copy
	*val_unwrap(**to_curr) = val_wrap(*to_next, t);     // fwd      
	**to_curr              = val_wrap(*to_next, t);     // update
	*to_next = *to_next + size;
	*to_curr = *to_curr + 1;
      };
      break;
    case T_STR:
      printf("STRING STEP\n");
      break;
    default:
      *to_curr = *to_curr + 1;
    };
  };
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
  step(to, &rsp_curr, &to_next, stack_count, &t_back);
  
  int vi;
  // now play catch up between to_curr and to_next
  while (to_curr != to_next) {   
    switch (types[t_front++]) {
    case T_VECT:
      vi = to_curr[0];
      to_curr++;
      step(to, &to_curr, &to_next, vi, &t_back);
      break;
    case T_BOX:
      step(to, &to_curr, &to_next, 1, &t_back);
      break;
    case T_CONS:
      step(to, &to_curr, &to_next, 2, &t_back);
      break;
    case T_STR:
      printf("STRING!!!\n");
      printf("size: %lld\n", 1 + ((*to_curr + 1) / 2));
      to_curr = to_curr + 3;
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
    printf("[%" PRIx64 "] = %016" PRIx64 ", %s\n", (int64_t)rsp + 8*i, rsp[i], val_typeof_string(rsp[i]));
  }
  printf("HEAP:\n");
  for (i = 0; i < heap_count; i++) {
    printf("[%" PRIx64 "] = %016" PRIx64 ", %s\n", (int64_t)h + 8*i, h[i], val_typeof_string(h[i]));
  }
}
