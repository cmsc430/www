#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "types.h"
#include "heap.h"

#define DEBUG 1

#ifdef DEBUG
#  define GC_DEBUG(x) x
#else
#  define GC_DEBUG(x)
#endif

void print_result(int64_t result);
void print_mem(int64_t *, int64_t *);
  
int obj_size(int64_t v) {
  int type_tag = ptr_type_mask & v;
  int64_t * obj = (int64_t *)(v ^ type_tag);
  switch (type_tag) {
  case box_type_tag:
    return 1;    
  case cons_type_tag:
    return 2;
  case str_type_tag:    
    return (obj[0] >> int_shift) + 1;
  default:
    printf("unkown object type in obj_size");
    exit(1);
  }
}

void print_types(type_front, type_rear) {
  int j;
  printf("TYPES:\n");
  for (j = type_front; j < type_rear; j++) {
    printf("  [%d]: %s\n", j,
	   ((type[j] == str_type_tag) ? "str" :
	    (type[j] == box_type_tag) ? "box" :
	    (type[j] == cons_type_tag) ? "cons" :
	    "unknown"));
  }
}

void move_obj(char ptr_type, int64_t * addr, int64_t ** to_next) {
  int size =
    (ptr_type == box_type_tag)  ? 1 :
    (ptr_type == cons_type_tag) ? 2 :
    (ptr_type == str_type_tag)  ? 1 + (addr[0] >> int_shift) :
    -1;
    
  int i;  
  for (i = 0; i < size; i++) {
    *to_next[0] = addr[i];
    if (i == 0) *addr = (int64_t) *to_next | ptr_type; // fwd
    *to_next = *to_next + 1;
  }
}


char * ptr_type_to_string(int64_t tag);

#define is_fwd(a) ((((a & ptr_addr_mask) - (int64_t) &heap[heap_size]) ^ (int64_t) from_side) >= 0)

void scan_word(int64_t ** curr, int64_t ** to_next, char * type_rear) {
  int64_t v = **curr;
  if (v & ptr_type_mask) {
    printf("pointer!\n");
    int64_t t = ptr_type_mask & v;
    int64_t * a = (int64_t *) (v ^ t);
    if ((*a & ptr_type_mask) && (is_fwd(*a))) {
      GC_DEBUG(printf("forward pointer, resolving\n"));
      *curr[0] = *a;
    } else {
      printf("from pointer, moving\n");
      curr[0] = *to_next;
      move_obj(t, *curr, to_next);
      GC_DEBUG(printf("PUSH!: %s\n", ptr_type_to_string((int64_t) t)));
      type[*type_rear] = t; (*type_rear)++;
    }
  }
  *curr = *curr + 1;
}

void collect_garbage(int64_t * rdi, int64_t * rbp, int64_t * rsp) {
  
  int64_t * to_space = (from_side < 0) ? heap + heap_size : heap;
  int64_t * to_next = to_space;
  
  char type_front = 0;
  char type_rear = 0;

  GC_DEBUG(printf("--------------------------------------------\n"));
  GC_DEBUG(printf("TRACING ROOTS\n"));
  GC_DEBUG(print_mem(rsp, rbp));
      
  // roots
  // shallowly move data pointed to by each root to 'to' space,
  // leaving forwarding address in 'from' space.
  int64_t * root = rsp;
  while (root != rbp) {
    GC_DEBUG(printf("moving a root!\n"));
    int64_t v = root[0];
    if (ptr_type_mask & v) {
      GC_DEBUG(printf("this root is a pointer, moving\n"));
      int64_t * a = (int64_t *) (ptr_addr_mask & v);
      if ((*a & ptr_type_mask) && (is_fwd(*a))) {
	root[0] = *a;
	GC_DEBUG(printf("forward pointer, resolving\n"));
      } else {
	GC_DEBUG(printf("from space pointer, moving\n"));
	int64_t t = ptr_type_mask & v;	
	move_obj(t, a, &to_next);
	root[0] = *a;
	// push t
	type[type_rear] = t; type_rear++;
      }
    } else {
      GC_DEBUG(printf("this root is an immediate, skipping.\n"));
    }
    
    GC_DEBUG(print_types(type_front, type_rear));
    GC_DEBUG(printf("ROOT:\n"));
    GC_DEBUG(print_mem(rsp, rbp));
    GC_DEBUG(printf("FROM:\n"));
    GC_DEBUG(print_mem(heap, rdi));
    GC_DEBUG(printf("TO:\n"));
    GC_DEBUG(print_mem(to_space, to_next));
    
    // advance
    root = &root[1];
  }

  GC_DEBUG(printf("--------------------------------------------\n"));
  GC_DEBUG(printf("TRACING TO SPACE\n"));
  
  int64_t * curr = to_space;
  while (curr != to_next) {
    int j;
    char t = type[type_front]; type_front++;

    GC_DEBUG(printf("TRACING A %s\n", ptr_type_to_string((int64_t) t)));
    
    switch (t) {
    case box_type_tag:
      scan_word(&curr, &to_next, &type_rear);
      break;
    case cons_type_tag:
      scan_word(&curr, &to_next, &type_rear);
      scan_word(&curr, &to_next, &type_rear);
      break;
    case str_type_tag:
      curr = &curr[1+(*curr >> int_shift)]; break;
    default:
      printf("unknown type: %d!!!\n", t);
      exit(1);
    }
    
    GC_DEBUG(print_types(type_front, type_rear));
    GC_DEBUG(printf("FROM:\n"));
    GC_DEBUG(print_mem(heap, rdi));
    GC_DEBUG(printf("TO:\n"));
    GC_DEBUG(print_mem(to_space, to_next));
  }

  GC_DEBUG(printf("--------------------------------------------\n"));
  GC_DEBUG(printf("DONE\n"));

  GC_DEBUG(printf("ROOT:\n"));
  GC_DEBUG(print_mem(rsp, rbp));
  GC_DEBUG(printf("TO:\n"));
  GC_DEBUG(print_mem(to_space, to_next));
}

char * ptr_type_to_string(int64_t tag) {
  switch (tag) {
  case box_type_tag:
    return "box";
  case cons_type_tag:
    return "cons";
  case str_type_tag:
    return "str";
  default:
    return "unknown";
  } 
}

void print_mem(int64_t *h, int64_t *end_of_heap) {
  int64_t size = ((int64_t)end_of_heap - (int64_t)h) / 8;  
  //printf("HEAP (%" PRId64 "):\n", size);
  int i;
  for (i = 0; i < size; i++) {
    printf("  [%" PRId64 "]: ", (int64_t)&h[i]);
    if (ptr_type_mask & h[i]) {
      printf("&<%s>%" PRId64 "\n", ptr_type_to_string(ptr_type_mask & h[i]), h[i] & ptr_addr_mask);
    } else {
      print_result(h[i]);
      printf("\n");
    }
  }
}
  
