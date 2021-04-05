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

const char * ptr_type_to_string(int64_t tag);

int obj_size(int64_t v) {
  int type_tag = ptr_type_mask & v;
  int64_t * obj = (int64_t *)(v ^ type_tag);
  switch (type_tag) {
  case box_type_tag:
    return 1;    
  case cons_type_tag:
    return 2;
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
	   ((type[j] == box_type_tag) ? "box" :
	    (type[j] == cons_type_tag) ? "cons" :
	    "unknown"));
  }
}

void move_obj(char ptr_type, int64_t * addr, int64_t ** to_next) {
  GC_DEBUG(printf("move_obj <%s> at [%" PRIx64 "] to [%" PRIx64 "]\n",
		  ptr_type_to_string((int64_t) ptr_type),
		  (int64_t) addr,
		  (int64_t) *to_next));
  int size =
    (ptr_type == box_type_tag)  ? 1 :
    (ptr_type == cons_type_tag) ? 2 :
    -1;
    
  int i;  
  for (i = 0; i < size; i++) {
    GC_DEBUG(printf("  [%" PRIx64 "] <- [%" PRIx64 "]\n",
		    (int64_t) *to_next, (int64_t) (addr + i)));
		    
    *to_next[0] = addr[i];
    if (i == 0) {
      *addr = (int64_t) *to_next | ptr_type; // fwd
      GC_DEBUG(printf("  [%" PRIx64 "] <- [%" PRIx64 "] (fwd)\n",
		      (int64_t) addr , (int64_t) *to_next));
      
    }
    *to_next = *to_next + 1;
  }
}

#define is_fwd(a) ((((a & ptr_addr_mask) - (int64_t) &heap[heap_size]) ^ (int64_t) from_side) >= 0)

void scan_word(int64_t ** curr, int64_t ** to_next, char * type_rear) {
  GC_DEBUG(printf("scan_word [%" PRIx64 "]: ", (int64_t) *curr));
  int64_t v = **curr;
  if (v & ptr_type_mask) {
    int64_t t = ptr_type_mask & v;
    int64_t * a = (int64_t *) (ptr_addr_mask & v);
    if ((*a & ptr_type_mask) && (is_fwd(*a))) {
      GC_DEBUG(printf("&<fwd>[%" PRIx64 "] -> [%" PRIx64 "]",
		      (int64_t) a, (int64_t) (*a & ptr_addr_mask)));
      *curr[0] = *a;
    } else {
      GC_DEBUG(printf("&<%s>[%" PRIx64 "]\n", ptr_type_to_string(t), (int64_t) a));
      **curr = ((int64_t) (*to_next)) | t;
      move_obj(t, a, to_next);
      GC_DEBUG(printf("PUSH!: %s", ptr_type_to_string((int64_t) t)));
      type[*type_rear] = t; (*type_rear)++;
    }
  } else {
    GC_DEBUG(print_result(v)); // an immediate
  }
  GC_DEBUG(printf("\n"));
  *curr = *curr + 1;
}

struct Pair {
  int64_t x;
  int64_t y;
};


int64_t * collect_garbage_p(int64_t * rdi, int64_t * rbp, int64_t * rsp) {
  struct Pair p;
  p.x = 9 << int_shift;
  p.y = 32 << int_shift;
  return rdi;
}

int64_t * collect_garbage(int64_t * rdi, int64_t * rbp, int64_t * rsp) {
  
  int64_t * to_space   = (from_side < 0) ? heap + heap_size : heap;
  int64_t * from_space = (from_side > 0) ? heap + heap_size : heap;
  int64_t * to_next    = to_space;
  
  char type_front = 0;
  char type_rear = 0;

  GC_DEBUG(printf("--------------------------------------------\n"));
  GC_DEBUG(printf("TRACING ROOTS\nROOTS:\n"));
  GC_DEBUG(print_mem(rsp, rbp));
  GC_DEBUG(printf("FROM:\n"));
  GC_DEBUG(print_mem(from_space, rdi));
      
  // roots
  // shallowly move data pointed to by each root to 'to' space,
  // leaving forwarding address in 'from' space.
  int64_t * root = rsp;
  while (root != rbp) {
    GC_DEBUG(printf("scan_root:"));
    GC_DEBUG(print_mem(root, root + 1));
    int64_t v = root[0];
    if (ptr_type_mask & v) {
      int64_t * a = (int64_t *) (ptr_addr_mask & v);
      if ((*a & ptr_type_mask) && (is_fwd(*a))) {
	root[0] = *a;
	GC_DEBUG(printf("forward pointer, resolving\n"));
      } else {
	int64_t t = ptr_type_mask & v;	
	move_obj(t, a, &to_next);
	root[0] = *a;
	GC_DEBUG(printf("PUSH!: %s\n", ptr_type_to_string((int64_t) t)));
	type[type_rear] = t; type_rear++;
      }
    } else {
      GC_DEBUG(printf("  "));
      GC_DEBUG(print_result(root[0]));
    }
    
    GC_DEBUG(print_types(type_front, type_rear));
    GC_DEBUG(printf("ROOT:\n"));
    GC_DEBUG(print_mem(rsp, rbp));
    GC_DEBUG(printf("FROM:\n"));
    GC_DEBUG(print_mem(from_space, rdi));
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
    default:
      printf("unknown type: %d!!!\n", t);
      exit(1);
    }
    
    GC_DEBUG(print_types(type_front, type_rear));
    GC_DEBUG(printf("FROM:\n"));
    GC_DEBUG(print_mem(from_space, rdi));
    GC_DEBUG(printf("TO:\n"));
    GC_DEBUG(print_mem(to_space, to_next));
  }

  GC_DEBUG(printf("--------------------------------------------\n"));
  GC_DEBUG(printf("DONE\n"));

  GC_DEBUG(printf("ROOT:\n"));
  GC_DEBUG(print_mem(rsp, rbp));
  GC_DEBUG(printf("TO:\n"));
  GC_DEBUG(print_mem(to_space, to_next));
  
  from_side = 0 - from_side;
  return to_next;
}

const char * ptr_type_to_string(int64_t tag) {
  switch (tag) {
  case box_type_tag:
    return "box";
  case cons_type_tag:
    return "cons";
  default:
    return "unknown";
  } 
}

void print_blobs(void *h, void *end_of_heap, int size_in_bytes, void (* print)()) {
  int i;
  while (h < end_of_heap) {
    (*print)(h);
    h = h + size_in_bytes;
  }
}

void print_word(int64_t *a) {
  printf("  [%" PRIx64 "]: ", (int64_t)a);
  if (ptr_type_mask & *a) {
      printf("&<%s>%" PRIx64 "\n", ptr_type_to_string(ptr_type_mask & *a), *a & ptr_addr_mask);
  } else {
    print_result(*a);
    printf("\n");
  }
}

void print_mem(int64_t *h, int64_t *end_of_heap) {
  print_blobs(h, end_of_heap, 8, &print_word);
}
  
