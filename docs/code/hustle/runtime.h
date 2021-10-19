#ifndef RUNTIME_H
#define RUNTIME_H

#include "values.h"

val_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
#define heap_size 10000
extern val_t *heap;

#endif /* RUNTIME_H */
