#ifndef RUNTIME_H
#define RUNTIME_H
int64_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
#define heap_size 100000000
extern int64_t *heap;
#endif /* RUNTIME_H */
