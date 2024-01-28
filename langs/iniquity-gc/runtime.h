#ifndef RUNTIME_H
#define RUNTIME_H
int64_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
#define heap_size 10000
extern int64_t *heap;
extern val_t *from;
extern val_t *to;

extern type_t *types;
#endif /* RUNTIME_H */
