#ifndef RUNTIME_H
#define RUNTIME_H
int64_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();
#endif /* RUNTIME_H */
