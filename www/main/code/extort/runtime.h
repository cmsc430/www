#ifndef RUNTIME_H
#define RUNTIME_H

#include "values.h"

val_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();
#endif /* RUNTIME_H */
