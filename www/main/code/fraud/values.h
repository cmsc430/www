#ifndef VALUES_H
#define VALUES_H

#include <stdint.h>

/* any abstract value */
typedef int64_t val_t;

typedef enum type_t {
  T_INVALID = -1,
  /* immediates */
  T_INT,
  T_BOOL,
  T_CHAR,
  T_EOF,  
  T_VOID,
} type_t;

typedef uint32_t val_char_t;

/* return the type of x */
type_t val_typeof(val_t x);

/**
 * Wrap/unwrap values
 *
 * The behavior of unwrap functions are undefined on type mismatch.
 */
int64_t val_unwrap_int(val_t x);
val_t val_wrap_int(int64_t i);

int val_unwrap_bool(val_t x);
val_t val_wrap_bool(int b);

val_char_t val_unwrap_char(val_t x);
val_t val_wrap_char(val_char_t b);

val_t val_wrap_eof();

val_t val_wrap_void();

#endif
