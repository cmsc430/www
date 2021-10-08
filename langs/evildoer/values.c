#include "types.h"
#include "values.h"

type_t val_typeof(val_t x)
{
  if ((int_type_mask & x) == int_type_tag)
    return T_INT;
  if ((char_type_mask & x) == char_type_tag)
    return T_CHAR;
  
  switch (x) {
  case val_true:
  case val_false:
    return T_BOOL;
  case val_eof:
    return T_EOF;
  case val_void:
    return T_VOID;    
  }
  
  return T_INVALID;
}

int64_t val_unwrap_int(val_t x)
{
  return x >> int_shift;
}
val_t val_wrap_int(int64_t i)
{
  return (i << int_shift) | int_type_tag;
}

int val_unwrap_bool(val_t x)
{
  return x == val_true;
}
val_t val_wrap_bool(int b)
{
  return b ? val_true : val_false;
}

val_char_t val_unwrap_char(val_t x)
{
  return (val_char_t)(x >> char_shift);
}
val_t val_wrap_char(val_char_t c)
{
  return (((val_t)c) << char_shift) | char_type_tag;
}

val_t val_wrap_eof(void)
{
  return val_eof;
}

val_t val_wrap_void(void)
{
  return val_void;
}
