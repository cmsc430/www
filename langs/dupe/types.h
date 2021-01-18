/*
  Bit layout of values

  Values are either:
  - Integers:   end in  #b0
  - True:              #b01
  - False:             #b11
*/
#define int_shift        1
#define int_type_mask    ((1 << int_shift) - 1)
#define int_type_tag     (0 << (int_shift - 1))
#define nonint_type_tag  (1 << (int_shift - 1))
#define val_true  ((0 << int_shift) | nonint_type_tag)
#define val_false ((1 << int_shift) | nonint_type_tag)
