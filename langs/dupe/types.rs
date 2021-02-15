/*
  Bit layout of values

  Values are either:
  - Integers:   end in  #b0
  - True:              #b01
  - False:             #b11
*/
pub const INT_SHIFT      : i64 = 0b1;
pub const INT_TYPE_MASK  : i64 = (1 << INT_SHIFT) - 1;
pub const INT_TYPE_TAG   : i64 = 0 << (INT_SHIFT - 1);
pub const NONINT_TYPE_TAG: i64 = 1 << (INT_SHIFT - 1);
pub const VAL_TRUE       : i64 = (0 << INT_SHIFT) | NONINT_TYPE_TAG;
pub const VAL_FALSE      : i64 = (1 << INT_SHIFT) | NONINT_TYPE_TAG;
