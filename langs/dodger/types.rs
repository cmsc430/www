/*
  Bit layout of values

  Values are either:
  - Integers:   end in  #b0
  - Characters: end in #b01
  - True:              #b11
  - False:            #b111
*/
pub const INT_SHIFT       : i64 = 0b1;
pub const INT_TYPE_MASK   : i64 = (1 << INT_SHIFT) - 1;
pub const INT_TYPE_TAG    : i64 = 0 << (INT_SHIFT - 1);
pub const NONINT_TYPE_TAG : i64 = 1 << (INT_SHIFT - 1);
pub const CHAR_SHIFT      : i64 = (int_shift + 1)
pub const CHAR_TYPE_MASK  : i64 = (1 << char_shift) - 1
pub const CHAR_TYPE_TAG   : i64 = (0 << (char_shift - 1)) | nonint_type_tag
pub const NONCHAR_TYPE_TAG: i64 = (1 << (char_shift - 1)) | nonint_type_tag
pub const VAL_TRUE        : i64 = (0 << CHAR_SHIFT) | NONINT_TYPE_TAG;
pub const VAL_FALSE       : i64 = (1 << CHAR_SHIFT) | NONINT_TYPE_TAG;
