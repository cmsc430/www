/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers

  Immediates are either
  - Integers:   end in  #b0 000
  - Characters: end in #b01 000
  - True:              #b11 000
  - False:           #b1 11 000
  - Eof:            #b10 11 000
  - Void:           #b11 11 000
  - Empty:         #b100 11 000

  Pointers are
  - Box:  end in #b001
  - Cons: end in #b010
  - Proc: end in #b100
*/
#define imm_shift        3
#define ptr_type_mask    ((1 << imm_shift) - 1)
#define ptr_addr_mask    ~ptr_type_mask
#define box_type_tag     1
#define cons_type_tag    2
#define proc_type_tag    4
#define int_shift        (1 + imm_shift)
#define int_type_mask    ((1 << int_shift) - 1)
#define int_type_tag     (0 << (int_shift - 1))
#define nonint_type_tag  (1 << (int_shift - 1))
#define char_shift       (int_shift + 1)
#define char_type_mask   ((1 << char_shift) - 1)
#define char_type_tag    ((0 << (char_shift - 1)) | nonint_type_tag)
#define nonchar_type_tag ((1 << (char_shift - 1)) | nonint_type_tag)
#define val_true  ((0 << char_shift) | nonchar_type_tag)
#define val_false ((1 << char_shift) | nonchar_type_tag)
#define val_eof   ((2 << char_shift) | nonchar_type_tag)
#define val_void  ((3 << char_shift) | nonchar_type_tag)
#define val_empty ((4 << char_shift) | nonchar_type_tag)
