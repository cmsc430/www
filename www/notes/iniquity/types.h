/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers: end in anything else
*/
#define imm_shift 3
#define imm_type_mask ((1 << imm_shift) - 1)
#define imm_type_tag 0

/*
  Immediates are either:
  - Integers: end in #b0 (& immediate tag)
  - Non-Integers: end in #b1 (& immediate tag)
*/
#define int_shift        (imm_shift + 1)
#define int_type_mask    ((1 << int_shift) - 1)
#define int_type_tag     ((0 << (int_shift - 1)) | imm_type_tag)
#define nonint_type_tag  ((1 << (int_shift - 1)) | imm_type_tag)

/*
  Pointers are "tagged addresses"
  - Boxes:   end in #b001
  - Pairs:   end in #b010
  - remaining bit patterns are reserved for future pointers     
  To recover the address, xor the tag to zero it out
*/
#define box_type_tag  1
#define pair_type_tag 2

/*
  Non-Integers are either:
  - Characters:     end in #b0 (& non-integer tag), code-point in remaining bits    
  - Non-Characters: end in #b1 (& non-integer tag)
*/
#define char_shift       (int_shift + 1)
#define char_type_mask   ((1 << char_shift) - 1)
#define char_type_tag    ((0 << (char_shift - 1)) | nonint_type_tag)
#define nonchar_type_tag ((1 << (char_shift - 1)) | nonint_type_tag)

/*
  Non-Chacters are either:
  - True:    #b0 (& non-character tag)
  - False:   #b1 (& non-character tag)
  - Empty:  #b10
  - remaining bit patterns are reserved for future values.
*/
#define singleton_shift (char_shift + 1)
#define val_true  ((0 << (singleton_shift - 1)) | nonchar_type_tag)
#define val_false ((1 << (singleton_shift - 1)) | nonchar_type_tag)
#define val_empty ((2 << (singleton_shift - 1)) | nonchar_type_tag)
