#define result_shift     3
#define result_type_mask ((1 << result_shift) - 1)
#define type_imm         0
#define type_box         1
#define type_pair        2
#define type_string      3
#define type_proc        4

#define imm_shift      (3 + result_shift)
#define imm_type_mask  ((1 << imm_shift) - 1)
#define imm_type_int   (0 << result_shift)
#define imm_type_bool  (1 << result_shift)
#define imm_type_char  (2 << result_shift)
#define imm_type_empty (3 << result_shift)
