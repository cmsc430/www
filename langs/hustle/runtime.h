int64_t entry();
FILE* in;
FILE* out;
void (*error_handler)();

// in words
#define heap_size 10000
int64_t *heap;
