#include <stdio.h>
#include <inttypes.h>

int64_t entry();

int main(int argc, char** argv) {
  int64_t result = entry();
  printf("result: %" PRId64 "\n", result); return 0;
}

int64_t get_int() {
  int64_t x;
  scanf("%lld", &x);
  return x;
} 
