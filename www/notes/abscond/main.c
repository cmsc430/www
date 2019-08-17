#include <stdio.h>
#include <inttypes.h>

int64_t entry();

int main(int argc, char** argv) {
  int64_t result = entry();
  printf("%" PRId64 "\n", result);
  return 0;
}
