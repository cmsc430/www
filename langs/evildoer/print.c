#include <stdio.h>
#include <inttypes.h>

int64_t entry();

int main(int argc, char** argv) {
  printf("%" PRId64 "\n", entry());
  return 0;
}
