#include <stdio.h>
#include <inttypes.h>

int64_t abscond_entry();

int main(int argc, char** argv) {
  int64_t result = abscond_entry();
  printf("%" PRId64 "\n", result);
  return 0;
}
