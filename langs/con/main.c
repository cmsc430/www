#include <stdio.h>
#include <inttypes.h>

int64_t entry();
void print_result(int64_t);

int main(int argc, char** argv)
{
  int64_t result;

  result = entry();
  print_result(result);
  return 0;
}

void print_result(int64_t x)
{
  printf("%" PRId64, x);
  printf("\n");
}
