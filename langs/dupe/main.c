#include "values.h"
#include "print.h"

val_t entry();

int main(int argc, char** argv)
{
  val_t result;

  result = entry();
  print_result(result);
  return 0;
}
