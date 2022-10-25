#include <stdio.h>
#include <stdlib.h>
#include <sigsegv.h>
#include <err.h>
#include <string.h> /* for memset */
#include "values.h"
#include "print.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();
val_t *heap;


#define MYSTACK_CRUMPLE_ZONE 8192
char mystack_storage[SIGSTKSZ + 2 * MYSTACK_CRUMPLE_ZONE + 31];
char *mystack; /* SIGSTKSZ bytes in the middle of storage. */

static void
prepare_alternate_stack (void)
{
  memset (mystack_storage, 's', sizeof mystack_storage);
  mystack = (char *) ((uintptr_t) (mystack_storage + MYSTACK_CRUMPLE_ZONE) | 31);
}

static void
check_alternate_stack_no_overflow (void)
{
  unsigned int i;

  for (i = MYSTACK_CRUMPLE_ZONE; i > 0; i--)
    if (*(mystack - i) != 's')
      {
        printf ("Alternate stack was exceeded by %u bytes!!\n", i);
        exit (1);
      }
  for (i = MYSTACK_CRUMPLE_ZONE; i > 0; i--)
    if (*(mystack + SIGSTKSZ - 1 + i) != 's')
      {
        printf ("Alternate stack was exceeded by %u bytes!!\n", i);
        exit (1);
      }
}


static sigset_t mainsigset;

static volatile int pass = 0;

static volatile char *stack_lower_bound;
static volatile char *stack_upper_bound;

static void
stackoverflow_handler_continuation (void *arg1, void *arg2, void *arg3)
{
  int arg = (int) (long) arg1;
  //  longjmp (mainloop, arg);
}

static void
stackoverflow_handler (int emergency, stackoverflow_context_t scp)
{
  char dummy;
  volatile char *addr = &dummy;
  if (!(addr >= stack_lower_bound && addr <= stack_upper_bound))
    abort ();
  pass++;
  printf ("Stack overflow %d caught.\n", pass);
  sigprocmask (SIG_SETMASK, &mainsigset, NULL);
  sigsegv_leave_handler (stackoverflow_handler_continuation,
                         (void *) (long) (emergency ? -1 : pass), NULL, NULL);
}


void error_exit()
{
  printf("err\n");
  exit(1);
}

void raise_error()
{
  return error_handler();
}

int main(int argc, char** argv)
{
  sigset_t emptyset;
  prepare_alternate_stack ();

    /* Install the stack overflow handler.  */
  if (stackoverflow_install_handler (&stackoverflow_handler,
                                     mystack, SIGSTKSZ)
      < 0)
    exit (2);
  stack_lower_bound = mystack;
  stack_upper_bound = mystack + SIGSTKSZ - 1;

  /* Save the current signal mask.  */
  sigemptyset (&emptyset);
  sigprocmask (SIG_BLOCK, &emptyset, &mainsigset);
    
  in = stdin;
  out = stdout;
  error_handler = &error_exit;
  heap = malloc(8 * heap_size);

  val_t result;
  result = entry(heap);

  print_result(result);
  
  if (val_typeof(result) != T_VOID)
    putchar('\n'); 

  free(heap);
  return 0;
}
