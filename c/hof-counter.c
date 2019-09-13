// https://rosettacode.org/wiki/Accumulator_factory#C
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

int invoke (int (*f)(int i), int y) { return f (y); }
int* fp;
void* make_counter (void *ptr)
{
  int counter = (int) ptr;
  int lexical_fn (int y) { return counter += y; }
  fp = lexical_fn;

  // The scope never leaves if we keep it in a thread forever or until done.
  for (;;) { sleep (1); }
}

int main ()
{
  pthread_t pth;
  pthread_create (&pth, NULL, make_counter, (void*)(int) 1);

  // A tiny pause is required.
  usleep (1);

  // Now we have a HOF in fn_ptr that will return 100 + arg
  printf ("Counter is: %d\n", invoke (fp, 5));
  printf ("Counter is: %d\n", invoke (fp, 2));

  printf ("fin.\n");

  exit (0);
}

/* x = foo(1);  */
/* x(5);  */
/* foo(3); */
/* print x(2.3); */
// Should result in 8.3
