#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

int* fn_ptr;

int caller (int (*fn)(int i), int y) { return fn (y); }

void* adder (void *ptr)
{
  int lexical_fn (int y) { return (int) ptr + y; }
  fn_ptr = lexical_fn;
  for (;;) { sleep (1); }
}

int main ()
{
  pthread_t pth;
  pthread_create (&pth, NULL, adder, (void*)(int) 100);
  usleep (1);
  // Now we have a HOF in fn_ptr that will return 100 + arg
  assert (130 == caller (fn_ptr, 30));
  assert (150 == caller (fn_ptr, 50));

  printf ("fin.\n");

  exit (0);
}
