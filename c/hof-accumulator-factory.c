// https://rosettacode.org/wiki/Accumulator_factory#C
// Compile and exec in one go with:
// gcc -g -lssl -lcrypto -pthread -lwebsockets -lcurl -Wall %s -o /tmp/%s.bin && /tmp/%s.bin &
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

double invoke (double (*f)(double i), double y) { return f (y); }
int* fp;
void* make_counter (void *ptr)
{
  double counter = (double)(int) ptr;
  int lexical_fn (double y) { return counter += y; }
  fp = lexical_fn;

  // The scope never leaves if we keep it in a thread forever or until done.
  for (;;) { sleep (1); }
}

int main ()
{
  pthread_t pth;
  pthread_create (&pth, NULL, make_counter, (void*)(int) 1);
  usleep (1);

  // Now we have a HOF in fn_ptr that will return 100 + arg
  printf ("Counter is: %f\n", invoke (fp, 5));
  printf ("Counter is: %f\n", invoke (fp, 2.3));

  printf ("fin.\n");

  exit (0);
}

/* x = foo(1);  */
/* x(5);  */
/* foo(3); */
/* print x(2.3); */
// Should result in 8.3
