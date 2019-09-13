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
int i = 0;
int* fp[2];

void* _make_counter (void *ptr)
{
  double counter = (double)(int) ptr;
  int lexical_fn (double y) { return counter += y; }
  fp[i++] = lexical_fn;

  // The scope never leaves if we keep it in a thread forever or until done.
  for (;;) { sleep (1); }
}

int make_counter (int n)
{
  int nth = i;
  pthread_t pth;
  pthread_create (&pth, NULL, _make_counter, (void*)(int) n);
  usleep (1);

  return nth;
}

int main ()
{
  int x = make_counter (1);
  printf ("Counter X is: %f\n", invoke (fp[x], 5));
  int y = make_counter (3);
  printf ("Counter X is: %f\n", invoke (fp[x], 2.3));
  // Well, it looks like it works, as long as you never call this.
  // printf ("Counter Y is: %f\n", invoke (fp[y], 10));

  printf ("fin.\n");

  exit (0);
}

/* x = foo(1);  */
/* x(5);  */
/* foo(3); */
/* print x(2.3); */
// Should result in 8.3
