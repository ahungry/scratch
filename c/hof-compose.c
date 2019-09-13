// Compile and exec in one go with:
// gcc -g -lssl -lcrypto -pthread -lwebsockets -lcurl -Wall %s -o /tmp/%s.bin && /tmp/%s.bin &
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

int invoke (int (*f)(int i), int y)
{
  printf("Invoked!");
  printf("%d!", y);
  fflush (stdout);
  // return f (y);
  int(*g)(int i) = (int)f (0);
  return g (y);
}
int i = 0;
int ft[2]; // fn thread hold
int* fp[2]; // fn pointers to lexical envs

void* _make_compose (void *ptr)
{
  int nth = i;
  void *f = ptr;
  void* lexical_fn (int y) {
    return f;
  }
  fp[i++] = lexical_fn;
  ft[nth] = 1;

  // The scope never leaves if we keep it in a thread forever or until done.
  for (;;) { sleep (1); }
}

int compose (int(*f)(int i))
{
  int nth = i;
  pthread_t pth;
  pthread_create (&pth, NULL, _make_compose, (void*)(int) f);
  // Spin until the closure is in place.
  for (;;) { if (ft[nth] == 1) break; }

  return nth;
}

int add1 (int n) { return n + 1; }
int main ()
{
  int(*f)(int) = add1;
  int a = f (2);
  printf ("res is: %d\n", a);
  int add2 = compose (add1);
  printf ("add2 is: %d\n", add2);
  printf ("Compose X is: %d\n", invoke (fp[add2], 1));   // x is 7
  /* int y = compose (3); */
  /* printf ("Compose Y is: %f\n", invoke (fp[y], 1));   // y is 14 */
  printf ("fin.\n");

  exit (0);
}
