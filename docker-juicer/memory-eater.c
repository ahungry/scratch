#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

char*
eat (int mem) {
  char* x = NULL;
  x = malloc (mem);

  for (int c = 0; c < mem; c++) {
    x[c] = c;
  }

  return x;
}

int
main (int argc, char **argv) {
  int i = 0;

  printf ("first arg was: %s\n", argv[1]);

  int mem = atoi(argv[1]);
  char* x = eat (mem);

  printf ("Lets allocate some memory: %d\n", mem);

  if (x != NULL) {
    printf ("Got the memory.");
  }

  for (;;) {
    printf ("Greetings %d\n", i);
    printf ("Memory block position: %d\n", x[i]);
    eat (1024 * i);
    sleep (1);
    i++;
  }

  exit(0);
}
