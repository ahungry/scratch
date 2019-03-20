#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct Cons {
  int n;
  struct Cons* next;
} Cons;

Cons*
make_cons (int n)
{
  Cons* cons = (Cons*) malloc (sizeof (Cons));
  cons->n = n;
  cons->next = NULL;

  return cons;
}

void
push_cons (int n, Cons* c)
{
  Cons* new = make_cons (n);
  Cons* head = c;
  while (head->next != NULL)
    {
      printf ("In push_cons: sent: %d - at: %d\n", n, head->n);
      head = head->next;
    }
  head->next = new;
  printf ("Done In push_cons: sent: %d - at: %d\n", n, head->next->n);
}

void
map (int (*fn)(int i), int n)
{
  printf ("%d\n", fn (n));
}

void
cmap (int (*fn)(int i), Cons* n)
{
  Cons* ptr = n;
  printf ("Here it is: %d\n", fn (ptr->n));

  while (ptr != NULL)
    {
      printf ("%d\n", fn (ptr->n));
      ptr = ptr->next;
    }
}

int add_one (int n) { return n + 1; }

int
main ()
{
  // map (&add_one, 3);

  Cons* a = make_cons (50);
  push_cons (6, a);
  push_cons (7, a);
  push_cons (70, a);

  printf ("%d\n", a->n);
  printf ("%d\n", a->next->n);
  printf ("%d\n", a->next->next->n);
  printf ("%d\n", a->next->next->next->n);

  Cons* e = make_cons (0);

  for (int i = 0; i < 3; i++)
    {
      push_cons (i, e);
    }

  cmap (&add_one, e);

  exit (0);
}
