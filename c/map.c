#include <stdlib.h>
#include <stdio.h>
#include <string.h>

double foo (double a, double b)
{
  double square (double z) { return z * z; }

  return square (a) + square (b);
}

int
caller (int (*fn)(int i), int y)
{
  return fn (y);
}

// https://gcc.gnu.org/onlinedocs/gcc/Nested-Functions.html
int*
adder (int x)
{
  int add_fn (int y) {
    printf ("Doing stuff in your lexical scope.\n");
    // Illegal instruction, core dumped if you refer here.
    // printf ("And...x is??? %d\n", x);

    return y;
    // return x + y;
  }

  // So, we can call the closure from other areas, as long as we don't
  // leave the scope of this actual declaration of it's lexical
  // variables...
  // return caller (add_fn, 10);

  return add_fn;
}

/* hack (int *array, int size) */
/* { */
/*   void store (int index, int value) */
/*   { array[index] = value; } */

/*   intermediate (store, size); */
/* } */

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

  printf ("Foo was: %f\n", foo (1, 2));

  int* fn = adder (30);
  int hah = caller (fn, 50);
  printf ("Hah was: %d\n", hah);
  // printf ("The nested fn was: %d\n", adder (10));
  printf ("FIN\n");

  exit (0);
}
