#include <string.h>
#include <stdio.h>

int
kmp_ok ()
{
  int i = 2;
  int j = 1;
  int textlen = 5;
  int patlen = 2;
  char *text = "qqaqq";
  char *pat = "qq";

  while (i < textlen) {
    fprintf (stderr, "kmp_next found a while loop slice\n");
    fflush (stderr);

    if (text[i] == pat[j]) {
      if (j == patlen - 1) {
        fprintf (stderr, "pre-lookup\n");
        fflush (stderr);
        fprintf (stderr, "post-lookup\n");
        fflush (stderr);
        return i - j;
      } else {
        fprintf(stderr, "iterate...\n");
        fflush (stderr);
        i++;
        j++;
      }
    } else {
      if (j > 0) {
        fprintf (stderr, "Lookup here...\n");
        // j = lookup[j - 1];
        j = 0;
      } else {
        i++;
      }
    }
  }
  return -1;
}

int
kmp_notok ()
{
  int i = 2;
  int j = 1;
  int textlen = 4;
  int patlen = 2;
  char *text = "qqqq";
  char *pat = "qq";

  while (i < textlen) {
    fprintf (stderr, "kmp_next found a while loop slice\n");
    fflush (stderr);

    if (text[i] == pat[j]) {
      if (j == patlen - 1) {
        fprintf (stderr, "pre-lookup\n");
        fflush (stderr);
        fprintf (stderr, "post-lookup\n");
        fflush (stderr);
        return i - j;
      } else {
        fprintf(stderr, "iterate...\n");
        fflush (stderr);
        i++;
        j++;
      }
    } else {
      if (j > 0) {
        fprintf (stderr, "Lookup here...\n");
        // j = lookup[j - 1];
        j = 2;
      } else {
        i++;
      }
    }
  }
  return -1;
}

int
main (int argc, char *argv[])
{
  fprintf (stderr, "kmp_ok: %d\n\n", kmp_ok()); // 3
  fprintf (stderr, "kmp_notok: %d\n\n", kmp_notok()); // should be 2, NOT 1

  return 0;
}
