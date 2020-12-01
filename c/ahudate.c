#include "string.h"
#include <stdio.h>
#include <stdlib.h>

// https://en.wikipedia.org/wiki/Year_2038_problem
int64_t
ahudate_year_to_epoch (char * year)
{
  int64_t y = atoi (year);

  fprintf (stderr, "The year was: %ld\n", y);

  return (int64_t) ((y - 1970) * 365.25 * 24 * 60 * 60);
}

int
ahudate_is_numeric (char c)
{
  // 48 to 57 are valid ASCII for this
  return (int) c >= 48 && (int) c <= 57;
}

int
ahudate_is_separator (char c)
{
  return ' ' == c || '/' == c || '-' == c;
}

enum masks {
  is_numeric,
  is_separator
};

int
ahudate_is_x (char c, int i)
{
  switch (i)
    {
    case is_numeric:
      return ahudate_is_numeric (c);

    case is_separator:
      return ahudate_is_separator (c);
    }

  return 0;
}

int
ahudate_is_mask_valid (char *s, int masks[], int mask_size)
{
  for (int i = 0; i < mask_size; i++)
    {
      if (! ahudate_is_x (s[i], masks[i]))
        {
          return 0;
        }
    }

  return 1;
}

typedef struct ahudate_mask {
  int size;
  int mask[];
} ahudate_mask_t;

ahudate_mask_t *
make_ahudate_mask (char *s)
{
  char c;
  ahudate_mask_t * m = malloc (sizeof (ahudate_mask_t));
  m->size = 0;

  for (int i = 0; i < (int) strlen (s); i++)
    {
      c = s[i];

      switch (c)
        {
        case 'd':
          m->mask[i] = is_numeric;
          break;
        case '/':
          m->mask[i] = is_separator;
          break;
        }
      m->size = i;
    }

  return m;
}

int
ahudate_is_yyyy_mm_dd (char *s)
{
  ahudate_mask_t * m = make_ahudate_mask ("dddd/dd/dd");
  int n = ahudate_is_mask_valid (s, m->mask, m->size);
  free (m);

  return n;
}

int
ahudate_get_year (char *s, char *year)
{
  int i = 0;

  for (int c = 0; c < (int) strlen (s); c++)
    {
      if (s[c] == '/' && i < 4)
        {
          i = 0;
          continue;
        }

      year[i++] = s[c];

      if (i > 3) return 0;
    }

  return 0;
}

int
main (int argc, char *argv[])
{
  for (int i = 1; i < argc; i++)
    {
      char year[4];

      if (! ahudate_is_yyyy_mm_dd (argv[i]))
        {
          fprintf (stderr, "\nInvalid input: %s\n", argv[i]);
          continue;
        }
      fprintf (stderr, "\nValid input: %s\n", argv[i]);

      ahudate_get_year (argv[i], year);

      fprintf (stderr, "Year was:%s\n", year);
      fprintf (stderr, "Was it valid?: %d\n", ahudate_is_yyyy_mm_dd (argv[i]));
      fprintf (stderr, "To epoch: %ld\n", ahudate_year_to_epoch (year));
    }

  exit (EXIT_SUCCESS);
}
