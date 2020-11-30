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
ahudate_is_yyyy_mm_dd (char *s, char sep)
{
  for (int i = 0; i < (int) strlen (s); i++)
    {
      if ((i == 4 || i == 7) && s[i] == sep)
        {
          continue;
        }

      if (! ahudate_is_numeric (s[i]))
        {
          return 0;
        }

      if (i >= 9)
        {
          return 1;
        }
    }

  return 0;
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
  char year[4];

  if (strstr (argv[1], "/"))
    {
      fprintf (stderr, "Slash formatted");
      ahudate_get_year (argv[1], year);
    }

  fprintf (stderr, "Hello World\n");
  fprintf (stderr, "Year was:%s\n", year);
  fprintf (stderr, "Was it valid?: %d\n", ahudate_is_yyyy_mm_dd (argv[1], '/'));
  fprintf (stderr, "To epoch: %ld\n", ahudate_year_to_epoch (year));

  exit (EXIT_SUCCESS);

  /* struct tm *tmp; */

  /* for (int j = 1; j < argc; j++) { */
  /*   tmp = getdate(argv[j]); */

  /*   if (tmp == NULL) { */
  /*     printf("Call %d failed; getdate_err = %d\n", */
  /*            j, getdate_err); */
  /*     continue; */
  /*   } */

  /*   printf("Call %d (\"%s\") succeeded:\n", j, argv[j]); */
  /*   printf("    tm_sec   = %d\n", tmp->tm_sec); */
  /*   printf("    tm_min   = %d\n", tmp->tm_min); */
  /*   printf("    tm_hour  = %d\n", tmp->tm_hour); */
  /*   printf("    tm_mday  = %d\n", tmp->tm_mday); */
  /*   printf("    tm_mon   = %d\n", tmp->tm_mon); */
  /*   printf("    tm_year  = %d\n", tmp->tm_year); */
  /*   printf("    tm_wday  = %d\n", tmp->tm_wday); */
  /*   printf("    tm_yday  = %d\n", tmp->tm_yday); */
  /*   printf("    tm_isdst = %d\n", tmp->tm_isdst); */
  /* } */

  /* exit(EXIT_SUCCESS); */
}
