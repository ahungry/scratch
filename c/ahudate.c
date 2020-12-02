#define _XOPEN_SOURCE 700
#include <time.h>

// Add flag: -DNDEBUG
//   to disable assertions
#include <assert.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define DAY_SECONDS (24 * 60 * 60)
#define YEAR_SECONDS (DAY_SECONDS * 365)

// https://en.wikipedia.org/wiki/Year_2038_problem
// This calculation works only when leap year is used as reference,
// as the 'date' CLI program properly accounts for every 4th year (1974 etc.)
// adding the extra day, where my attempt here adds for each quarter of year
int64_t
ahudate_year_to_epoch (char * year)
{
  int64_t y = atoi (year);
  int64_t year_diff = y - 1970;
  int64_t year_leap = year_diff / 4;

  fprintf (stderr, "The year was: %ld\n", y);
  fprintf (stderr, "The leap years were: %ld\n", year_leap);

  // return (int64_t) ((y - 1970) * 365.25 * 24 * 60 * 60);
  return (int64_t) (year_diff * YEAR_SECONDS) + (year_leap * DAY_SECONDS);
}

typedef struct ahudate_datetime {
  int64_t y;
  int64_t m;
  int64_t d;
} ahudate_datetime_t;

int64_t
ahudate_epoch_to_datetime (int64_t n)
{
  int years = n / (365 * DAY_SECONDS);
  int leap_year_seconds = (years / 4) * DAY_SECONDS;
  int seconds_no_year = n - (years * 365 * DAY_SECONDS) - leap_year_seconds;
  int day = seconds_no_year / DAY_SECONDS;
  int month = 0;
  int year = 1970 + years;
  int leap = year % 4 == 0 ? 1 : 0;

  if (day >= 0 && day <= 31) { month = 1; day -= 0; }
  if (day > 31 && day <= 59 + leap) { month = 2; day -= 31; }
  if (day > 59 + leap && day <= 90 + leap) { month = 3; day -= 59 + leap; }
  if (day > 90 + leap && day <= 120 + leap) { month = 4; day -= 90 + leap; }
  if (day > 120 + leap && day <= 151 + leap) { month = 5; day -= 120 + leap; }
  if (day > 151 + leap && day <= 181 + leap) { month = 6; day -= 151 + leap; }
  if (day > 181 + leap && day <= 212 + leap) { month = 7; day -= 181 + leap; }
  if (day > 212 + leap && day <= 243 + leap) { month = 8; day -= 212 + leap; }
  if (day > 243 + leap && day <= 273 + leap) { month = 9; day -= 243 + leap; }
  if (day > 273 + leap && day <= 304 + leap) { month = 10; day -= 273 + leap; }
  if (day > 304 + leap && day <= 334 + leap) { month = 11; day -= 304 + leap; }
  if (day > 334 + leap && day <= 365 + leap) { month = 12; day -= 334 + leap; }

  // It always calculates to be one less than it is
  day++;

  fprintf (stderr, "years: %ld year: %d lys: %ld month: %ld day: %ld leap: %ld\n",
           (long) years,
           year,
           (long) leap_year_seconds,
           (long) month,
           (long) day,
           (long) leap);

  return 1970 + years;
}

int64_t
ahudate_datetime_to_epoch (ahudate_datetime_t * m)
{
  // calculate the year epoch
  // The leap year calculation seems to work, although basing it off epoch
  // seems odd in hindsight, as 1970 wasn't itself a leap year...
  int64_t year_diff = m->y - 1970;
  int64_t year_leap = year_diff / 4;
  int64_t epoch_year = (int64_t) (year_diff * YEAR_SECONDS) + (year_leap * DAY_SECONDS);

  // calculate the month epoch
  int64_t month_days = 0;

  switch (m->m)
    {
    case 1: month_days = 0; break;
    case 2: month_days = 31; break;
    case 3: month_days = 59; break;
    case 4: month_days = 90; break;
    case 5: month_days = 120; break;
    case 6: month_days = 151; break;
    case 7: month_days = 181; break;
    case 8: month_days = 212; break;
    case 9: month_days = 243; break;
    case 10: month_days = 273; break;
    case 11: month_days = 304; break;
    case 12: month_days = 334; break;
    }

  // Add the extra day if leap year
  if (m->y % 4 == 0 && m->m > 2)
    {
      month_days++;
    }

  int64_t epoch_month = (int64_t) month_days * DAY_SECONDS;
  int64_t epoch_days = (int64_t) (m->d - 1) * DAY_SECONDS;

  // return (int64_t) ((y - 1970) * 365.25 * 24 * 60 * 60);
  return epoch_year + epoch_month + epoch_days;
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
  is_capture,
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

    case is_capture:
      return 1;
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
  // int mask[];
  int * mask;
} ahudate_mask_t;

ahudate_mask_t *
make_ahudate_mask (char *s)
{
  char c;
  int slen = (int) strlen (s);
  ahudate_mask_t * m = malloc (sizeof (ahudate_mask_t));

  m->mask = malloc (slen * sizeof (int));
  m->size = 0;

  for (int i = 0; i < slen; i++)
    {
      c = s[i];

      switch (c)
        {
        case '_':
          m->mask[i] = is_capture;
          break;
        case 'd':
          m->mask[i] = is_numeric;
          break;
        case '/':
          m->mask[i] = is_separator;
          break;
        }
      m->size = i + 1;
    }

  return m;
}

void
unmake_ahudate_mask (ahudate_mask_t * m)
{
  free (m->mask);
  free (m);
}

char *
ahudate_mask_capture (char *s, char *mask)
{
  char *o = malloc (1);
  ahudate_mask_t * m = make_ahudate_mask (mask);

  if (NULL == realloc (o, sizeof (char)))
    {
      fprintf (stderr, "Failed to realloc!\n");
    }

  int c = 0;

  for (int i = 0; i < m->size; i++)
    {
      if (! ahudate_is_x (s[i], m->mask[i]))
        {
          return NULL;
        }

      if (is_capture == m->mask[i])
        {
          if (NULL == realloc (o, sizeof (char) * (c + 2)))
            {
              fprintf (stderr, "Failed to realloc!\n");
            }

          o[c++] = s[i];
          o[c] = '\0';
        }
    }
  unmake_ahudate_mask (m);

  return o;
}

int
ahudate_is_yyyy_mm_dd (char *s)
{
  ahudate_mask_t * m = make_ahudate_mask ("dddd/dd/dd");
  int n = ahudate_is_mask_valid (s, m->mask, m->size);
  unmake_ahudate_mask (m);

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

// Test output like this
//   date -d "1974-01-01 00:00:00+0" "+%s"
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

  /* RFC-3339 format */
  // strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", gmtime(t));

  /* ISO-8601 local time */
  // strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%S", localtime(t));

  /* RFC-822/RFC-2822 format */
  // strftime(buf, sizeof(buf), "%a, %d %b %Y %H:%M:%S %z", localtime(t));

  //   struct tm * time;
  time_t t;
  struct tm tm;
  // char *buf = "Wed, 02 Oct 2002 13:00:00 GMT";
  // strptime (buf, "%a, %d %b %Y %H:%M:%S %z", time);
  if (strptime ("6 Dec 2001 12:33:45", "%d %b %Y %H:%M:%S", &tm) == NULL)
    {
      fprintf (stderr, "Failed to run strptime!\n");
      exit (EXIT_FAILURE);
    }

  t = mktime (&tm);
  fprintf (stderr, "The date in epoch is: %ld\n", (long) t);

  // Turn a dt into an epoch
  ahudate_datetime_t * dt = malloc (sizeof (ahudate_datetime_t));
  dt->y = 2020;
  dt->m = 2;
  dt->d = 29;
  int64_t dt_epoch = ahudate_datetime_to_epoch (dt);
  fprintf (stderr, "The datetime in epoch is: %ld\n", (long) dt_epoch);

  // Turn an epoch into a dt
  int64_t year = ahudate_epoch_to_datetime (dt_epoch);
  fprintf (stderr, "The year from epoch datetime is: %ld\n", (long) year);

  // ahudate_mask_t * m = make_ahudate_mask ("____/dd/dd");
  // char *mask_capture = malloc (sizeof (char));
  char *mask_capture = ahudate_mask_capture ("1982/10/09", "____/dd/dd");
  // unmake_ahudate_mask (m);
  assert (strcmp (mask_capture, "1982") == 0);
  assert (strcmp (ahudate_mask_capture ("1982/10/09", "____/dd/dd"), "1982") == 0);
  assert (strcmp (ahudate_mask_capture ("1982/10/09", "dddd/__/dd"), "10") == 0);
  assert (strcmp (ahudate_mask_capture ("1982/10/09", "dddd/dd/__"), "09") == 0);
  assert (strcmp (ahudate_mask_capture ("1982-10-09", "____-dd-dd"), "1982") == 0);
  assert (strcmp (ahudate_mask_capture ("1982-10-09", "dddd-__-dd"), "10") == 0);
  assert (strcmp (ahudate_mask_capture ("1982-10-09", "dddd-dd-__"), "09") == 0);
  fprintf (stderr, "Result of mask capture is: %s\n", mask_capture);

  exit (EXIT_SUCCESS);
}
