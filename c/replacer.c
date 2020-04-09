#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void
slurp (char *fn, char *find, char *replace)
{
  FILE *fp;
  char *c;
  char line[10000];
  char *pos = NULL;
  char *pre = NULL;
  char *post = NULL;

  int findlen = strlen (find);

  fp = fopen (fn, "r");

  while (NULL != (c = fgets (line, 255, fp)))
    {
      pos = (char *) &line;
      post = pos;

      while (NULL != (pos = strstr (post, find)))
        {
          int ipos = pos - post;

          pre = malloc (255);
          memcpy (pre, pos - ipos, ipos);
          printf ("%s%s", pre, replace);

          pos += findlen;
          post = pos;
        }

      printf ("%s", post);
    }

  fclose (fp);
}

int
main (int argc, char *argv[])
{
  if (argc < 4)
    {
      printf ("Usage: <file> <find> <replace>");

      return 1;
    }

  char *fn = argv[1];
  char *find = argv[2];
  char *replace = argv[3];

  // printf ("Slurping file: %s\n", fn);

  slurp (fn, find, replace);

  // printf ("Content is: %s\n", content);

  return 0;
}
