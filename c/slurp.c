#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char *
slurp (char *fn)
{
  FILE *fp;
  char *c;
  char *content = malloc (sizeof (char));
  char line[255];

  fp = fopen (fn, "r");

  while (NULL != (c = fgets (line, 255, fp)))
    {
      content = realloc (content, sizeof (char) * (strlen (c) + strlen (content)));
      strcat (content, c);
    }

  fclose (fp);

  return content;
}

int
main (int argc, char *argv[])
{
  if (argc < 2)
    {
      printf ("Add a file to slurp");

      return 1;
    }

  char *fn = argv[1];

  printf ("Slurping file: %s\n", fn);

  char *content = slurp (fn);

  printf ("Content is: %s\n", content);

  return 0;
}
