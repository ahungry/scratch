#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>

int
main ()
{
  int sock = 0;
  int n = 0;

  char buf[1024];

  struct sockaddr_in remote;

  memset (buf, '0', sizeof (buf));

  sock = socket (AF_INET, SOCK_STREAM, 0);

  if (0 > sock)
    {
      printf ("Socket failure\n");

      return 1;
    }

  remote.sin_family = AF_INET;
  remote.sin_port = htons (12345);
  remote.sin_addr.s_addr = inet_addr ("127.0.0.1");

  if (connect (sock, (struct sockaddr *) &remote, sizeof (remote)))
    {
      printf ("Could not connect to that host and port\n");

      return 1;
    }

  while ((n = read (sock, buf, sizeof (buf) -1)) > 0)
    {
      buf[n] = 0;

      if (fputs (buf, stdout) == EOF)
        {
          printf ("\nStdout error");
        }

      printf ("\n");
    }

  if (n < 0)
    {
      printf ("Standard input error\n");
    }

  return 0;
}
