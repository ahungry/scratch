// Start a process, send a single UDP to somewhere, and end.
// http://www.microhowto.info/howto/send_a_udp_datagram_in_c.html

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/uio.h>

void die (const char *s) { perror (s); exit (1); }

// Fire and forget some udp (we don't get anything back)
// I guess we would have to connect() or wait to read a response from the remote?
void send_udp ()
{
  const char* hostname = "127.0.0.1"; /* localhost */
  const char* portname = "12345";
  struct addrinfo hints;
  memset (&hints, 0, sizeof (hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_DGRAM;
  hints.ai_protocol = 0;
  hints.ai_flags = AI_ADDRCONFIG;
  struct addrinfo* res = 0;
  int err = getaddrinfo (hostname, portname, &hints, &res);
  if (err != 0) die("getaddrinfo");
  int fd = socket (res->ai_family, res->ai_socktype, res->ai_protocol);
  if (fd == -1) die("socket");
  const char content[30] = "Hello world";
  if (sendto (fd, content, sizeof (content), 0,
              res->ai_addr, res->ai_addrlen) == -1)
    {
      die("sendto");
    }
}

int main ()
{
  send_udp ();
  exit (0);
}
