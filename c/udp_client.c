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

void send_udp (int fd, struct addrinfo* res, char *s);

void die (const char *s) { perror (s); exit (1); }

int get_socket_fd (struct addrinfo** return_res)
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

  // Return the res by setting in place
  *return_res = res;

  /* const char content[30] = "Hello world"; */
  /* if (sendto (fd, content, sizeof (content), 0, */
  /*             res->ai_addr, res->ai_addrlen) == -1) */
  /*   { */
  /*     die("sendto"); */
  /*   } */

  // send_udp (fd, res);

  return fd;
}

// Fire and forget some udp (we don't get anything back)
void send_udp (int fd, struct addrinfo* res, char *s)
{
  // const char content[30] = "Hello world";
  // char content[30];
  char *content = s;
  // memcpy (content, s, strlen (s));
  if (sendto (fd, content, strlen (content), 0,
              res->ai_addr, res->ai_addrlen) == -1)
    {
      die("sendto");
    }
}

// Receive a udp we previously sent out
void receive_udp (int fd, struct addrinfo* res)
{
  int n;
  char buf[100];
  uint len;
  len = sizeof (res);
  n = recvfrom (fd, buf, 100, 0, (struct sockaddr *) &res, &len);

  if (n < 0) die("recvfrom");
  printf ("Got %d bytes\n", n);
}

int main (int argc, char *argv[])
{
  if (argc < 2)
    {
      printf ("Please pass in a string to send!\n");
      exit (1);
    }

  struct addrinfo* res = 0;
  int fd = get_socket_fd (&res);
  send_udp (fd, res, argv[1]);
  receive_udp (fd, res);

  return 0;
}
