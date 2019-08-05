#include "world.h"
#include "util.h"
#include "network.h"

// TODO Do not hardcode the outbound etc
int
get_socket_fd (struct addrinfo** return_res)
{
  char* hostname = world.udp_out_host;
  char* portname = world.udp_out_port;
  /* const char* hostname = "127.0.0.1"; /\* localhost *\/ */
  /* const char* portname = "12346"; */
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
void
send_udp (int fd, struct addrinfo* res, int c)
{
  char buf[1];
  buf[0] = c;

  if (sendto (fd, buf, sizeof (buf), 0,
              res->ai_addr, res->ai_addrlen) == -1)
    {
      die("sendto");
    }
}
