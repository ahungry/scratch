#include "network.h"
#include "term.h"
#include "util.h"
#include "world.h"

// Listen for incoming events.
#define MAXBUF 1024 * 1024

// fd = socket file descriptor

int
get_socket_fd (struct addrinfo** return_res)
{
  char* hostname = world.udp_out_host; // "localhost"
  char* portname = world.udp_out_port; // "12346"

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

void
echo (int sd)
{
    uint len;
    struct sockaddr_in remote;

    /* need to know how big address struct is, len must be set before the
       call to recvfrom!!! */

    len = sizeof (remote);

    while (1) {
      char bufin[MAXBUF];
      int n; // Received bytes

      /* read a datagram from the socket (put result in bufin) */
      n = recvfrom (sd, bufin, MAXBUF, 0, (struct sockaddr *) &remote, &len);

      /* print out the address of the sender */
      /* printf("Got a datagram from %s port %d\n", */
      /*        inet_ntoa(remote.sin_addr), ntohs(remote.sin_port)); */

      if (n < 0)
        {
          perror ("Error receiving data");
        }
      else
        {
          // printf("GOT %d BYTES\n",n);
          /* Got something, just send it back */
          sendto (sd, bufin, n, 0, (struct sockaddr *) &remote,len);

          abuf ab = ABUF_INIT;

          // Ultimately, the rows we draw etc. we would receive
          // from a remote data source, and run the refresh on receipt of it.
          cursor_hide (&ab);
          clear_and_reposition (&ab);
          cursor_goto (&ab, world.cx, world.cy); // 0,0
          ab_append (&ab, bufin, n); // Append all received bytes
          cursor_show (&ab);

          ab_write (&ab);
          ab_free (&ab);
        }
    }
}

void *
udp_listen ()
{
  int ld;
  struct sockaddr_in skaddr;
  uint length;

  /* create a socket
     IP protocol family (PF_INET)
     UDP protocol (SOCK_DGRAM)
  */

  if ((ld = socket( PF_INET, SOCK_DGRAM, 0 )) < 0) {
    printf("Problem creating socket\n");
    exit(1);
  }

  /* establish our address
     address family is AF_INET
     our IP address is INADDR_ANY (any of our IP addresses)
     the port number is assigned by the kernel
  */

  int listen_port = atoi (world.udp_listen_port);

  skaddr.sin_family = AF_INET;
  skaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  skaddr.sin_port = htons(listen_port); // 0 for the kernel to choose random

  if (bind(ld, (struct sockaddr *) &skaddr, sizeof(skaddr))<0) {
    printf("Problem binding\n");
    exit(0);
  }

  /* find out what port we were assigned and print it out */

  length = sizeof( skaddr );
  if (getsockname(ld, (struct sockaddr *) &skaddr, &length)<0) {
    printf("Error getsockname\n");
    exit(1);
  }

  /* port number's are network byte order, we have to convert to
     host byte order before printing !
  */
  // printf("The server UDP port number is %d\n",ntohs(skaddr.sin_port));

  /* Go echo every datagram we get */
  echo(ld);

  return NULL;
}

void
receive_udp (int fd, struct addrinfo* res)
{
  int n;
  char buf[100];
  uint len;
  len = sizeof (res);
  n = recvfrom (fd, buf, 100, 0, (struct sockaddr *) &res, &len);

  if (n < 0) die("recvfrom");
  printf ("Got %d bytes\n", n);
}
