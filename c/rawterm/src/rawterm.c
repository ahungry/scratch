// Run a raw-mode app and render any received UDP data in the active view.
// https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html

/** includes **/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <arpa/inet.h>  /* IP address conversion stuff */
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <netdb.h>      /* gethostbyname */
#include <netinet/in.h> /* INET constants and stuff */
#include <pthread.h>
#include <stdio.h>      /* standard C i/o facilities */
#include <stdlib.h>     /* needed for atoi() */
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h> /* socket specific definitions */
#include <sys/types.h>  /* system data type definitions */
#include <termios.h>
#include <unistd.h>     /* defines STDIN_FILENO, system calls,etc */

#include "term.h"
#include "util.h"
#include "world.h"
#include "network.h"

/** defines **/

#define MY_VERSION "0.0.1"
#define CTRL_KEY(k) ((k) & 0x1f)

// Acts like a constructor
#define ABUF_INIT {NULL, 0}

enum editor_key
  {
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
  };

/** declarations **/

int editor_read_key ();
int get_cursor_position (int *rows, int *cols);

/** data **/


/** terminal **/


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

// Some of the incoming keys have more than 1 byte, even up to 2 or 3.
// This handles parsing the bytes that follow an escape sequence, then
// map them to special outbound enumerations.
int editor_read_key ()
{
  int nread;
  char c;

  while ((nread = read (STDIN_FILENO, &c, 1)) != 1)
    {
      if (nread == -1 && errno != EAGAIN) die ("read");
    }

  // A control code like C- was hit.
  if (c == '\x1b')
    {
      char seq[3];

      if (read (STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
      if (read (STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

      if (seq[0] == '[')
        {
          if (seq[1] >= 0 && seq[1] <= '9')
            {
              if (read (STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
              if (seq[2] == '~')
                {
                  switch (seq[1])
                    {
                    case '1': return HOME_KEY;
                    case '3': return DEL_KEY;
                    case '4': return END_KEY;
                    case '5': return PAGE_UP;
                    case '6': return PAGE_DOWN;
                    case '7': return HOME_KEY;
                    case '8': return END_KEY;
                    }
                }
            }
          else
            {
              // Parse the arrow keys
              switch (seq[1])
                {
                case 'A': return ARROW_UP;
                case 'B': return ARROW_DOWN;
                case 'C': return ARROW_RIGHT;
                case 'D': return ARROW_LEFT;
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
                }
            }
        }
      else if (seq[0] == 'O')
        {
          switch (seq[1])
            {
            case 'H': return HOME_KEY;
            case 'F': return END_KEY;
            }
        }

      return '\x1b';
    }

  return c;
}

/** output **/
void editor_draw_rows (struct abuf *ab)
{
  int y;

  for (y = 0; y < world.rows; y++)
    {
      // out_row_or_beyond_buffer (ab, y, world.numrows, world.rows, world.rowoff);
      clear_row (ab);
      // out_maybe_eol (ab, y, world.rows);
    }
}

void editor_refresh_screen ()
{
  struct abuf ab = ABUF_INIT;

  // Ultimately, the rows we draw etc. we would receive
  // from a remote data source, and run the refresh on receipt of it.
  cursor_hide (&ab);
  clear_and_reposition (&ab);
  editor_draw_rows (&ab);
  cursor_goto (&ab, world.cx, world.cy);
  cursor_show (&ab);

  ab_write (&ab);
  ab_free (&ab);
}

void editor_process_keypress ()
{
  int c = editor_read_key ();

  // Here we go
  struct addrinfo* res = 0;
  int fd = get_socket_fd (&res);
  send_udp (fd, res, c);
  // receive_udp (fd, res);
}


/** init **/

void init_world ()
{
  world.cx = 10;
  world.cy = 10;
  world.rowoff = 0;
  world.numrows = 0;
  world.row = NULL;

  if (get_window_size (&world.rows, &world.cols) == -1) die("get_window_size");
}

// Listen for incoming events.
#define MAXBUF 1024 * 1024

void echo (int sd)
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

int main (int argc, char *argv[])
{
  pthread_t pth;
  enable_raw_mode ();
  init_world ();
  // udp_listen ();

  if (argc < 4)
    {
      printf ("Too few arguments, please start with ./rawterm <listen port> <outbound host> <outbound port>\n");
      exit (1);
    }

  world.udp_listen_port = argv[1];
  world.udp_out_host = argv[2];
  world.udp_out_port = argv[3];

  pthread_create (&pth, NULL, udp_listen, NULL);

  while (1)
    {
      // editor_refresh_screen ();
      editor_process_keypress ();
    }

  return 0;
}
