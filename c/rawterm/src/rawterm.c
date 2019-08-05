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
