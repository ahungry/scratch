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

void die (const char *s);
int editor_read_key ();
int get_cursor_position (int *rows, int *cols);

/** data **/

typedef struct erow
{
  int size;
  char *chars;
} erow;

struct world_atom
{
  int cx, cy;
  int rowoff;
  int rows;
  int cols;
  int numrows;
  erow *row;
  struct termios orig_termios;
};

struct world_atom world;

/** util **/

// COL / ROW

// https://stackoverflow.com/questions/8257714/how-to-convert-an-int-to-string-in-c#8257728

typedef struct abuf
{
  char *b;
  int len;
} abuf;

#define ABUF_INIT {NULL, 0}

void ab_append (struct abuf *ab, const char *s, int len)
{
  char *new = realloc (ab->b, ab->len + len);

  if (new == NULL) return;

  memcpy (&new[ab->len], s, len);
  ab->b = new;
  ab->len += len;
}

void ab_write (struct abuf *ab)
{
  write (STDOUT_FILENO, ab->b, ab->len);
}

void ab_free (struct abuf *ab)
{
  free (ab->b);
}

int get_byte_size_of_int_as_char (int n)
{
  return (int) ((ceil (log10 (n)) + 1) * sizeof (char));
}

// clear everything at end of row
void clear_row (struct abuf *ab)
{
  ab_append (ab, "\x1b[K", 3);
}

void cursor_hide (struct abuf *ab)
{
  ab_append (ab, "\x1b[?25l", 6);
}

void cursor_show (struct abuf *ab)
{
  ab_append (ab, "\x1b[?25h", 6);
}

// TODO: Ensure x or y does not go out of bounds of window.
void cursor_goto (struct abuf *ab, int x, int y)
{
  char buf[32];

  snprintf (buf, sizeof (buf), "\x1b[%d;%dH", y + 1, x + 1);
  ab_append (ab, buf, strlen (buf));
}

// TODO: What didn't it like about this?
void xcursor_goto (struct abuf *ab, int x, int y)
{
  int xlen = get_byte_size_of_int_as_char (x);
  int ylen = get_byte_size_of_int_as_char (y);
  int olen = 4; // \x1b [ H, and the ;
  int len = xlen + ylen + olen;
  char str[len]; // Slot for \x1b[_;_H

  sprintf (str, "\x1b[%d;%dH", x, y);
  // write (STDOUT_FILENO, str, len); // Position cursor 12;40H would center on an 80x24 size
  ab_append (ab, str, len);
}

void clear_screen (struct abuf *ab)
{
  ab_append (ab, "\x1b[2J", 4); // Clear screen
}

void clear_and_reposition (struct abuf *ab)
{
  clear_screen (ab);
  cursor_goto (ab, 1, 1);
}

/** terminal **/

void die (const char *s)
{
  struct abuf ab = ABUF_INIT;

  clear_and_reposition (&ab);
  ab_free (&ab);
  perror (s);
  exit (1);
}

void disable_raw_mode ()
{
  if (-1 == tcsetattr (STDIN_FILENO, TCSAFLUSH, &world.orig_termios))
    die ("tcsetattr");
}

void enable_raw_mode ()
{
  if (-1 == tcgetattr (STDIN_FILENO, &world.orig_termios)) die("tcgetattr");
  atexit (disable_raw_mode);

  struct termios raw = world.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  // Changes \n into \r\n as default when left off
  // raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  // raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1; // Every 10th of second redraw / skip the read (stop block).

  if (-1 == tcsetattr (STDIN_FILENO, TCSAFLUSH, &raw)) die("tcsetattr");
}

int get_window_size (int *rows, int *cols)
{
  struct winsize ws;

  if (ioctl (STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
    {
      // Fallback for non-ioctl supporting systems
      if (write (STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;

      return get_cursor_position (rows, cols);
    }
  else
    {
      *cols = ws.ws_col;
      *rows = ws.ws_row;

      return 0;
    }
}

// TODO Do not hardcode the outbound etc
int get_socket_fd (struct addrinfo** return_res)
{
  const char* hostname = "127.0.0.1"; /* localhost */
  const char* portname = "12346";
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
void send_udp (int fd, struct addrinfo* res, int c)
{
  char buf[32];

  snprintf (buf, sizeof (buf), "%x", c);

  if (sendto (fd, buf, strlen (buf), 0,
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
                    case '5': return  PAGE_UP;
                    case '6': return  PAGE_DOWN;
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

int get_cursor_position (int *rows, int *cols)
{
  char buf[32];
  unsigned int i = 0;

  if (write (STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

  while (i < sizeof (buf) - 1)
    {
      if (read (STDIN_FILENO, &buf[i], 1) != 1) break;
      if (buf[i] == 'R') break;
      i++;
    }
  buf[i] = '\0';

  // printf ("\r\n&buf[1]: '%s'\r\n", &buf[1]);
  if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  if (sscanf (&buf[2], "%d;%d", rows, cols) != 2) return -1;

  return 0;
}

/** output **/
void editor_draw_rows (struct abuf *ab)
{
  ab_append (ab, "Hello world", 11);
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
#define MAXBUF 1024*1024

void echo (int sd)
{
    uint len;
    int n;
    char bufin[MAXBUF];
    struct sockaddr_in remote;

    /* need to know how big address struct is, len must be set before the
       call to recvfrom!!! */

    len = sizeof(remote);

    while (1) {
      /* read a datagram from the socket (put result in bufin) */
      n=recvfrom(sd,bufin,MAXBUF,0,(struct sockaddr *)&remote,&len);

      /* print out the address of the sender */
      /* printf("Got a datagram from %s port %d\n", */
      /*        inet_ntoa(remote.sin_addr), ntohs(remote.sin_port)); */

      if (n<0) {
        perror("Error receiving data");
      } else {
        // printf("GOT %d BYTES\n",n);
        /* Got something, just send it back */
        sendto(sd,bufin,n,0,(struct sockaddr *)&remote,len);

        struct abuf ab = ABUF_INIT;

        // Ultimately, the rows we draw etc. we would receive
        // from a remote data source, and run the refresh on receipt of it.
        cursor_hide (&ab);
        clear_and_reposition (&ab);
        ab_append (&ab, bufin, strlen (bufin));
        // editor_draw_rows (&ab);
        cursor_goto (&ab, world.cx, world.cy);
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

  skaddr.sin_family = AF_INET;
  skaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  skaddr.sin_port = htons(12345); // 0 for the kernel to choose random

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
  pthread_create (&pth, NULL, udp_listen, NULL);
  enable_raw_mode ();
  init_world ();
  // udp_listen ();

  if (argc >= 2)
    {
      // editor_open (argv[1]);
      // printf ("%s", argv[1]);
    }

  while (1)
    {
      // editor_refresh_screen ();
      editor_process_keypress ();
    }

  return 0;
}
