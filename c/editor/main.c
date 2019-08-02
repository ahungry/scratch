// https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html

/** includes **/

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

/** defines **/

#define MY_VERSION "0.0.1"
#define CTRL_KEY(k) ((k) & 0x1f)

/** declarations **/

void die (const char *s);
char editor_read_key ();
int get_cursor_position (int *rows, int *cols);

/** data **/

struct world_atom
{
  int cx, cy;
  int rows;
  int cols;
  struct termios orig_termios;
};

struct world_atom world;

/** util **/

// COL / ROW

// https://stackoverflow.com/questions/8257714/how-to-convert-an-int-to-string-in-c#8257728

struct abuf
{
  char *b;
  int len;
};

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

// Alt implementation (shove in a oversized buffer, use snprintf to recompute):
// char buf[32];
// snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cy + 1, E.cx + 1);
// abAppend(&ab, buf, strlen(buf));
void cursor_goto (struct abuf *ab, int x, int y)
{
  char buf[32];

  snprintf (buf, sizeof (buf), "\x1b[%d;%dH", y, x);
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
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
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

char editor_read_key ()
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
          // Parse the arrow keys
          switch (seq[1])
            {
            case 'A': return 'w';
            case 'B': return 's';
            case 'C': return 'd';
            case 'D': return 'a';
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

// Calculation to center the screen
int get_padding (int cols, int len) { return (cols - len) / 2; }

// Write the anchor, then pad out to the middle.
void do_padding (struct abuf *ab, int pad)
{
  if (pad)
    {
      ab_append (ab, "~", 1);
      pad--;
    }
  while (pad--) ab_append (ab, " ", 1);
}

/** output **/
void editor_draw_rows (struct abuf *ab)
{
  int y;

  for (y = 0; y < world.rows; y++)
    {
      if (y == world.rows / 3)
        {
          char welcome[80];
          int welcomelen = snprintf (welcome,  sizeof (welcome),
                                     "xxx -- version %s", MY_VERSION);
          if (welcomelen > world.cols) welcomelen = world.cols;
          int padding = get_padding (world.cols, welcomelen);
          do_padding (ab, padding);
          ab_append (ab, welcome, welcomelen);
        }
      else
        {
          // Write empty line marker
          ab_append (ab, "~", 1);
        }
      clear_row (ab);

      if (y < world.rows -1)
        {
          ab_append (ab, "\r\n", 2);
        }
    }
}

void editor_refresh_screen ()
{
  struct abuf ab = ABUF_INIT;

  // Ultimately, the rows we draw etc. we would receive
  // from a remote data source, and run the refresh on receipt of it.
  cursor_hide (&ab);
  editor_draw_rows (&ab);
  cursor_goto (&ab, world.cx, world.cy);
  cursor_show (&ab);

  ab_write (&ab);
  ab_free (&ab);
}

/** input **/

void editor_move_cursor (char key)
{
  switch (key)
    {
    case 'a': world.cx--; break;
    case 'd': world.cx++; break;
    case 'w': world.cy--; break;
    case 's': world.cy++; break;
    }
}

void editor_process_keypress ()
{
  struct abuf ab = ABUF_INIT;
  char c = editor_read_key ();

  // TODO: Here, we would want to send it out / process it.
  switch (c)
    {
    case CTRL_KEY('q'):
      clear_and_reposition (&ab);
      ab_write (&ab);
      ab_free (&ab);
      exit (0);
      break;

    case 'w':
    case 'a':
    case 's':
    case 'd':
      editor_move_cursor (c);
      break;
    }
}


/** init **/

void init_world ()
{
  world.cx = 10;
  world.cy = 10;

  if (get_window_size (&world.rows, &world.cols) == -1) die("get_window_size");
}

int main ()
{
  enable_raw_mode ();
  init_world ();

  while (1)
    {
      editor_refresh_screen ();
      editor_process_keypress ();
    }

  return 0;
}
