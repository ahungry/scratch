#include "term.h"
#include "world.h"

// clear everything at end of row
void
clear_row (abuf *ab)
{
  ab_append (ab, "\x1b[K", 3);
}

void
cursor_hide (abuf *ab)
{
  ab_append (ab, "\x1b[?25l", 6);
}

void
cursor_show (abuf *ab)
{
  ab_append (ab, "\x1b[?25h", 6);
}

// TODO: Ensure x or y does not go out of bounds of window.
void
cursor_goto (abuf *ab, int x, int y)
{
  char buf[32];

  snprintf (buf, sizeof (buf), "\x1b[%d;%dH", y + 1, x + 1);
  ab_append (ab, buf, strlen (buf));
}

// TODO: What didn't it like about this?
void
xcursor_goto (abuf *ab, int x, int y)
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

void
clear_screen (abuf *ab)
{
  ab_append (ab, "\x1b[2J", 4); // Clear screen
}

void
clear_and_reposition (abuf *ab)
{
  clear_screen (ab);
  cursor_goto (ab, 1, 1);
}

void
disable_raw_mode ()
{
  if (-1 == tcsetattr (STDIN_FILENO, TCSAFLUSH, &world.orig_termios))
    die ("tcsetattr");
}

void
enable_raw_mode ()
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

int
get_cursor_position (int *rows, int *cols)
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

int
get_window_size (int *rows, int *cols)
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
