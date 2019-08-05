#include "term.h"

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
