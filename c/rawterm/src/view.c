#include "network.h"
#include "term.h"
#include "view.h"
#include "world.h"

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

// Receive a udp we previously sent out
// Some of the incoming keys have more than 1 byte, even up to 2 or 3.
// This handles parsing the bytes that follow an escape sequence, then
// map them to special outbound enumerations.
int
editor_read_key ()
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
void
editor_draw_rows (abuf *ab)
{
  int y;

  for (y = 0; y < world.rows; y++)
    {
      // out_row_or_beyond_buffer (ab, y, world.numrows, world.rows, world.rowoff);
      clear_row (ab);
      // out_maybe_eol (ab, y, world.rows);
    }
}

void
editor_refresh_screen ()
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

void
editor_process_keypress ()
{
  int c = editor_read_key ();

  // Here we go
  struct addrinfo* res = 0;
  int fd = get_socket_fd (&res);

  send_udp (fd, res, c);

  close (fd);
  // receive_udp (fd, res);
}
