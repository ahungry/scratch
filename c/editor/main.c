// https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html

/** includes **/

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <math.h>

/** defines **/

#define CTRL_KEY(k) ((k) & 0x1f)

/** data **/

struct termios orig_termios;

/** util **/

// COL / ROW
void die (const char *s);

// https://stackoverflow.com/questions/8257714/how-to-convert-an-int-to-string-in-c#8257728
int get_byte_size_of_int_as_char (int n)
{
  return (int) ((ceil (log10 (n)) + 1) * sizeof (char));
}

void cursor_goto (int x, int y)
{
  int xlen = get_byte_size_of_int_as_char (x);
  int ylen = get_byte_size_of_int_as_char (y);
  int olen = 4; // \x1b [ H, and the ;
  char str[xlen + ylen + olen]; // Slot for \x1b[_;_H

  sprintf (str, "\x1b[%d;%dH", x, y);
  write (STDOUT_FILENO, "\x1b[H", 3); // Position cursor 12;40H would center on an 80x24 size
}

void clear_and_reposition ()
{
  write (STDOUT_FILENO, "\x1b[2J", 4); // Clear screen
  cursor_goto (1, 1);
  write (STDOUT_FILENO, "\x1b[H", 3); // Position cursor 12;40H would center on an 80x24 size
}

/** terminal **/

void die (const char *s)
{
  clear_and_reposition ();
  perror (s);
  exit (1);
}

void disable_raw_mode ()
{
  if (-1 == tcsetattr (STDIN_FILENO, TCSAFLUSH, &orig_termios))
    die ("tcsetattr");
}

void enable_raw_mode ()
{
  if (-1 == tcgetattr (STDIN_FILENO, &orig_termios)) die("tcgetattr");
  atexit (disable_raw_mode);

  struct termios raw = orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1; // Every 10th of second redraw / skip the read (stop block).

  if (-1 == tcsetattr (STDIN_FILENO, TCSAFLUSH, &raw)) die("tcsetattr");
}

char editor_read_key ()
{
  int nread;
  char c;

  while ((nread = read (STDIN_FILENO, &c, 1)) != 1)
    {
      if (nread == -1 && errno != EAGAIN) die ("read");
    }

  return c;
}

/** output **/
void editor_draw_rows ()
{
  int y;

  for (y = 0; y < 24; y++)
    {
      write (STDOUT_FILENO, "~\r\n", 3);
    }
}

void editor_refresh_screen ()
{
  clear_and_reposition ();
  editor_draw_rows ();
  cursor_goto (1, 1);
}

/** input **/
void editor_process_keypress ()
{
  char c = editor_read_key ();

  // TODO: Here, we would want to send it out / process it.
  switch (c)
    {
    case CTRL_KEY('q'):
      clear_and_reposition ();
      exit (0);
      break;
    }
}


/** init **/
int main ()
{
  enable_raw_mode ();

  while (1)
    {
      editor_refresh_screen ();
      editor_process_keypress ();
    }

  return 0;
}
