// https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

struct termios orig_termios;

void die (const char *s)
{
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

int main ()
{
  enable_raw_mode ();

  while (1)
    {
      char c = '\0';
      if (-1 == read (STDIN_FILENO, &c, 1) && errno != EAGAIN) die("read");

      // TODO: Here we want to scoop the input and send it to the remote.
      if (iscntrl (c))
        {
          printf ("%d %x\r\n", c, c); // is control character?
        }
      else
        {
          printf ("%d ('%c') %x\r\n", c, c, c);
        }

      if (c == 'q') break;
    }

  return 0;
}
