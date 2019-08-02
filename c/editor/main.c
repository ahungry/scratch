// https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html

/** includes **/

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

/** defines **/

#define CTRL_KEY(k) ((k) & 0x1f)

/** data **/

struct termios orig_termios;

/** terminal **/

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

/** input **/
void editor_process_keypress ()
{
  char c = editor_read_key ();

  // TODO: Here, we would want to send it out / process it.
  switch (c)
    {
    case CTRL_KEY('q'):
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
      editor_process_keypress ();
    }

  return 0;
}
