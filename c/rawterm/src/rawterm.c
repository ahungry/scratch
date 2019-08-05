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
#include "view.h"

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
