#ifndef RT_TERM_H
#define RT_TERM_H

#include <stdio.h>
#include <stdlib.h>     /* needed for atoi() */
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>     /* defines STDIN_FILENO, system calls,etc */

#include "util.h"
#include "world.h"

// clear everything at end of row
void
clear_row (struct abuf *ab);

void
cursor_hide (struct abuf *ab);

void
cursor_show (struct abuf *ab);

void
cursor_goto (struct abuf *ab, int x, int y);

void
xcursor_goto (struct abuf *ab, int x, int y);

void
clear_screen (struct abuf *ab);

void
clear_and_reposition (struct abuf *ab);

void
disable_raw_mode ();

void
enable_raw_mode ();

int
get_cursor_position (int *rows, int *cols);

int
get_window_size (int *rows, int *cols);

#endif
