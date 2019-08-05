#ifndef RT_TERM_H
#define RT_TERM_H

#include <stdio.h>
#include <string.h>

#include "util.h"

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

#endif
