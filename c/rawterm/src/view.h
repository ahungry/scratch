#ifndef RT_VIEW_H
#define RT_VIEW_H

#include "util.h"

// Receive a udp we previously sent out
// Some of the incoming keys have more than 1 byte, even up to 2 or 3.
// This handles parsing the bytes that follow an escape sequence, then
// map them to special outbound enumerations.
int
editor_read_key ();

/** output **/
void
editor_draw_rows (abuf *ab);

void
editor_refresh_screen ();

void
editor_process_keypress ();

# endif
