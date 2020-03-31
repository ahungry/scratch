// https://www.cairographics.org/tutorial/
// https://www.cairographics.org/manual/cairo-PNG-Support.html

// The main cairo site keeps timing out, the only worthwhile sample was
// under the ats lang page (hah!)

// http://ats-lang.sourceforge.net/DOCUMENT/ATS2CAIRO/HTML/c36.html

// #include "/usr/include/cairo/cairo.h"
#include <cairo.h>
#include <stdio.h>
#include <math.h>

int
main (int argc, char *argv[])
{
  int width = 300, height = 300;
  cairo_surface_t *surface =
    cairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
  cairo_t *ctx = cairo_create (surface);

  // http://zetcode.com/gfx/cairo/basicdrawing/
  // Draw a circle
  cairo_set_line_width (ctx, 9);
  cairo_set_source_rgb (ctx, 0.69, 0.19, 0);
  cairo_translate (ctx, width / 2, height / 2);
  cairo_arc (ctx, 0, 0, 100, 0, 2 * M_PI);
  cairo_stroke_preserve (ctx);
  cairo_set_source_rgb (ctx, 0.3, 0.4, 0.6);
  cairo_fill (ctx);

  // Draw some hello world text
  cairo_select_font_face (ctx, "Sans",
                          CAIRO_FONT_SLANT_NORMAL,
                          CAIRO_FONT_WEIGHT_BOLD);
  cairo_set_font_size (ctx, 32.0);
  cairo_set_source_rgb (ctx, 1.0, 1.0, 1.0);
  cairo_move_to (ctx, - width / 3, 0);
  cairo_show_text (ctx, "..Cairo Time..");

  // Draw some lines out from center of circle?
  cairo_set_line_width (ctx, 2);
  cairo_set_source_rgb (ctx, 0.0, 1.0, 0.0);
  cairo_move_to (ctx, 0.0, 0.0);
  cairo_line_to (ctx, width, height);
  cairo_stroke (ctx);

  const char *filename = "/tmp/dummy.png";

  cairo_status_t status = cairo_surface_write_to_png (surface, filename);
  cairo_destroy (ctx);
  cairo_surface_destroy (surface);

  printf ("The status is: %d\n", status);

  return 0;
}
