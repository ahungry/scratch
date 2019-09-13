#include<stdlib.h>
#include "structs.h"
 extern "C" void F(struct S *s) { s->a += s->a; s->b *= s->b; }
