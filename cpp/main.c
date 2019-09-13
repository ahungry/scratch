// A program that outputs dynamic so and reloads on the fly
// Adapted from:
// https://stackoverflow.com/questions/10564670/is-there-any-way-to-compile-additional-code-at-runtime-in-c-or-c
#include <dlfcn.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "structs.h"

int main ( int argc, char **argv ) {

  // create own program
  char* src = "#include<stdlib.h>\n#include \"structs.h\"\n extern \"C\" void F(struct S *s) { s->a += s->a; s->b *= s->b; }\n";
  int len = strlen (src);
  FILE* fh = fopen ("tmp.cpp", "w+");
  fwrite (src, len, 1, fh);
  fclose (fh);

  // create library
  system ( "/usr/bin/gcc -shared tmp.cpp -o libtmp.so" );

  // load library
  void * fLib = dlopen ( "./libtmp.so", RTLD_LAZY );
  if ( !fLib ) {
    printf ("Oops");
    printf ("%s", dlerror ());
  }

  if ( fLib ) {
    void* ptr = dlsym ( fLib, "F" );
    int ( *fn ) ( struct S *s ) = (int*) ptr;

    if ( fn ) {
      for(int i=0;i<11;i++) {
        struct S s;
        s.a = i;
        s.b = i;

        // use function
        fn(&s);
        printf ("%d %d \n", s.a, s.b);
      }
    }
    dlclose ( fLib );
  }

  return 0;
}
