// A program that outputs dynamic so and reloads on the fly
// https://stackoverflow.com/questions/10564670/is-there-any-way-to-compile-additional-code-at-runtime-in-c-or-c
#include <iostream>
#include <fstream>
#include <dlfcn.h>
#include <stdlib.h>
#include <cstdint>

#include "structs.h"

using namespace std;

int main ( int argc, char **argv ) {

    // create own program
    ofstream f ( "tmp.cpp" );
    f << "#include<stdlib.h>\n#include \"structs.h\"\n extern \"C\" void F(S &s) { s.a += s.a; s.b *= s.b; }\n";
    f.close();

    // create library
    system ( "/usr/bin/gcc -shared tmp.cpp -o libtmp.so" );

    // load library
    void * fLib = dlopen ( "./libtmp.so", RTLD_LAZY );
    if ( !fLib ) {
        cerr << "Cannot open library: " << dlerror() << '\n';
    }

    if ( fLib ) {
        void* ptr = dlsym ( fLib, "F" );
        int ( *fn ) ( S & ) = (int*)((intptr_t) ptr);

        if ( fn ) {
            for(int i=0;i<11;i++) {
                S s;
                s.a = i;
                s.b = i;

                // use function
                fn(s);
                cout << s.a << " " << s.b << endl;
            }
        }
        dlclose ( fLib );
    }

    return 0;
}
