default: tinywm

tinywm:
	gcc -Wall `pkgconf --libs --cflags x11` ./tinywm.c -o tinywm

xephyr-start:
	Xephyr -ac -br -noreset -screen 800x600 :1

xephyr-run:
	DISPLAY=:1 ./tinywm
