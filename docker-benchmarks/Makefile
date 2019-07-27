alpine-build-clojure:
	docker build -f Dockerfile_clojure -t bench-alpine-clojure .

alpine-run-clojure:
	docker run --rm -it -p 3010:3000 bench-alpine-clojure

alpine-build-php:
	docker build \
	--no-cache \
	-f Dockerfile_alpine_php \
	-t bench-alpine-php .

alpine-run-php:
	docker run \
	--rm \
	-it \
	-p 3003:80 \
	bench-alpine-php

alpine-build:
	docker build \
	--no-cache \
	-f Dockerfile_alpine \
	-t bench-alpine .

alpine-run:
	docker run \
	--rm \
	-it \
	-p 3001:3000 \
	bench-alpine

ubuntu-build:
	docker build \
	--no-cache \
	-f Dockerfile_ubuntu \
	-t bench-ubuntu .

ubuntu-run:
	docker run \
	--rm \
	-it \
	-p 3002:3000 \
	bench-ubuntu

## Build stuff for custom C http server
CC?= gcc

CFLAGS = -Wall -c -std=gnu99 -I/usr/local/include
LDFLAGS = -L/usr/local/lib -Wl,-rpath=/usr/local/lib -pthread

EXE = bin/5m-ms

all: $(EXE)

$(EXE): main.o
	$(CC) $(LDFLAGS) $< -o $@

main.o: main.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	-rm *.o
	-rm $(EXE)

start: all
	./bin/5m-ms 12003

docker-build:
	docker build -f Dockerfile_c -t 5m-ms-c-vanilla .

docker-run:
	docker run --rm -it -p 3005:12003 5m-ms-c-vanilla
