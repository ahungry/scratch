CC = gcc
CFLAGS = -c -Wall -std=c99
LDFLAGS =

all: memory-eater

memory-eater: memory-eater.o
	$(CC) $< $(LDFLAGS) -o $@

memory-eater.o: memory-eater.c
	$(CC) $(CFLAGS) $<

docker-build:
	docker build -f Dockerfile -t memory-eater .

docker-run:
	docker run --memory-swappiness=1 --name=memory-eater --rm -it -m4g memory-eater

freeze:
	docker pause memory-eater
	docker update --memory-swap=4g --memory=4m memory-eater

unfreeze:
	docker update --memory-swap=8g --memory=4g memory-eater
	docker unpause memory-eater

xfreeze:
	sudo swapon -a
	docker pause memory-eater
	docker update --memory-swap=4g --memory=4m memory-eater

xunfreeze:
	docker update --memory-swap=8g --memory=4g memory-eater
	sudo swapoff -a
	sudo swapon -a
	docker unpause memory-eater
