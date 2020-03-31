all: udp.bin udp_client.bin

udp.bin: udp.c
	gcc -Wall udp.c -lm -ldl -o udp.bin

udp_client.bin: udp_client.c
	gcc -Wall udp_client.c -lm -ldl -o udp_client.bin

assertx.bin: assert.c
	gcc -Wall $< -lm -ldl -o $@

assert.bin: assert.c
	gcc -DNDEBUG -Wall $< -lm -ldl -o $@

cairo.bin: cairo.c
	gcc -Wall $(shell pkg-config --libs cairo) -I/usr/include/cairo -o $@ $<

docker-build-cairo:
	docker build -t alpine:cairo . -f Dockerfile_cairo

docker-run-cairo: docker-build-cairo
	$(info Use: docker cp alpine_cairo:/tmp/dummy.png ./dummy.png)
	docker run --name alpine_cairo --rm -it alpine:cairo
