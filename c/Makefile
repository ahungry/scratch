all: udp.bin udp_client.bin

udp.bin: udp.c
	gcc -Wall udp.c -lm -ldl -o udp.bin

udp_client.bin: udp_client.c
	gcc -Wall udp_client.c -lm -ldl -o udp_client.bin

assertx.bin: assert.c
	gcc -Wall $< -lm -ldl -o $@

assert.bin: assert.c
	gcc -DNDEBUG -Wall $< -lm -ldl -o $@
