CFLAGS=-Wall -Wextra -pedantic -std=c99
LFLAGS=-lm -ldl -pthread

SRC := src
OBJ := obj

SOURCES := $(wildcard $(SRC)/*.c)
OBJECTS := $(patsubst $(SRC)/%.c, $(OBJ)/%.o, $(SOURCES))

rawterm.bin: $(OBJECTS)
	$(CC) $^ -o $@ $(LFLAGS)

$(OBJ)/%.o: $(SRC)/%.c
	$(CC) $(CFLAGS) -I$(SRC) -c $< -o $@
