# dwm - dynamic window manager
# See LICENSE file for copyright and license details.

include config.mk

CC ?= cc

SRC := $(wildcard src/*.c)
OBJ := $(patsubst %.c,%.o,$(SRC))
OBJ := $(patsubst src/%,obj/%,$(OBJ))

build: mintywm

obj/%.o: src/%.c config.h
	mkdir -p obj
	$(CC) $(CFLAGS) -c -o $@ $<

mintywm: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o mintywm

debug: CFLAGS = $(DEBUGFLAGS)
debug: build

run: mintywm
	DISPLAY=:32 ./mintywm

clean:
	rm -rf obj/
	rm -f mintywm

.PHONY: build clean
