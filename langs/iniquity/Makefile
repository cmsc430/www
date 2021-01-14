UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

%.run: %.o main.o char.o byte.o string.o gc.o
	gcc main.o char.o byte.o string.o gc.o $< -o $@

main.o: main.c types.h heap.h
	gcc -c main.c -o main.o

char.o: char.c types.h
	gcc -c char.c -o char.o

byte.o: byte.c types.h
	gcc -c byte.c -o byte.o

string.o: string.c types.h
	gcc -c string.c -o string.o

gc.o: gc.c types.h heap.h
	gcc -c gc.c -o gc.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run

test: 42.run
	@test "$(shell ./42.run)" = "42"
