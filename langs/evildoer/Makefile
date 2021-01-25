UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

%.run: %.o main.o char.o byte.o
	gcc main.o char.o byte.o $< -o $@

main.o: main.c types.h
	gcc -c main.c -o main.o

char.o: char.c types.h
	gcc -c char.c -o char.o

byte.o: byte.c types.h
	gcc -c byte.c -o byte.o

byte-shared.o: byte-shared.c types.h
	gcc -fPIC -c byte-shared.c -o byte-shared.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run
