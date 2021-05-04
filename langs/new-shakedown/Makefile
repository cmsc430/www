UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

%.run: %.o runtime.o clib.o
	gcc runtime.o clib.o $< -o $@

runtime.o: main.o char.o io.o
	ld -r main.o char.o io.o -o runtime.o

main.o: main.c types.h runtime.h
	gcc -fPIC -c main.c -o main.o

char.o: char.c types.h
	gcc -fPIC -c char.c -o char.o

io.o: io.c runtime.h
	gcc -fPIC -c io.c -o io.o

clib.o: clib.c types.h
	gcc -c clib.c -o clib.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run
