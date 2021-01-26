UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

%.run: %.o runtime.o
	gcc runtime.o $< -o $@

runtime.o: main.o char.o io.o io_set.o
	ld -r main.o char.o io.o io_set.o -o runtime.o

main.o: main.c types.h
	gcc -c main.c -o main.o

char.o: char.c types.h
	gcc -c char.c -o char.o

io.o: io.c io.h
	gcc -c io.c -o io.o

io_set.o: io_set.c io.h
	gcc -c io_set.c -o io_set.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run
