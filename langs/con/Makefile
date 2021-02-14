UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

%.run: %.o main.o
	gcc main.o $< -o $@

main.o: main.c
	gcc -c main.c -o main.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run

test: 42.run
	@test "$(shell ./42.run)" = "42"

%.rrts: %.o libmain.a
	gcc -o $@ libmain.a $< -lpthread -ldl

libmain.a: main.rs
	rustc -C target-feature=+crt-static --crate-type staticlib main.rs

