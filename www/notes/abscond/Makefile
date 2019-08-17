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

%.s: %.scm
	racket -t compile.rkt -m $< > $@

clean:
	rm *.o *.s *.run

test: 42.run
	@test "$(shell ./42.run)" = "42"
