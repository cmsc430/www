# NOTES:
# - You will need a static version of libunistring to link against; on Mac
#   ld will always choose .dylib over .a to link, so either rename or remove
#   the .dylib versions.

UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
  libs='-lunistring'
else
  format=elf64
  libs='-l:libunistring.a'
endif

objs = \
	main.o \
	values.o \
	print.o \
	symbol.o \
	string.o \
	io.o \
	error.o \
	stdlib.o

default: runtime.o

runtime.o: $(objs)
	gcc -r $(objs) $(libs) -o runtime.o

%.run: %.o runtime.o
	gcc runtime.o $< -o $@

.c.o:
	gcc -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

stdlib.s: stdlib.rkt
	cat stdlib.rkt | racket -t compile-library.rkt -m > stdlib.s

%.s: %.rkt
	cat $< | racket -t compile-stdin.rkt > $@

clean:
	rm *.o *.s *.run

test: example.run
	@test "$(shell ./example.run)" = "$(shell racket example.rkt)"
