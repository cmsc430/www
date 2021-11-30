UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

objs = \
	main.o \
	values.o \
	print.o \
	symbol.o \
	string.o \
	io.o \
	stdlib.o

default: runtime.o

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

%.run: %.o runtime.o
	gcc runtime.o $< -o $@

.c.o:
	gcc -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

stdlib.s: stdlib.rkt
	racket -t compile-library.rkt -m stdlib.rkt > stdlib.s

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run

test: example.run
	@test "$(shell ./example.run)" = "$(shell racket example.rkt)"
