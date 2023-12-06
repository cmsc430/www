UNAME := $(shell uname)
.PHONY: test

.SECONDARY: $(wild *.make)

BUILD_DIR := .

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

objs = \
	main.o \
	values.o \
	print.o \
	stdlib.o \
	io.o

default: runtime.o

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

.c.o:
	gcc -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

%.make: %.rkt
	echo "MAKING"
	racket -t build.rkt -m $< > $@

%.run: %.make
	make $@

clean:
	@$(RM) *.o *.s *.run $(BUILD_DIR)/*.make ||:
	@echo "$(shell basename $(shell pwd)): cleaned!"

-include $(wildcard $(BUILD_DIR)/*.make)
