# NOTES:
# - You will need a static version of libunistring to link against; on Mac
#   ld will always choose .dylib over .a to link, so either rename or remove
#   the .dylib versions.


UNAME := $(shell uname)
.PHONY: test


# When on GRACE, we pass options to find libunistring in the course's
# public directory.
ifeq ($(shell hostname | egrep "grace.\.umd\.edu"),)
else
 CMSC430_LIB := /afs/glue/class/fall2022/cmsc/430/0101/public
 link_opts := -L$(CMSC430_LIB)/lib/ -Wl,-rpath=$(CMSC430_LIB)/lib/
 include := -I$(CMSC430_LIB)/include/
endif

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
	error.o \
	os.o \
	stdlib.o

default: runtime.o

outlaw.rkt: compile-stdin.rkt \
	ast.rkt \
	parse.rkt \
	a86/ast.rkt \
	registers.rkt \
	types.rkt \
	lambdas.rkt \
	fv.rkt \
	utils.rkt \
	compile-ops.rkt \
	compile-datum.rkt \
	compile-expr.rkt \
	compile-define.rkt \
	compile-literals.rkt \
	compile.rkt \
	read-all.rkt \
	a86/printer.rkt \
	compile-stdin.rkt
	(racket -t combine.rkt -m compile-stdin.rkt stdlib.rkt ;\
	 printf "(main)\n") \
	> outlaw.rkt

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

%.run: %.o runtime.o
	gcc $(link_opts) -lunistring runtime.o $< -o $@

.c.o:
	gcc $(include) -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

stdlib.s: stdlib.rkt
	cat stdlib.rkt | racket -t compile-library.rkt -m > stdlib.s

%.s: %.rkt
	cat $< | racket -t compile-stdin.rkt -m > $@

clean:
	rm *.o *.s *.run outlaw.rkt

test: example.run
	@test "$(shell ./example.run)" = "$(shell racket example.rkt)"
