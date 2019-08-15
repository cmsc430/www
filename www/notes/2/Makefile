%.run: %.o main.o
	ld -lc main.o $< -o $@

main.o: main.c
	gcc -c main.c -o main.o

%.o: %.s
	nasm -f macho64 -o $@ $<

%.s: %.scm
	racket abscond-compile.rkt $< > $@

clean:
	rm *.o *.s *.run