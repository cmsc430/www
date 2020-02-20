texpdfs = lecture-01.pdf
racketpdfs = ocaml-to-racket.pdf abscond.pdf con.pdf dupe.pdf extort.pdf
pdfs = $(texpdfs) $(racketpdfs)
all: $(pdfs)

$(texpdfs): %.pdf: %.tex
	pdflatex $<

$(racketpdfs): %.pdf: %.rkt
	slideshow -D -o $@ $<

clean:
	rm -f *.aux *.log *.nav *.out *.pdf *.snm *.toc *.vrb
