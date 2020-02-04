texpdfs = lecture-01.pdf
racketpdfs = ocaml-to-racket.ps
pdfs = $(texpdfs) $(racketpdfs)
all: $(pdfs)

$(texpdfs): %.pdf: %.tex
	pdflatex $<

$(racketpdfs): %.ps: %.rkt
	slideshow -p -o $@ $<

clean:
	rm -f *.aux *.log *.nav *.out *.pdf *.snm *.toc *.vrb
