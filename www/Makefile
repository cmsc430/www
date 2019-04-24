course = main

.PHONY : all clean test

all: clean $(course)

test: clean
	raco test .

$(course):
	scribble --htmls ++style extra.css \
		++extra accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl

clean:
	rm -rf $(course)



