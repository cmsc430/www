course = main

.PHONY : all clean test

all: clean $(course)

test: clean
	raco test .

$(course):
	scribble --htmls ++style extra.css \
		++style fancyverb.css \
		++extra accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl
	mkdir -p $(course)/code/
	cp -r notes/abscond/ $(course)/code/abscond/

clean:
	rm -rf $(course)
