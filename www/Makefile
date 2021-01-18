course = main

.PHONY : all clean test

all: clean $(course)

test: clean
	raco test .

$(course):
	scribble --htmls \
		++style css/extra.css \
		++style css/fancyverb.css \
		++extra js/accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl
	mkdir -p $(course)/code/	
	cd ../langs ; \
		tar -c `git ls-files intro a86 abscond blackmail con dupe extort fraud hustle iniquity jig .knock loot mug fp project.pdf` \
			a86/main.c a86/tri.s | \
		(cd ../www/main/code ; tar -x) ; cd ../..
	cd slides ; \
		tar -c `git ls-files ocaml-to-racket.pdf abscond.pdf con.pdf dupe.pdf extort.pdf grift.pdf grift2.pdf hustle.pdf` | \
		(cd ../main/code ; tar -x) ; cd ../..

clean:
	rm -rf $(course)
