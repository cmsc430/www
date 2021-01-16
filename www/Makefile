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
	cd notes ; \
		tar -c `git ls-files intro ../langs/a86 ../langs/abscond ../langs/blackmail ../langs/con ../langs/dupe ../langs/extort ../langs/fraud ../langs/hustle ../langs/iniquity ../langs/jig ../langs/knock ../langs/loot ../langs/mug ../langs/fp project.pdf` | \
		(cd ../main/code ; tar -x) ; cd ../..
	cd slides ; \
		tar -c `git ls-files ocaml-to-racket.pdf abscond.pdf con.pdf dupe.pdf extort.pdf grift.pdf grift2.pdf hustle.pdf` | \
		(cd ../main/code ; tar -x) ; cd ../..

clean:
	rm -rf $(course)
