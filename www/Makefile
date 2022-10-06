course = main

.PHONY : all clean test scribble zips

all: clean $(course)

test: clean
	raco test .

$(course): scribble zips

zips:
	mkdir -p $(course)/code/
	cd ../langs ; \
		tar -c `git ls-files intro a86 abscond blackmail con dupe dodger evildoer extort fraud hustle hoax iniquity jig knock loot mug mountebank neerdowell fp project.pdf` \
			a86/main.c a86/gcd.c a86/tri.s abscond/42.s | \
		(cd ../www/main/code ; tar -x ; \
			for f in abscond blackmail con dupe dodger evildoer extort fraud hustle hoax iniquity jig knock loot mug mountebank ; do \
			zip $${f}.zip -r $${f}/ ; \
			done )
	cd slides ; \
		tar -c `git ls-files ocaml-to-racket.pdf abscond.pdf con.pdf dupe.pdf extort.pdf grift.pdf grift2.pdf hustle.pdf` | \
		(cd ../main/code ; tar -x) ; cd ../..

scribble:
	raco scribble --htmls \
		++style css/extra.css \
		++style css/fancyverb.css \
		++extra js/accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl



clean:
	rm -rf $(course)
