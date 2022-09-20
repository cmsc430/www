course = main

.PHONY : all clean test

all: clean $(course)

test: clean
	raco test .

$(course):
	raco scribble --htmls \
		++style css/extra.css \
		++style css/fancyverb.css \
		++extra js/accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl
	mkdir -p $(course)/code/	
	cd ../langs ; \
		tar -c `git ls-files intro a86 abscond blackmail con dupe dodger extort fraud hustle hoax iniquity jig knock loot mug mountebank neerdowell fp project.pdf` \
			a86/main.c a86/gcd.c a86/tri.s abscond/42.s | \
		(cd ../www/main/code ; tar -x ; \
			zip -c abscond.zip -r abscond/ ; \
			zip -c blackmail.zip -r blackmail/ ; \
			zip -c con.zip -r con/ ; \
			zip -c dupe.zip -r dupe/ ; \
			zip -c fraud.zip -r fraud/ ; \
			zip -c hustle.zip -r hustle/ ; \
			zip -c hoax.zip -r hoax/ ; \
			zip -c iniquity.zip -r iniquity/ ; \
			zip -c jig.zip -r jig/ ; \
			zip -c knock.zip -r knock/ ; \
			zip -c loot.zip -r loot/ ; \
			zip -c knock.zip -r mug/ ; \
			zip -c knock.zip -r mountebank/) ; cd ../..
	cd slides ; \
		tar -c `git ls-files ocaml-to-racket.pdf abscond.pdf con.pdf dupe.pdf extort.pdf grift.pdf grift2.pdf hustle.pdf` | \
		(cd ../main/code ; tar -x) ; cd ../..

clean:
	rm -rf $(course)
