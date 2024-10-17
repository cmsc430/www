course = main

.PHONY : all clean test scribble zips push

all: clean $(course)

test: clean
	raco test .

$(course): scribble zips

zips:
	mkdir -p $(course)/code/
	cd `racket -e '(require pkg/lib) (display (path->string (pkg-directory "langs")))'` ; \
		tar -c `git ls-files a86 abscond blackmail con dupe dodger evildoer extort fraud hustle hoax iniquity iniquity-gc jig knock loot mug mountebank neerdowell outlaw` | \
		(cd ../www/main/code ; tar -x ; \
			for f in abscond blackmail con dupe dodger evildoer extort fraud hustle hoax iniquity iniquity-gc jig knock loot mug mountebank neerdowell outlaw ; do \
			zip $${f}.zip -r $${f}/ ; \
			done )

scribble:
	raco scribble --htmls \
		++style css/extra.css \
		++style css/fancyverb.css \
		++extra js/accessibility.js \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		$(course).scrbl

push:
	rsync -rvzp main/ dvanhorn@junkfood.cs.umd.edu:/fs/www/class/fall2024/cmsc430/

clean:
	rm -rf $(course)
