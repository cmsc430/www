#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 9:30-10:45, IRB 0318)

@(define (wk d) (nonbreaking (bold d)))

@; for unreleased assignments, switch to seclink when ready to release
@(define (tbaseclink lnk txt) txt)

@tabular[#:style 'boxed
         #:sep @hspace[1] 
         #:row-properties '(bottom-border)
         (list (list @bold{Week} @bold{Due} @bold{Tuesday} @bold{Thursday})
               (list @wk{1/25}
	       	     ""
                     @secref["Intro"]
                     @elem{@secref["OCaml to Racket"], (@link["https://youtu.be/xKCFkXUcmK4"]{video})} )

               (list @wk{2/1}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"], cont., (@link["https://youtu.be/sLOMQ_j7cPE"]{video}) }
                     @elem{@secref["a86"], (@link["https://www.youtube.com/watch?v=25tV38STdbQ"]{video})} )

               (list @wk{2/8}
	       	     @seclink["Assignment 2"]{A2}
                     @itemlist[@item{@secref["Abscond"]}
                               @item{@secref["Blackmail"], (@link["https://www.youtube.com/watch?v=28F-5sCUfzg"]{video})}
			       @item{@secref["Con"], (@link["https://youtu.be/nh8x0EQsQQY"]{video})}]
                     @elem{@secref["Dupe"], (@link["https://youtu.be/GdQZ2D1lyZA"]{video})})

               (list @wk{2/15}
	             ""
                     @itemlist[@item{@secref["Dodger"], (@link["https://youtu.be/-SuINAKs7gE"]{video})}
                               @item{Semantics, Correctness, Testing}]
                     @elem{@secref["Evildoer"], (@link["https://youtu.be/ouOgFdbT9fk"]{video})})
                                    
               (list @wk{2/22}
	       	     @seclink["Assignment 3"]{A3}
		     @elem{@secref["Extort"], (@link["https://youtu.be/4lU-0i5sl-Q"]{video})}
		     @elem{@secref["Fraud"], (@link["https://youtu.be/XmezNX4qfWE"]{video})})
               
               (list @wk{3/1}
	       	     ""
                     @elem{@secref["Fraud"], cont. (@link["https://youtu.be/XmezNX4qfWE"]{video})}
                     @itemlist[@item{Exam Review}
		               @item{@secref["Hustle"] (@link["https://youtu.be/SwKc_FeEmHk"]{video})}])
               
               (list @wk{3/8}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @itemlist[
		       @item{@secref["Hustle"], cont. (@link["https://youtu.be/SwKc_FeEmHk"]{video})}
		       @item{@secref["Hoax"], (@link["https://youtu.be/yEsXYn8exfk"]{video})}]
                     @elem{No lecture (exam)})

               (list @wk{3/15}
	       	     @seclink["Assignment 4"]{A4}
		     @itemlist[
		       @item{@secref["Hoax"], cont. (@link["https://youtu.be/SwKc_FeEmHk"]{video})}
                       @item{@secref{Iniquity}, (@link["https://www.youtube.com/watch?v=Yz2n7KgIfNQ"]{video})}]
                     @elem{@secref{Jig}})
		                    
               (list @wk{3/22}
                     ""
		     @elem{Spring Break}
                     @elem{Spring Break})
               
               (list @wk{3/29}
                     ""
                     @elem{@secref["Loot"]}
                     @elem{@secref["Loot"], cont.})
               
               (list @wk{4/5}
                     @seclink["Assignment 5"]{A5}                     
                     @elem{@secref["Knock"]}
                     @elem{Review and Voting})
                                    
               (list @wk{4/12}
	             ""
                     @itemlist[
		       @item{@secref["Mug"]}
		       @item{@secref["Mountebank"]}]
		     @secref["Neerdowell"]
                     )

               (list @wk{4/19}
                     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{Self-hosting?}
                     @elem{Self-hosting?})

               (list @wk{4/26}
                     @seclink["Assignment 6"]{A6}
		     @elem{GC}
		     @elem{GC}
		     )

               (list @wk{5/3}
               @bold{@tbaseclink["Settle on Final Project"]{Final Project}}                     
                     @elem{Final Project Q&A}
		     @elem{Slack}
		     )
               
               (list @wk{5/10}
                     "Final Projects due on exam date"
                     @elem{Last day}
                     'cont)

               )]


@bold{Final project assessment: @|final-date|.}
