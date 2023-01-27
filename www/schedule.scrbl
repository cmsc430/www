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
               (list @wk{1/23}
	       	     ""
               ""
                     @secref["Intro"])

               (list @wk{1/30}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"], (@link["https://youtu.be/xKCFkXUcmK4"]{video})}
                     @elem{@secref["OCaml to Racket"], cont., (@link["https://youtu.be/sLOMQ_j7cPE"]{video}) }
                     )

               (list @wk{2/6}
	       	     @seclink["Assignment 2"]{A2}
                     @elem{@secref["a86"], (@link["https://www.youtube.com/watch?v=25tV38STdbQ"]{video})}
                     @elem{@secref["Abscond"]})

               (list @wk{2/13}
	             ""
                     @itemlist[@item{@secref["Blackmail"], (@link["https://www.youtube.com/watch?v=28F-5sCUfzg"]{video})}
			       @item{@secref["Con"], (@link["https://youtu.be/nh8x0EQsQQY"]{video})}]
                     @itemlist[@item{@secref["Dupe"], (@link["https://youtu.be/GdQZ2D1lyZA"]{video})}
                               @item{@secref["Dodger"], (@link["https://youtu.be/-SuINAKs7gE"]{video})}])
                                    
               (list @wk{2/20}
	       	     @seclink["Assignment 3"]{A3}
		     @elem{@secref["Evildoer"], (@link["https://youtu.be/ouOgFdbT9fk"]{video})}
		     @itemlist[
		       @item{@secref["Evildoer"], cont. (@link["https://youtu.be/ouOgFdbT9fk"]{video})}
		       @item{@secref["Extort"], (@link["https://youtu.be/4lU-0i5sl-Q"]{video})}])
               
               (list @wk{2/27}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @elem{@secref["Fraud"] (@link["https://youtu.be/XmezNX4qfWE"]{video})})
               
               (list @wk{3/6}
	       	     ""
		     @elem{@secref["Hustle"] (@link["https://youtu.be/SwKc_FeEmHk"]{video})}
                     @itemlist[
		       @item{@secref["Hustle"], cont. (@link["https://youtu.be/SwKc_FeEmHk"]{video})}
		       @item{@secref["Hoax"], (@link["https://youtu.be/yEsXYn8exfk"]{video})}])

               (list @wk{3/13}
                     @seclink["Assignment 4"]{A4}
		     @itemlist[
		       @item{@secref["Hoax"], cont. (@link["https://youtu.be/SwKc_FeEmHk"]{video})}
                       @item{@secref{Iniquity}, (@link["https://www.youtube.com/watch?v=Yz2n7KgIfNQ"]{video})}]
                     @elem{@secref{Iniquity}})
		                                   
               (list @wk{3/20}
                     ""
                     @elem{No Lecture (Spring Break)}
                     @elem{No Lecture (Spring Break)})

               (list @wk{3/27}
                     ""
                     @elem{@secref["Jig"]}
                     @elem{@secref["Jig"], cont.})
               
               (list @wk{4/3}
      	       	     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{No lecture (exam)}
                     @elem{@secref["Loot"]})
                                    
               (list @wk{4/10}
	             @seclink["Assignment 5"]{A5}
                     @elem{@secref["Loot"], cont.}
                     @elem{@secref["Knock"]}
                     )

               (list @wk{4/17}
                     @seclink["Assignment 6"]{A6}
                     @elem{GC}
                     @elem{GC, cont.})

               (list @wk{4/24}
                     ""
		     @elem{@secref{Mug}}
		     @elem{No class, Thanksgiving}
		     )

               (list @wk{5/1}
               @bold{@tbaseclink["Settle on Final Project"]{Final Project}}                     
                     @elem{@secref{Mountebank}}
		     @elem{@secref{Neerdowell}}
		     )
               
               (list @wk{5/6}
                     "Final Projects due on exam date"
                     @elem{Outlaw}
                     @elem{Self-hosting}
                     )

               )]


@bold{Final project assessment: @|final-date|.}
