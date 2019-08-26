#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 2-3:15 CSI 2117)

@(define (wk d) (nonbreaking (bold d)))


@tabular[#:style 'boxed
         #:sep @hspace[1] 
         #:row-properties '(bottom-border)
         (list (list @bold{Week} @bold{Due} @bold{Tuesday} @bold{Thursday})
               (list @wk{Aug 27}
	       	     ""
                     @secref["Intro"]
                     @secref["OCaml to Racket"])
               
               (list @wk{Sep 3}
	       	     @seclink["Assignment 1"]{A1}
                     @secref["Abscond"]
                     @secref["Blackmail"])

               (list @wk{Sep 10}
	       	     "A2" @;seclink["Assignment 2"]{A2}
                     @secref["Con"]
                     @secref["Dupe"])
               
               (list @wk{Sep 17}
	       	     "A3" @;seclink["Assignment 3"]{A3}
                     @secref["Extort"]
                     @secref["Fraud"])
               
               (list @wk{Sep 24}
	       	     "A4" @;seclink["Assignment 4"]{A4}
                     @secref["Grift"]
                     @secref["Hustle"])
               
               (list @wk{Oct 1}
	       	     "A5" @;seclink["Assignment 5"]{A5}
                     @elem{TBD}
                     'cont)
               
               (list @wk{Oct 8}
	       	     @bold{M1}
                     @elem{TBD}
                     'cont)
               
               (list @wk{Oct 15}
	       	     "A6"
                     @elem{TBD}
                     'cont)
               
               (list @wk{Oct 22}
	       	     "A7"	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{Oct 29}
	       	     "A8"	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 5}
	       	     "A9"	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 12}
	       	     @bold{M2}	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 19}
	       	     ""	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 26} "" @elem{TBD} @elem{@bold{Thanksgiving}})
               
               (list @wk{Dec 3}
                     ""     
                     @elem{TBD} 
                     'cont)

               )]


@bold{Final project assessment: @final-date .}
