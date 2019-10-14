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
               (list @wk{8/27}
	       	     ""
                     @secref["Intro"]
                     @secref["OCaml to Racket"])
               
               (list @wk{9/3}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"] (cont.)}
                     @itemlist[@item{@secref["Abscond"]} @item{@secref["Blackmail"]}])

               (list @wk{9/10}
	       	     @seclink["Assignment 2"]{A2}
                     @secref["Con"]
		     @secref["Dupe"])
               
               (list @wk{9/17}
	       	     @seclink["Assignment 3"]{A3}
                     @secref["Extort"]
                     @secref["Fraud"])
               
               (list @wk{9/24}
	       	     @seclink["Assignment 4"]{A4}
                     @secref["Grift"]
		     @secref["Hustle"])
               
               (list @wk{10/1}
	       	     @seclink["Assignment 5"]{A5}                     
                     @secref["Iniquity"]
                     @elem{@secref["Iniquity"] (cont.)})
               
               (list @wk{10/8}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     'cont)
               
               (list @wk{10/15}
	       	     @seclink["Assignment 6"]{A6}
                     @elem{TBD}
                     'cont)
               
               (list @wk{10/22}
	       	     "A7"	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{10/29}
	       	     "A8"	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{11/5}
	       	     "A9"	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{11/12}
	       	     @bold{M2}	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{11/19}
	       	     ""	       
                     @elem{TBD}
		     'cont)
               
               (list @wk{11/26} "" @elem{TBD} @elem{@bold{Thanksgiving}})
               
               (list @wk{12/3}
                     ""     
                     @elem{TBD} 
                     'cont)

               )]


@bold{Final project assessment: @final-date .}
