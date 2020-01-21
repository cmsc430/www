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
               (list @wk{1/28}
	       	     ""
                     @secref["Intro"]
                     @secref["OCaml to Racket"])
               
               (list @wk{2/4}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"] (cont.)}
                     @itemlist[@item{@secref["Abscond"]} @item{@secref["Blackmail"]}])

               (list @wk{2/11}
	       	     @seclink["Assignment 2"]{A2}
                     @secref["Con"]
		     @secref["Dupe"])
               
               (list @wk{2/18}
	       	     @seclink["Assignment 3"]{A3}
                     @secref["Extort"]
                     @secref["Fraud"])
               
               (list @wk{2/25}
	       	     @seclink["Assignment 4"]{A4}
                     @secref["Grift"]
		     @secref["Hustle"])
               
               (list @wk{3/3}
	       	     @seclink["Assignment 5"]{A5}                     
                     @secref["Iniquity"]
                     @elem{@secref["Iniquity"] (cont.)})
               
               (list @wk{3/10}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @secref["Jig"])
               
               (list @wk{3/17}
                     ""
                     @elem{Spring Break}
                     'cont)
               
               (list @wk{3/24}
	       	     @seclink["Assignment 6"]{A6}
                     @secref["Knock"]
                     @secref["Loot"])                    
               
               (list @wk{3/31}
	       	     ""
                     @elem{Loot (cont.)}
		     @elem{Types})
                                    
               (list @wk{4/7}
	       	     @seclink["Assignment 7"]{A7}
                     @elem{Letrec}
		     @elem{Exceptions via CPS})                    
               
               (list @wk{4/14}
                     ""
                     @elem{Pattern matching}
		     @elem{Quotation})
               
               (list @wk{4/21}
	       	     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{Review}
		     @elem{No lecture (exam)})
               
               (list @wk{4/28} 
                     "" 
                     @elem{TBD} 
                     'cont)
               
               (list @wk{4/5}
                     ""     
                     @elem{TBD} 
                     'cont)

               (list @wk{4/12}
                     ""     
                     @elem{TBD} 
                     @elem{No class})

               )]


@bold{Final project assessment: @final-date .}
