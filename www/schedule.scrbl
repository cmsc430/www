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
                     @elem{@secref["OCaml to Racket"] (@link["code/ocaml-to-racket.pdf"]{slides})})
               
               (list @wk{2/4}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"] (cont.)}
                     @itemlist[@item{@secref["Abscond"](@link["code/abscond.pdf"]{slides})} @item{@secref["Blackmail"](@link["code/abscond.pdf"]{slides})}])

               (list @wk{2/11}
	       	     @seclink["Assignment 2"]{A2}
                     @elem{@secref["Con"] (@link["code/con.pdf"]{slides})}
                     @elem{@secref["Dupe"] (@link["code/dupe.pdf"]{slides})})
            		     
               
               (list @wk{2/18}
	       	     @seclink["Assignment 3"]{A3}
                     @elem{@secref["Extort"] (@link["code/extort.pdf"]{slides})}
                     @secref["Fraud"])
               
               (list @wk{2/25}
                     ""
                     @elem{@secref["Grift"] (@link["code/grift.pdf"]{slides})}
		                 @elem{@secref["Grift"] (cont.)})
               
               (list @wk{3/3}
                     ""
                     @secref["Hustle"]
                     @elem{@secref["Iniquity"]})
               
               (list @wk{3/10}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @elem{@secref["Iniquity"] (cont.)})
               
               (list @wk{3/17}
	       	     @seclink["Assignment 4"]{A4}
                     @elem{Spring Break}
                     'cont)
               
               (list @wk{3/24}
	       	     @seclink["Assignment 5"]{A5}                     
                     @secref["Jig"]
                     @secref["Knock"])                    
               
               (list @wk{3/31}
	       	     @seclink["Assignment 6"]{A6}
                     @secref["Loot"]
                     @elem{@secref["Loot"] (cont.)})
                                    
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
               
               (list @wk{5/5}
                     ""     
                     @elem{TBD} 
                     'cont)

               (list @wk{5/12}
                     ""     
                     @elem{TBD} 
                     @elem{No class})

               )]


@bold{Final project assessment: @final-date .}
