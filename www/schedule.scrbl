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
		                 @elem{@secref["Grift"] (cont.)(@link["code/grift2.pdf"]{slides})})
               
               (list @wk{3/3}
	       	           @seclink["Assignment 4"]{A4}
                     @elem{@secref["Hustle"] (@link["code/hustle.pdf"]{slides})}
                     @elem{@secref["Hustle"] (cont.)})
               
               (list @wk{3/10}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @elem{Overview of Exam Question 5})
               
               (list @wk{3/17}
                     ""
                     @elem{Spring Break}
                     'cont)
               
               (list @wk{3/24}
	       	     @seclink["Assignment 5"]{A5}                     
                     @elem{Global Pandemic}
                     'cont)
               
               (list @wk{3/31}
                     ""
                     @elem{Discussion of new Roadmap}
                     @elem{@secref["Iniquity"]})
                                    
               (list @wk{4/7}
                     ""
                     @elem{@secref["Iniquity"] (cont.)}
		                 @elem{@secref["Jig"]})

               (list @wk{4/14}
                     ""
                     @elem{@secref["Knock"]}
                     @elem{@secref["Quiz"]})

               (list @wk{4/21}
	       	           @seclink["Assignment 6"]{A6}                     
                     @elem{@secref["Loot"]}
                     @elem{@secref["Loot"] (cont.)})

               (list @wk{4/28}
	       	     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{Review}
		                 @elem{No lecture (exam)})
               
               (list @wk{5/5}
                     ""
                     @elem{Either FFI or Dyanmic Values}
                     @elem{Either FFI or Dyanmic Values})

               (list @wk{5/12}
                     ""
                     @elem{Garbage Collection, briefly}
                     @elem{No class})

               )]


@bold{Final project assessment: @final-date .}
