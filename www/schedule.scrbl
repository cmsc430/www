#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 2-3:15, Online)

@(define (wk d) (nonbreaking (bold d)))


@tabular[#:style 'boxed
         #:sep @hspace[1] 
         #:row-properties '(bottom-border)
         (list (list @bold{Week} @bold{Due} @bold{Tuesday} @bold{Thursday})
               (list @wk{1/25}
	       	     ""
                     @secref["Intro"]
                     @elem{@secref["OCaml to Racket"] (@link["code/ocaml-to-racket.pdf"]{slides}) (@link["https://www.youtube.com/watch?v=KTWq1XAfuMQ"]{vid})})

               (list @wk{2/1}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"] (cont.) (@link["https://www.youtube.com/watch?v=QfxVrYuUboc"]{vid})}
                     @elem{@secref["a86"] (@link["https://www.youtube.com/watch?v=iEThSsxHmaI"]{vid})})

               (list @wk{2/8}
	       	     @seclink["Assignment 2"]{A2}
                     @itemlist[@item{@secref["Abscond"]}
                               @item{@secref["Blackmail"]}]
                     @elem{@secref["Con"]})

               (list @wk{2/15}
                     ""
                     @elem{@secref["Dupe"]}
                     @secref["Dodger"])
               
               (list @wk{2/22}
	       	     @seclink["Assignment 3"]{A3}
                     @elem{@secref["Evildoer"]}
		                 @elem{@secref["Extort"]})
               
               (list @wk{3/1}
	       	     ""
                     @elem{@secref["Fraud"]}
                     @elem{Exam Review})
               
               (list @wk{3/8}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @elem{Exam Q+A})

               (list @wk{3/15}
                     ""
                     @elem{No lecture (Spring Break)}
                     @elem{No lecture (Spring Break)})
               
               (list @wk{3/22}
                     @seclink["Assignment 4"]{A4}
                     @elem{@secref["Hustle"]}
                     @elem{@secref["Iniquity"]})
               
               (list @wk{3/29}
                     ""
                     @elem{@secref["Jig"]}
                     @elem{@secref["Knock"]})
               
               (list @wk{4/5}
                     @seclink["Assignment 5"]{A5}                     
                     @elem{Randomized Testing}
                     @elem{Review})
                                    
               (list @wk{4/12}
                     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{No lecture (exam)}
                     @elem{Exam Review}                     
                     )

               (list @wk{4/19}
                     ""
                     @elem{@secref["Loot"]}
                     @elem{@secref["Loot"] (cont.)})

               (list @wk{4/26}
                     @seclink["Assignment 6"]{A6}
                     @elem{@secref["Shakedown"]}
                     @elem{Class-voted topic 1})

               (list @wk{5/3}
               @bold{@seclink["Final Project"]{Final Project}}
                     @elem{Class-voted topic 1 (cont.)}
                     @elem{Class-voted topic 2})
               
               (list @wk{5/10}
	       	           ""
                     @elem{Class-voted topic 2 (cont.)}
                     @elem{No lecture (end of semester)})

               (list @wk{5/10}
                     ""
                     @elem{No lectures}
                     'cont)

               )]


@bold{Final project assessment: @final-date .}
