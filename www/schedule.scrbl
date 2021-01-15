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
                     @elem{@secref["OCaml to Racket"] (@link["code/ocaml-to-racket.pdf"]{slides})})

               (list @wk{2/1}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"] (cont.)}
                     @elem{@secref["Abscond"]})

               (list @wk{2/8}
	       	     @seclink["Assignment 2"]{A2}
                     @itemlist[@item{@secref["Agreement"]}
                               @item{@secref["Blackmail"]}]
                     @elem{@secref["Con"]})

               (list @wk{2/15}
                     ""
                     @elem{@secref["Dupe"]}
                     @secref["Extort"])
               
               (list @wk{2/22}
	       	     @seclink["Assignment 3"]{A3}
                     @elem{@secref["Fraud"]}
		                 @elem{Refactor the AST (video in ELMS)})
               
               (list @wk{3/1}
	       	           @seclink["Assignment 4"]{A4}
                     @elem{@secref["Grift"]}
                     @elem{Exam Review})
               
               (list @wk{3/8}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @elem{Exam Q+A})

               (list @wk{3/15}
                     ""
                     @elem{@secref["Hustle"]}
                     @elem{@secref["Iniquity"]})
               
               (list @wk{3/22}
                     @seclink["Assignment 5"]{A5}
                     @elem{@secref["Jig"]}
                     @elem{@secref["Knock"]})
               
               (list @wk{3/29}
                     ""
                     @elem{@secref["Graphviz"]}
                     @elem{Discussion of student-chosen topics})
                                    
               (list @wk{4/5}
                     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{Review}
		                 @elem{No lecture (exam)})

               (list @wk{4/12}
                     ""
                     @elem{@secref["Loot"]}
                     @elem{@secref["Loot"] (cont.)})

               (list @wk{4/19}
                     ""
                     @elem{@secref["Shakedown"]}
                     @elem{No lecture (Thanksgiving)})

               (list @wk{4/26}
	       	           ""
                     @elem{Class-voted topic 1}
                     @elem{Class-voted topic 1 (cont.)})
               
               (list @wk{5/3}
                     @seclink["Assignment 6"]{A6}
                     @elem{Class-voted topic 2}
                     @elem{Class-voted topic 2 (cont.)})

               (list @wk{5/10}
               @bold{@seclink["Final Project"]{Final Project}}
                     @elem{No lectures}
                     'cont)

               )]


@bold{Final project assessment: @final-date .}
