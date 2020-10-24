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
               (list @wk{08/31}
	       	     ""
                     @secref["Intro"]
                     @elem{@secref["OCaml to Racket"] (@link["code/ocaml-to-racket.pdf"]{slides})})

               (list @wk{09/07}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"] (cont.)}
                     @elem{@secref["Abscond"]})

               (list @wk{09/14}
	       	     @seclink["Assignment 2"]{A2}
                     @itemlist[@item{@secref["Agreement"]}
                               @item{@secref["Blackmail"]}]
                     @elem{@secref["Con"]})

               (list @wk{09/21}
                     ""
                     @elem{@secref["Dupe"]}
                     @secref["Extort"])
               
               (list @wk{09/28}
	       	     @seclink["Assignment 3"]{A3}
                     @elem{@secref["Fraud"]}
		                 @elem{Refactor the AST (video in ELMS)})
               
               (list @wk{10/05}
	       	           @seclink["Assignment 4"]{A4}
                     @elem{@secref["Grift"]}
                     @elem{Exam Review})
               
               (list @wk{10/12}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @elem{Exam Q+A})

               (list @wk{10/19}
                     ""
                     @elem{@secref["Hustle"]}
                     @elem{@secref["Iniquity"]})
               
               (list @wk{10/26}
                     @seclink["Assignment 5"]{A5}
                     @elem{@secref["Jig"]}
                     @elem{@secref["Knock"]})
               
               (list @wk{11/02}
                     ""
                     @elem{Digression: Taking a look at ASTs}
                     @elem{@secref["Loot"]})
                                    
               (list @wk{11/09}
                     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{Review}
		                 @elem{No lecture (exam)})

               (list @wk{11/16}
                     ""
                     @elem{@secref["Shakedown"]}
                     @elem{@secref["Shakedown"] (cont.)})

               (list @wk{11/23}
                     @seclink["Assignment 6"]{A6}
                     @elem{@secref["Trick"]}
                     @elem{No lecture (Thanksgiving)})

               (list @wk{11/30}
	       	           ""
                     @elem{Class-voted topic 1}
                     @elem{Class-voted topic 1 (cont.)})
               
               (list @wk{12/7}
                     @seclink["Assignment 7"]{A7}
                     @elem{Class-voted topic 2}
                     @elem{Class-voted topic 2 (cont.)})

               (list @wk{12/14}
               @bold{@seclink["Final Project"]{Final Project}}
                     @elem{No lectures}
                     'cont)

               )]


@bold{Final project assessment: @final-date .}
