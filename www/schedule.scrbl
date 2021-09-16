#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 2-3:15, Online)

@(define (wk d) (nonbreaking (bold d)))

@; for unreleased assignments, switch to seclink when ready to release
@(define (tbaseclink lnk txt) txt)

@tabular[#:style 'boxed
         #:sep @hspace[1] 
         #:row-properties '(bottom-border)
         (list (list @bold{Week} @bold{Due} @bold{Tuesday} @bold{Thursday})
               (list @wk{8/31}
	       	     ""
                     @secref["Intro"]
                     @elem{@secref["OCaml to Racket"], (@link["https://youtu.be/xKCFkXUcmK4"]{video})} )

               (list @wk{9/7}
	       	     @seclink["Assignment 1"]{A1}
                     @elem{@secref["OCaml to Racket"], cont., (@link["https://youtu.be/sLOMQ_j7cPE"]{video}) }
                     @elem{@secref["a86"], (@link["https://www.youtube.com/watch?v=25tV38STdbQ"]{video})} )

               (list @wk{9/14}
	       	     @seclink["Assignment 2"]{A2}
                     @itemlist[@item{@secref["Abscond"]}
                               @item{@secref["Blackmail"], (@link["https://www.youtube.com/watch?v=28F-5sCUfzg"]{video})}]
                     @elem{@secref["Con"]})

               (list @wk{9/21}
                     ""
                     @elem{@secref["Dupe"]}
                     @secref["Dodger"])
               
               (list @wk{9/28}
	       	     @seclink["Assignment 3"]{A3}
                     @elem{@secref["Evildoer"]}
		                 @elem{@secref["Extort"]})
               
               (list @wk{10/5}
	       	     ""
                     @elem{@secref["Fraud"]}
                     @elem{Exam Review})
               
               (list @wk{10/12}
	       	     @bold{@seclink["Midterm_1"]{M1}}
                     @elem{No lecture (exam)}
                     @elem{Exam Q+A})

               (list @wk{10/19}                     
	       	     @tbaseclink["Assignment 4"]{A4}
                     @elem{@secref["Hustle"]}
                     @elem{@secref["Iniquity"]})
		                    
               (list @wk{10/26}
                     ""
                     @elem{@secref["Jig"]}
                     @elem{@secref["Knock"]})
               
               (list @wk{11/2}
                     @tbaseclink["Assignment 5"]{A5}                     
                     @elem{Randomized Testing}
                     @elem{Review})
               
               (list @wk{11/9}
                     @bold{@seclink["Midterm_2"]{M2}}
                     @elem{No lecture (exam)}
                     @elem{Exam Review})
                                    
               (list @wk{11/16}
	             ""
                     @elem{@secref["Loot"]}
		     @elem{@secref["Loot"] (cont.)}		     
                     )

               (list @wk{11/23}
                     ""
                     @elem{Hustle Typing  - Compiler Implications}
                     @elem{No class, Thanksgiving.})

               (list @wk{11/30}
                     @tbaseclink["Assignment 6"]{A6}
		     @elem{Typing Lambdas - Hindley-Milner}
		     @elem{@secref["Shakedown"]}
		     )

               (list @wk{12/7}
               @bold{@tbaseclink["Settle on Final Project"]{Final Project}}                     
                     @elem{Final Project Q&A}
		     @elem{Slack}
		     )
               
               (list @wk{12/14}
                     "Final Projects due on exam date"
                     @elem{No lectures}
                     'cont)

               )]


@bold{Final project assessment: @final-date .}
