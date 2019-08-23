#lang scribble/manual
@(require scribble/core racket/list)

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 2-3:15 CSI 2117)

@local-table-of-contents[]

@section[#:style 'unnumbered]{Lecture Material}

@(define (wk d) (nonbreaking (bold d)))


@tabular[#:style 'boxed
         #:sep @hspace[1] 
         #:row-properties '(bottom-border)
         (list (list @bold{Week} @bold{Tuesday} @bold{Thursday})
               (list @wk{Aug 27} 
                     @secref["Intro"]
                     @secref["OCaml to Racket"])
               
               (list @wk{Sep 3} 
                     @secref["Abscond"]
                     @secref["Blackmail"])
               
               (list @wk{Sep 10} 
                     @secref["Con"]
                     @secref["Dupe"])
               
               (list @wk{Sep 17} 
                     @elem{TBD}
                     'cont)
               
               (list @wk{Sep 24}
                     @elem{TBD}
                     'cont)
               
               (list @wk{Oct 1}
                     @elem{TBD}
                     'cont)
               
               (list @wk{Oct 8} 
                     @elem{TBD}
                     'cont)
               
               (list @wk{Oct 15}
                     @elem{TBD}
                     'cont)
               
               (list @wk{Oct 22}
                     @elem{TBD}
		     'cont)
               
               (list @wk{Oct 29}
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 5} 
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 12} 
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 19}
                     @elem{TBD}
		     'cont)
               
               (list @wk{Nov 26} @elem{TBD} @elem{@bold{Thanksgiving}})
               (list @wk{Dec 3} 
	       	     @elem{TBD} 
		     'cont))]
