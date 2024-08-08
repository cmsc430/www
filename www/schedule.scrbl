#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 9:30-10:45, IRB 0318)

@(define (wk d) (nonbreaking (bold d)))

@; for unreleased assignments, switch to seclink when ready to release
@(define (tbaseclink lnk txt) txt)

@(define (day s) @elem[s])


@;{ Fall }
@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (list (list @bold{Week}
                     @bold{Due}
                     @bold{Tuesday}
                     @bold{Thursday})

               (list @wk{8/27}
	       	     ""
	             @secref["Intro"]
		     @elem{@secref["OCaml to Racket"]})
		               

               (list @wk{9/3}
	       	     ""
	             @elem{@secref["a86"]}                     
                     @elem{@secref["a86"]})

               (list @wk{9/10}
                     @seclink["Assignment 1"]{A1}
                     @itemlist[@item{@secref["Abscond"]}
                               @item{@secref["Blackmail"]}]
	             @elem{...})
                     
               (list @wk{9/17}
                     @seclink["Assignment 2"]{A2}
		     @elem{...}
		     @itemlist[@item{@secref["Con"]}
                               @item{@secref["Dupe"]}])

               (list @wk{9/24}
                     @elem{@seclink["Assignment 2"]{A3} P1}
		     @itemlist[@item{@secref["Dodger"]}
                               @item{@secref["Evildoer"]}]
	             @secref["Evildoer"])

               (list @wk{10/1}
                     @elem{@seclink["Assignment 2"]{A3} P2}
		     @secref{Extort}
		     @secref{Fraud})

               (list @wk{10/8}
                     ""
		     @secref{Fraud}
		     @secref["Midterm_1"])

               (list @wk{10/15}
                     ""
		     @secref{Fraud}
		     @secref{Hustle})
               (list @wk{10/22}
	             @elem{@seclink["Assignment 4"]{A4} P1}
		     @secref{Hustle}
		     @secref{Hustle})

               (list @wk{10/22}
                     @elem{@seclink["Assignment 4"]{A4} P2}
		     @secref{Hoax}
		     @secref{Iniquity})

               (list @wk{10/29}
                     ""
		     @secref{Iniquity}
		     @secref{Iniquity})


	       (list @wk{11/5}
	             ""
		     @secref{Knock}
		     @secref["Midterm_2"])
		     

               (list @wk{11/12}
	             ""
		     @secref{Jig}
		     @secref{Loot})

               (list @wk{11/19}
	             @elem{@seclink["Assignment 5"]{A5}}
		     @secref{Loot}
		     @secref{Mug})

               (list @wk{11/26}
	             ""
		     @secref{Neerdowell}
		     @elem{No class})

               (list @wk{12/3}
	             ""
		     @secref{Outlaw}
		     @elem{@secref{Outlaw}, cont.})

)]

@;{ Spring }
@;{
@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         (list (list @bold{Week}
                     @bold{Due}
                     @bold{Monday}
                     @bold{Wednesday})

               (list @wk{1/22}
	       	           ""
	             "No class"
                     @secref["Intro"])
		               

               (list @wk{1/29}
	       	     ""
	             @elem{@secref["OCaml to Racket"]}
                     @elem{@secref["OCaml to Racket"]})

               (list @wk{2/5}
                     @seclink["Assignment 1"]{A1}
                     @elem{@secref["a86"]}
                     @elem{@secref["a86"]})

               (list @wk{2/12}
                     @seclink["Assignment 2"]{A2}
		     @itemlist[@item{@secref["Abscond"]}
                               @item{@secref["Blackmail"]}]
                     @itemlist[@item{@secref["Con"]}
                               @item{@secref["Dupe"]}])

               (list @wk{2/19}
                     @elem{@seclink["Assignment 2"]{A3} P1}
		     @itemlist[@item{@secref["Dodger"]}
                               @item{@secref["Evildoer"]}]
	             @secref["Evildoer"])

               (list @wk{2/26}
                     @elem{@seclink["Assignment 2"]{A3} P2}
		     @secref{Extort}
		     @secref{Fraud})

               (list @wk{3/4}
                     ""
		     @secref{Fraud}
		     @secref["Midterm_1"])

               (list @wk{3/11}
                     ""
		     @secref{Fraud}
		     @secref{Hustle})
               (list @wk{3/18}
	             ""
		     @elem{Spring Break}
		     @elem{Spring Break})
               (list @wk{3/25}
	             @elem{@seclink["Assignment 4"]{A4} P1}
		     @secref{Hustle}
		     @secref{Hustle})

               (list @wk{4/1}
                     @elem{@seclink["Assignment 4"]{A4} P2}
		     @secref{Hoax}
		     @secref{Iniquity})

               (list @wk{4/8}
                     ""
		     @secref{Iniquity}
		     @secref{Iniquity})


	       (list @wk{4/15}
	             ""
		     @secref{Knock}
		     @secref["Midterm_2"])
		     

               (list @wk{4/22}
	             ""
		     @secref{Jig}
		     @secref{Loot})

               (list @wk{4/29}
	             @elem{@seclink["Assignment 5"]{A5}}
		     @secref{Loot}
		     @secref{Mug})

               (list @wk{5/6}
	             ""
		     @secref{Neerdowell}
		     @secref{Outlaw})

)]
}

@;{
@tabular[#:style 'boxed
#:sep @hspace[1]
#:row-properties '(bottom-border)
(list (list @bold{Date} @bold{Topic} @bold{Due})
(list @day{5/30} @secref["Intro"] "")
(list @day{5/31} @secref["OCaml to Racket"] "")
(list @day{6/1}  @secref["a86"] "")
(list @day{6/2}  @secref["Abscond"] @seclink["Assignment 1"]{A1})
(list @day{6/5}  @itemlist[@item{@secref["Blackmail"]} @item{@secref["Con"]}] @seclink["Assignment 2"]{A2})
(list @day{6/6}  @itemlist[@item{@secref["Dupe"]} @item{@secref{Dodger}}] "")
(list @day{6/7}  @secref["Evildoer"] "")
(list @day{6/8}  @secref["Extort"] "")
(list @day{6/9}  @secref["Fraud"] "")
(list @day{6/12} @secref["Hustle"] @seclink["Assignment 3"]{A3})
(list @day{6/13} @secref["Hoax"] "")
(list @day{6/14} "Midterm 1" @secref["Midterm_1"])
(list @day{6/15} @secref["Iniquity"] "")
(list @day{6/16} @elem{@secref["Iniquity"], cont.} "")
(list @day{6/19} @elem{Juneteenth Holiday} "")
(list @day{6/20} @secref["Jig"] @seclink["Assignment 4"]{A4})
(list @day{6/21} @secref["Knock"] "")
(list @day{6/22} @elem{@secref["Knock"], cont.} "")
(list @day{6/23} @secref["Loot"] "")
(list @day{6/26} @elem{@secref["Loot"], cont.} "")
(list @day{6/27} @elem{GC} @seclink["Assignment 5"]{A5})
(list @day{6/28} @secref["Mug"] "")
(list @day{6/29} "Midterm 2" @secref["Midterm_2"])
(list @day{6/30} @secref["Mountebank"] "")
(list @day{7/3}  @secref["Neerdowell"] @seclink["Assignment 6"]{A6})
(list @day{7/4} "Independence Day Holiday" "")
(list @day{7/5} @secref["Outlaw"] "")
(list @day{7/6} @elem{@secref["Outlaw"], cont.} "")
(list @day{7/7} "Slack" @secref{Project})
)
]
}

@bold{Final project assessment: @|final-date|.}
