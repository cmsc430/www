#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@;(TuTh 9:30-10:45, IRB 0318)

@(define (wk d) (nonbreaking (bold d)))

@; for unreleased assignments, switch to seclink when ready to release
@(define (tbaseclink lnk txt) txt)

@(define (day s) @elem[s])


@tabular[#:style 'boxed
#:sep @hspace[1]
#:row-properties '(bottom-border)
(list (list @bold{Date} @bold{Topic} @bold{Due})
(list @day{5/30} @secref["Intro"] "")
(list @day{5/31} @secref["OCaml to Racket"] @seclink["Assignment 1"]{A1})
(list @day{6/1}  @secref["a86"] @seclink["Assignment 2"]{A2})
(list @day{6/2}  @secref["Abscond"] "")
(list @day{6/5}  @itemlist[@item{@secref["Blackmail"]} @item{@secref["Con"]}]  "")
(list @day{6/6}  @itemlist[@item{@secref["Dupe"]} @item{@secref{Dodger}}] "")
(list @day{6/7}  @secref["Evildoer"] @seclink["Assignment 3"]{A3})
(list @day{6/8}  @secref["Extort"] @seclink["Assignment 3"]{A3})
(list @day{6/9}  @secref["Fraud"] "")
(list @day{6/12} @secref["Hustle"] "")
(list @day{6/13} @elem{@secref["Hustle"], cont.} "")
(list @day{6/14} @secref["Hoax"] "")
(list @day{6/15} @secref["Iniquity"] "")
(list @day{6/16} @elem{@secref["Iniquity"], cont.} "")
(list @day{6/19} @secref["Jig"] "")
(list @day{6/20} @elem{@secref["Jig"], cont.} "")
(list @day{6/21} @secref["Loot"] "")
(list @day{6/22} @elem{@secref["Loot"], cont.} "")
(list @day{6/23} @secref["Knock"] @seclink["Assignment 5"]{A5})
(list @day{6/26} @elem{@secref["Knock"], cont.} "")
(list @day{6/27} @elem{GC} @seclink["Assignment 6"]{A6})
(list @day{6/28} @elem{GC, cont.} "")
(list @day{6/29} @secref["Mug"] "") 
(list @day{6/30} @secref["Mountebank"] "") 
(list @day{7/3}  @secref["Neerdowell"] "") 
(list @day{7/4} "Independence Day Holiday" "")
(list @day{7/5} @secref["Outlaw"] @bold{@tbaseclink["Settle on Final Project"]{Final Project}})
(list @day{7/6} @elem{@secref["Outlaw"], cont.} "")
(list @day{7/7} "Final Projects due" "")
)
]


@bold{Final project assessment: @|final-date|.}
