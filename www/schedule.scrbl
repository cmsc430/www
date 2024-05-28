#lang scribble/manual
@(require scribble/core scribble/html-properties racket/list)
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
         #:column-properties '(center ())

         @; TODO: I wanted to make the week indicator stretch multiple rows,
         @; but scribble's [tabular] form doesn't seem to support this.

         @; #:cell-properties @append*[(make-list 2 (cons (list (attributes '((rowspan . "5"))))
         @;                                               (make-list (sub1 (* 5 4)) '())))]

         @; #:cell-properties @append*[(make-list 2 (list (list (attributes '((rowspan . "5")))) '() '() '() '()))]

         (list (list @bold{Week} @bold{Date} @bold{Topic} @bold{Due})

               (list @wk{1}
                     @day{5/27}
                     "No class."
                     "")
               (list ""
                     @day{5/28}
                     @secref{Intro}
                     "")
               (list ""
                     @day{5/29}
                     "No class (Wednesday)."
                     "")
               (list ""
                     @day{5/30}
                     @elem{@secref{OCaml to Racket}}
                     "")
               (list ""
                     @day{5/31}
                     @elem{@secref{a86}}
                     "")

               (list @wk{2}
                     @day{6/3}
                     @secref{Abscond}
                     @seclink["Assignment 1"]{A1})
               (list ""
                     @day{6/4}
                     @itemlist[@item{@secref{Blackmail}}
                               @item{@secref{Con}}]
                     "")
               (list ""
                     @day{6/5}
                     "No class (Wednesday)."
                     "")
               (list ""
                     @day{6/6}
                     @itemlist[@item{@secref{Dupe}}
                               @item{@secref{Dodger}}]
                     "")
               (list ""
                     @day{6/7}
                     @secref{Evildoer}
                     "")

               (list @wk{3}
                     @day{6/10}
                     @secref{Extort}
                     @;@seclink["Assignment 2"]{A2}
                     "A2")
               (list ""
                     @day{6/11}
                     @secref{Fraud}
                     "")
               (list ""
                     @day{6/12}
                     "No class (midterm, Wednesday)."
                     @;@seclink["Midterm 1"]{Midterm 1}
                     "Midterm 1")
               (list ""
                     @day{6/13}
                     @secref{Hustle}
                     "")
               (list ""
                     @day{6/14}
                     @secref{Hoax}
                     "")

               (list @wk{4}
                     @day{6/17}
                     @secref{Iniquity}
                     @;@seclink["Assignment 3"]{A3}
                     "A3")
               (list ""
                     @day{6/18}
                     @elem{@secref{Iniquity}, cont.}
                     "")
               (list ""
                     @day{6/19}
                     "No class (Juneteenth, Wednesday)."
                     "")
               (list ""
                     @day{6/20}
                     @secref{Jig}
                     "")
               (list ""
                     @day{6/21}
                     @secref{Knock}
                     "")

               (list @wk{5}
                     @day{6/24}
                     @elem{@secref{Knock}, cont.}
                     @;@seclink["Assignment 4"]{A4}
                     "A4")
               (list ""
                     @day{6/25}
                     @secref{Loot}
                     "")
               (list ""
                     @day{6/26}
                     "No class (midterm, Wednesday)."
                     @;@seclink["Midterm 2"]{Midterm 2}
                     "Midterm 2")
               (list ""
                     @day{6/27}
                     @elem{@secref{Loot}, cont.}
                     "")
               (list ""
                     @day{6/28}
                     "Garbage collection."
                     "")

               (list @wk{6}
                     @day{7/1}
                     @secref{Mug}
                     @;@seclink["Assignment 5"]{A5}
                     "A5")
               (list ""
                     @day{7/2}
                     @secref{Mountebank}
                     "")
               (list ""
                     @day{7/3}
                     "No class (Wednesday)."
                     "")
               (list ""
                     @day{7/4}
                     @secref{Neerdowell}
                     "")
               (list ""
                     @day{7/5}
                     @secref{Outlaw}
                     @secref{Project}))]

@bold{Final project assessment: @|final-date|.}
