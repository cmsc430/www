#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
          scribble/decode
          scribble/html-properties
      	  "defns.rkt"
          "utils.rkt")

@(define jose @link["http://jmct.cc/"]{José Manuel Calderón Trilla})

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
                     (decode-flow strs)))


@(define accessible
   (style #f (list (js-addition "js/accessibility.js")
                   (attributes '((lang . "en"))))))

@title[#:style accessible @courseno]{: Design and Implementation of Programming Languages}

@image[#:scale 1/2 #:style float-right]{img/wizard.png}

@emph{Fall, @year}

@emph{Lectures: Tuesday & Thursday, 2:00pm Eastern - 3:15pm Eastern, Online}

@emph{Professor: @jose}

CMSC 430 is an introduction to compilers.  Its major goal is to arm
students with the ability to design, implement, and extend a
programming language. Throughout the course, students will design and
implement several related languages.


@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list (list @bold{Staff} 'cont 'cont)
	       (list @bold{Name} @elem{@bold{E-mail}} @elem{@bold{Hours}})
	       (list @jose "jmct@umd.edu" "Tue/Thur 12pm-1:30pm EST")
               (list "Tasnim Kabir" "tkabir1@cs.umd.edu" "Friday 9am-11am (EST)")
               (list "Alex Taber" "ataber@terpmail.umd.edu" "Tue/Thur 3:30-5:00 PM EST")
               (list "Vyas Gupta" "vgupta13@terpmail.umd.edu" "M/W 2-3:30pm EST"))]

@bold{Communications:} Email

@bold{Assumptions:} This course assumes you know the material in CMSC 330 and
CMSC 216. In particular, you need to know how to program in a functional
programming language like OCaml and some familiarity with programming in C and
Assembly. See the @seclink["Texts"]{Texts} page for references to brush up on
this material.

@bold{Disclaimer:} All information on this web page is tentative and subject to
change. Any substantive change will be accompanied with an announcement to the
class via ELMS.

@include-section{syllabus.scrbl}
@include-section{texts.scrbl}
@include-section{schedule.scrbl}
@include-section{notes.scrbl}
@include-section{assignments.scrbl}
@include-section{midterms.scrbl}
@include-section{project.scrbl}
@include-section{racket.scrbl}
