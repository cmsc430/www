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

@emph{Spring, @year}

@emph{Lectures: Tuesday & Thursday, 9:30-10:45pm, CSIC 2117}

@emph{Professor: @jose}

CMSC 430 is an introduction to compilers.  Its major goal is to arm
students with the ability to design, implement, and extend a
programming language. Throughout the course, students will design and
implement several related languages.


@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list (list @bold{Staff} 'cont 'cont 'cont)
	       (list @bold{Name} @bold{Office} @elem{@bold{E-mail}} @elem{@bold{Hours}})
	       (list @jose @elem{TBD} "FIXME@cs.umd.edu" "11AM-1PM Thu")
               (list @link["https://sankhs.com/"]{Sankha Narayan Guria} @elem{4172 @AVW} "sankha@cs.umd.edu" "3PM-5PM M")
               (list "Tasnim Kabir" @elem{4172 @AVW} "tkabir1@cs.umd.edu" "12:30PM-2:30PM F")
               (list "Ivan Quiles-Rodriguez" @elem{4172 @AVW} "iquiles@umd.edu" "3PM-5PM W")
               (list "John Kastner" @elem{4172 @AVW} "kastner@umd.edu" "3PM-5PM W")
               (list "Yiyun Liu" @elem{4172 @AVW} "liuyiyun@terpmail.umd.edu" "3:30-5PM TuTh"))]

@bold{Communications:} @link["https://piazza.com/umd/spring2020/cmsc430"]{https://piazza.com/umd/spring2020/cmsc430}

@bold{Assumptions:} This course assumes you know the material in CMSC
330 and CMSC 216.  In particular, you need to know how to program in a
functional programming language like OCaml and some
familiarity with programming in C and Assembly.

@bold{Disclaimer:} All information on this web page is tentative and
subject to change until the start of the semester.

@include-section{syllabus.scrbl}
@include-section{texts.scrbl}
@include-section{schedule.scrbl}
@include-section{notes.scrbl}
@include-section{assignments.scrbl}
@include-section{midterms.scrbl}
@include-section{project.scrbl}
@include-section{racket.scrbl}
