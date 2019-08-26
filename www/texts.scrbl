#lang scribble/manual
@(require scribble/core "utils.rkt")

@title[#:style 'unnumbered]{Texts}

This course @bold{has no required textbook}.  See the @secref{Notes}
instead.


@section{Background}


@itemlist[

@item{
@image[#:style float-right #:scale 1/3]{img/htdp-2e-cover.gif}

@link["https://htdp.org/2018-01-06/Book/index.html"]{How
to Design Programs, Second Edition (On-line Draft)} by Felleisen,
Findler, Flatt, Krishnamurthi.

If you have never seen functional programming, particularly
type-driven design in functional programming, you should read this
book.

This text uses the Racket environment and Racket-like programming
languages so it can also serve as a good introduction to the
programming language we will be using throughout this course, even
if you are familiar with the concepts the book covers.


}

@item{
@image[#:style float-right #:scale 1/4]{img/csapp3e-cover.png}

@link["https://csapp.cs.cmu.edu/"]{Computer Systems: A Programmer's
Perspective, Third Edition} by Bryant and O'Hallaron.

This book gives an excellent overview of computer systems from a
programmer's perspective.  Chapter 3 "Machine-Level Representations of
Programs" is particularly relevant, giving a (for our purposes)
comprehensive description of the x86-64 architecture we target in this
course.}


@item{
@image[#:style float-right #:scale 1/8]{img/incr-compile.png}

@link["http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf"]{An
Incremental Approach to Compiler Construction}, Scheme Workshop 2006,
Abdulaziz Ghuloum.

This paper is the inspiration behind the design of this course.  It
describes the "incremental" approach we adopt where we study compilers
by starting with a complete compiler for a very small language and
incrementally grow it into a compiler for a powerful high-level
language.  If you would like to read a capsule summary of the course
concepts, this is the paper for you.

}

]