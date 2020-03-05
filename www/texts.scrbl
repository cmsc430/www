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

@link["https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf"]{x64 Cheat Sheet}
Introduction to Computer Systems, Brown University, Tom Doeppner.

These lecture notes provide a quick overview of the x64 assembly language.
Sections 1-3 will be the most relevant to our class. Section 4 has more to do
with C-style data structures and the C calling convention.  It is important to
note that whenever this document refers to a function or function call, it is
referring to a @emph{C-style} function or function call.

}

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


@section{References}


@itemlist[

@item{
@link["https://www.felixcloutier.com/x86/"]{x86 and amd64 instruction reference}

Derived from the May 2019 version of the (4,922 page!)
@link["https://software.intel.com/sites/default/files/managed/39/c5/325462-sdm-vol-1-2abcd-3abcd.pdf"]{Intel®
64 and IA-32 Architectures Software Developer’s Manual}.  We will only
use a small fraction of these instructions, but this is a
comprehensive list of what's available on a x86-64 machine.}

@item{@link["https://docs.racket-lang.org/guide/"]{The Racket Guide} 

Intended for those new to Racket, i.e. @emph{you!}}

@item{@link["https://docs.racket-lang.org/reference/"]{The Racket Reference}

The definitive, comprehensive manual for Racket.}


]



