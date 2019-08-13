#lang scribble/manual
@title[#:style 'unnumbered]{Week 2: The First Few Compilers}

@table-of-contents[]

@section{Lets Write a Compiler!}

Our goal is to write a compiler which is a function:

@verbatim{
compiler :: SourceProgram -> TargetProgram
}

In 430 TargetProgram is going to be a binary executable.

@subsection{Lets write our first Compilers}

@tt{SourceProgram} will be a sequence of four @emph{tiny} “languages”

@itemlist[#:style 'ordered
@item{Numbers, e.g. @racket[7], @racket[12], @racket[42], ...}
@item{Numbers + Increment, e.g. @racket[(add1 7)], @racket[(add1 (add1 7))], ...}
@item{Numbers + Increment + Decrement, e.g. @racket[(add1 7)], @racket[(add1 (add1 12))], @racket[(sub1 (add1 42))], ...}
@item{Numbers + Increment + Decrement + Local Variables, e.g. 
@racketblock[
(let* ((x (add1 7))
       (y (add1 x)))
  (add1 y))]
}
]

@subsection{Recall: What does a Compiler @emph{look like}?}

@image{img/compiler-pipeline.png}
Compiler Pipeline

An input source program is converted to an executable binary in many stages:

@itemlist[
@item{@bold{Parsed} into a data structure called an @bold{Abstract Syntax Tree}}
@item{@bold{Checked} to make sure code is well-formed (and well-typed)}
@item{@bold{Simplified} into some convenient @bold{Intermediate Representation}}
@item{@bold{Optimized} into (equivalent) but faster program}
@item{@bold{Generated} into assembly x86}
@item{@bold{Linked} against a run-time (usually written in C)}
]

@subsection{Simplified Pipeline}

@bold{Goal}: Compile @emph{source} into @emph{executable} that, when
run, @bold{prints} the result of evaluating the source.

@bold{Approach}: Lets figure out how to write

@itemlist[#:style 'ordered
@item{A @bold{compiler} from the input @emph{string} into @emph{assembly},}
@item{A @bold{run-time} that will let us do the printing.}
]

@image{img/compiler-pipeline-1-2.png}
Simplified Compiler Pipeline with Runtime

Next, lets see how to do (1) and (2) using our sequence of languages.


@section{A Compiler for Numbers}

This is perhaps the simplest programming language: it consists solely
of number literals.

@subsection{The "Run-time"}

Lets work @emph{backwards} and start with the run-time.

Here’s what it looks like as a C program @tt{main.c}

@verbatim{
#include <stdio.h>

extern int our_code() asm("our_code_label");

int main(int argc, char** argv) {
  int result = our_code();
  printf("%d\n", result);
  return 0;
}
}




@verbatim{
section .text
global our_code_label
our_code_label:
  mov eax, 42
  ret
}


