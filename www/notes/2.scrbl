#lang scribble/manual
@title[#:style 'unnumbered #:tag "week2"]{Week 2: The First Few Compilers}
@(require redex/reduction-semantics
          redex/pict (only-in pict scale)
	  "../fancyverb.rkt")

@(require scribble/examples racket/sandbox)
@(define ev
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket #:requires '("notes/2/asm-printer.rkt"))))

@(require (for-syntax racket/base racket/file))
@(define-syntax (filebox-include stx)
  (syntax-case stx ()
    [(_ form n fn)
     (let ((s (file->string (syntax->datum #'fn))))
       #`(filebox (tt n) (form #,s)))]))

@table-of-contents[]

@section{Let's Make a Programming Language!}

A compiler is just one (optional!) component of a @emph{programming
language}.  So if you want to make a compiler, you must first settle
on a programming language to compile.

The specification of a programming language consists of two parts: the
syntax, which specifies the @bold{form} of programs, and semantics,
which specifies the @bold{meaning} of programs.

We will simplify matters of syntax by using the Lisp notation of
s-expression for program phrases.  The job of a @emph{parser} is to
construct an abstract syntax tree from the textual representation of a
program.  For us, we will rely on the @racket[read] function to take
care of parsing and essentially gloss over this (well understood,
arguably boring) topic.  We will focus on the abstract syntax by
defining BNF grammars.

The heart of a programming language is its semantics and we will spend
more time concerned this aspect.

There are a few common ways a language's meaning is specified:

@itemlist[

@item{By example.}

@item{By informal description.}

@item{By reference to an implementation, often an interpreter.}

@item{By formal (mathematical) definition.}

]

Each approach has its advantages and disadvantages.  Examples are
concise and unambiguous, but incomplete.  Informal (prose)
descriptions can be intuitive, but open to interpretation and
ambiguity.  Reference implementations provide precise, executable
specifications, but may over specify language details by tying them to
implementation artifacts.  Formal definitions balance precision while
allowing for under-specification, but require detailed definitions and
training to understand.

We will use a combination of each.

@subsection{Abscond: a language of numbers}

To begin, let's start with a dead simple programming language called
@bold{Abscond}.  The only kind of expression in Abscond are integer
literals.  Running an abscond program just produces that integer.
(Told you it was simple.)

@subsection{Abstract syntax for Abscond}

An Abscond program consists of a single expression, and the 
grammar of expressions is very simple:

@(define-language A
  (e ::= i)
  (i ::= integer))

@centered{@render-language[A]}

So, @racket[0], @racket[120], @racket[-42], etc. are Abscond programs.

A datatype for representing expressions can be defined as:

@#reader scribble/comment-reader
(racketblock
;; type Expr = Integer
)

@subsection{Meaning of Abscond programs}

The meaning of an Abscond program is simply the number itself.  So
@racket[42] evaluates to @racket[42].

We can write an ``interpreter'' that consumes an expression and
produces it's meaning:

@#reader scribble/comment-reader
(examples #:eval ev #:no-prompt #:label #f
;; Expr -> Integer
;; Interpreter for Abscond
(define (abscond-interp e)
  e)
)

@#reader scribble/comment-reader
(examples #:eval ev
(abscond-interp 42)
(abscond-interp -8)
)

We can even write a command line program for interpreting Abscond programs.
Save the following in a file @tt{abscond-interp.rkt}:

@filebox[@tt{abscond-interp.rkt}]{
@codeblock|{
#lang racket

; Expr -> Integer
; Interpreter for Abscond
(define (abscond-interp e)
  e)

(with-input-from-file (vector-ref (current-command-line-arguments) 0)
  (λ ()
    (let ((p (read)))
      (unless (integer? p) (error "syntax error" p))
      (displayln (abscond-interp p)))))
}|}

The details here aren't important, but this program @racket[read]s the
contents of a file given on the command line.  If it's an integer,
i.e. a well-formed Abscond program, then it runs the intepreter and
displays the result.

For example:
@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> echo 42 > 42.scm
> racket abscond-interp.rkt 42.scm
42
}|}

Even though the semantics is obvious, we can provide a formal
definition of Abscond using @bold{operational semantics}.

An operational semantics is a mathematical definition that
characterizes the meaning of programs.  We will defined the semantics
of Abscond as a @emph{binary relation} between programs and their
meanings.  So in the setting of Abscond, this binary relation will be
a set of pairs of expressions and integers.  This relation will be
defined inductively using @emph{inference rules}.  For such a simple
language, just a single inference rule suffices:

@(define-judgment-form A
  #:mode (eval I O)
  #:contract (eval e i)
  [----------
   (eval i i)])

@(centered (render-judgment-form eval))

Here, we are defining a binary relation, called @render-term[A eval] ,
and saying every integer paired with itself is in the relation.  So
@math{(2,2)} is in @render-term[A eval], @math{(5,5)} is in
@render-term[A eval], and so on.

The inference rules define the binary relation by defining the
@emph{evidence} for being in the relation.  The rule makes use of
@emph{meta-variables} drawn from the non-terminals of the language
grammar.  A pair is in the relation if you can construct an instance
of the rule (substituting some integer for @math{i}) in the rule.

(This part probably seems opaque at the moment, but it will become
clearer as we work through more examples, so don't worry.)


The operational semantics @emph{defines} the meaning of Abscond
programs.  The intepreter @emph{computes} that meaning.  We can view
the semantics as a specification, and the interpreter as an
implementation.

Characterizing the correctness of the interpreter boils down to the
following statement:


For all expressions @racket[e] and integers @racket[i], if
(@racket[e],@racket[i]) in @render-term[A eval], then
@racket[(abscond-interp e)] equals @racket[i].

We now have a complete (if overly simple) programming language with an
operational semantics and an interpreter, which is (obviously)
correct.  Now let's write a compiler.

@subsection{Toward a Compiler for Abscond}

A compiler, like an interpreter, is an implementation of a programming
language.  The key difference is that a compiler stages the work of
interpreting a program into two phases.  The first translates the
original program (the source language) into a program in another
programming language (the target language).  The second runs this
program.  These phases, often called @bold{compile-time} and
@bold{run-time}.  The program that does the translation is the
@bold{compiler}.  The program that does the running of the translated
program is the @bold{run-time system}.

So in general, the relationship between an interpreter and compiler is

@verbatim{
(interp-source e) = (interp-target (compile-source e))
}

We can in principle choose any target language we'd like.  For this
class, we will choose the @bold{x86-64} instruction set architecture.

There are several reasons for this choice:

@itemlist[

@item{it is a low-level language, so compiling to a high-level
language to x86-64 will require building everything from scratch,}

@item{it is the programming language at the ``bottom'' of your
computer; it's interpreter is implemented in hardware on your
computer's CPU,}

@item{it is one of the two dominant computing architectures (the other
being ARM), and}

@item{it is a mature technology with good tools and materials.}
]

So our compiler will emit x86 assembly code.  To make our lives a bit
easier, we will write the run-time system in C.  Let's start with the
Abscond runtime:

@filebox-include[fancy-c "main.c" "notes/2/main.c"]

This C program provides the main entry point for running an Abscond
program.  It must be linked against an object file that provides the
definition of @tt{enter_abscond}; this is the code our compiler will
emit.

The @tt{enter_abscond} function computes the result of running the
Abscond code, i.e. an integer.  Here we are taking advantage of the
x86-64 architecture by using 64-bit signed integers by using the
@tt{int64_t} C type.

The runtime system calls the function and prints the result.

We can compile the run-time system to get an object file.  We'll use
@tt{gcc} for compiling C:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> gcc -m64 -c -o main.o main.c
}|}

This creates @tt{main.o}; linking this file together with an object
file that contains @tt{enter_abscond} will produce an executable that,
when run, will carry out the execution of an Abscond program.

@subsection{An Example}

Before trying to write the Abscond compiler, let's first make an
example of what we would like the compiler to produce for a particular
example.  Let's say the Abscond program is @racket[42].  What should
the assembly code for this program look like?  Here we have to learn a
bit about the x86-64 assembly language.

@filebox-include[fancy-nasm "42.s" "notes/2/42.s"]

Above is a x86-64 program, written in NASM syntax.  We will be using
@tt{nasm} as our assembler in this class because it is widely used and
available on most platforms.

@itemlist[#:style 'numbered

@item{The first line declares a global label (@tt{_enter_abscond}), an
entry point in to the code below.}

@item{The next line declares the start of a section of code consisting
of textual instructions.}

@item{The third line contains the @tt{_enter_abscond} label, i.e. the
start of the @tt{enter_abscond} code.  When the run-time systems calls
@tt{enter_abscond}, it will jump to this point in the code.}

@item{The fourth line is an instruction to move the integer literal 42
into the @tt{rax} register.  By convention, whatever is in the
@tt{rax} register when code returns control to the caller will hold
the return value.}

@item{The final line is an instruction to return control to the
caller.}
]

To assemble this program into an object file, we can run the @tt{nasm}
assembler:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> nasm -f macho64 -o 42.o 42.s
}|}

This creates @tt{42.o}, an object file containing the instructions
above (in binary format).

We can link this file with the run-time to produce an executable file:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> ld -lc main.o 42.o -o 42.run
}|
}

This creates the file @tt{42.run}, an exectuable program:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> ./42.run
42
}|}

We now have a working example.  The remaining work will be to design a
compiler that takes an Abscond program and emits a file like
@tt{42.s}, but with the appropriate integer literal.

@subsection{A Compiler for Abscond}

We will now write a compiler for Abscond.  To heart of the compiler
will be a function with the following signature:

@#reader scribble/comment-reader
(racketblock
;; Expr -> Asm
(define (compile-abscond e) ...)
)

Where @tt{Asm} is a data type for representing assembly programs,
i.e. it will be the AST of x86-64 assembly.  This datatype will evolve
as we see more X86 instructions, but for now it is very simple:

@#reader scribble/comment-reader
(racketblock
;; type Asm = [Listof Instruction]

;; type Instruction =
;; | `ret
;; | `(mov ,Arg ,Arg)
;; | Label

;; type Label = Symbol

;; type Arg =
;; | Reg
;; | Integer

;; type Reg =
;; | `rax
)

So the AST reprentation of our example is:

@racketblock[
'(abscond_entry
  (mov rax 42)
  ret)
]

Writing the @racket[compile-abscond] function is easy:

@#reader scribble/comment-reader
(examples #:eval ev 
;; Expr -> Asm
(define (compile-abscond e)
  `(abscond_entry
    (mov rax ,e)
    ret))

(compile-abscond 42)
(compile-abscond 38)
)



To convert back to the concrete NASM syntax, we can write a (few)
function(s), which we'll place in its own module:

@filebox-include[codeblock "asm-printer.rkt" "notes/2/asm-printer.rkt"]

@#reader scribble/comment-reader
(examples #:eval ev
(display-asm (compile-abscond 42)))


                   
Putting it all together, we can write a command line compiler much
like the command line interpreter before, except now we emit assembly
code:

@filebox-include[codeblock "abscond-compile.rkt" "notes/2/abscond-compile.rkt"]

Example:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> racket abscond-compile.rkt 42.scm
  	global _abscond_entry
	section .text
_abscond_entry:
	mov rax, 42
	ret
}|}

Using a Makefile, we can capture the whole compilation dependencies as:

@filebox-include[fancy-make "Makefile" "notes/2/Makefile"]

And now compiling Abscond programs is easy-peasy:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> make 42.run
gcc -c main.c -o main.o
racket abscond-compile.rkt 42.scm > 42.s
nasm -f macho64 -o 42.o 42.s
ld -lc main.o 42.o -o 42.run
rm 42.o 42.s
> ./42.run
42
}|}



It's worth taking stock of what we have at this point, compared to the
interpreter approach.  To run the interpreter requires all of Racket
in the run-time system.  

When running a program using the interpreter, we have to parse the
Abscond program, check the syntax of the program (making sure it's an
integer), then run the interpreter and print the result.

When running a program using the compiler, we still have to parse the
Abscond program and check its syntax, but this work happens @emph{at
compile-time}.  When we @emph{run} the program this work will have
already been done.  While the compiler needs Racket to run, at
run-time, Racket does not need to be available.  All the run-time
needs is our (very tiny) object file compiled from C.  Racket doesn't
run at all -- we could delete it from our computer or ship the
executable to any compatible x86-64 machine and run it there.  This
adds up to much more efficient programs.  Just to demonstrate, here's
a single data point measuring the difference between interpreting and
compiling Abscond programs:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> time racket abscond-interp.rkt 42.scm
42

real	0m0.273s
user	0m0.215s
sys	0m0.053s
}|}

Compiling:

@filebox[@emph{shell}]{
@fancyverbatim["fish"]|{
> time ./42.run 
42

real	0m0.014s
user	0m0.002s
sys	0m0.004s
}|}


@;{
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


}