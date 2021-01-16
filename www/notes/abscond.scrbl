#lang scribble/manual

@(require (for-label (except-in racket compile)))
@(require scribble/examples
	  redex/reduction-semantics	  
          redex/pict
	  (only-in pict scale)
	  (only-in racket system)
	  "../fancyverb.rkt"
	  "../utils.rkt"
	  "utils.rkt"
	  "ev.rkt")

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "abscond" f))))))
	   '("interp.rkt" "ast.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "abscond")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(require (for-syntax "../utils.rkt" racket/base "utils.rkt"))
@(define-syntax (shell-expand stx)
   (syntax-case stx ()
     [(_ s ...)
      (parameterize ([current-directory (build-path notes "abscond")])
        (begin (apply shell (syntax->datum #'(s ...)))
	       #'(void)))]))

@;{ Have to compile 42.s (at expand time) before listing it }
@(shell-expand "racket -t compile-file.rkt -m 42.rkt > 42.s")


@title[#:tag "Abscond"]{Abscond: a language of numbers}

@emph{Let's Make a Programming Language!}

@table-of-contents[]

@section{Overview}

A compiler is just one (optional!) component of a @emph{programming
language}.  So if you want to make a compiler, you must first settle
on a programming language to compile.

The specification of a programming language consists of two parts: the
syntax, which specifies the @bold{form} of programs, and semantics,
which specifies the @bold{meaning} of programs.

Syntax, while important, is a fairly superficial aspect of
programming languages. The real heart of a programming
language is its semantics and we will spend more time
concerned this aspect.

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


To begin, let's start with a dead simple programming language called
@bold{Abscond}.  The only kind of expression in Abscond are integer
literals.  Running an abscond program just produces that integer.
(Told you it was simple.)

@section{Concrete syntax for Abscond}

We will simplify matters of syntax by using the Lisp
notation of s-expression for the @bold{concrete} form of
program phrases. The job of a @emph{parser} is to construct
an abstract syntax tree from the textual representation of a
program. We will consider parsing in two phases:

@itemlist[
 @item{the first converts a stream of textual input into an
  s-expression, and}
 @item{the second converts an s-expression into an instance
       of a datatype for representing expressions called an @bold{AST}.}
 ]

For the first phase, we rely on the @racket[read] function
to take care of converting strings to s-expressions. In
order to parse s-expressions into ASTs, we will write fairly
straightforward functions that convert between the
representations.

Abscond, like the other languages studied in this course, is
designed to be a subset of Racket. This has two primary benefits:

@itemlist[
 @item{the Racket interpreter and compiler can be used as a reference implementation of the languages we build, and}
 @item{there are built-in facilities for reading and writing
  data in the parenthezised form that Racket uses, which we can borrow to make parsing easy.}
 ]

The concrete form of an Abscond program will consist of, like Racket, the line of text:

@verbatim{#lang racket}

followed by a (concrete) expression.  The grammar of expressions is
very simple:



@(define-language A
  (e ::= integer))

@centered{@render-language[A]}

So, @racket[0], @racket[120], @racket[-42], etc. are
concrete Abscond expressions and a complete Abscond program
looks like this:

@codeblock-include["abscond/42.rkt"]

Reading Abscond programs from ports, files, strings,
etc. consists of reading (and ignoring) the
@tt{#lang racket}
line and then using the @racket[read] function to
parse the concrete expression as an s-expression.


@section{Abstract syntax for Abscond}

While not terribly useful for a language as overly simplistic as Abscond, we use
an AST datatype for representing expressions and another syntactic categories.
For each category, we will have an appropriate constructor.  In the case of Abscond
all expressions are integers, so we have a single constructor, @racket[Int].

@(define-language A-concrete
  (e ::= (Int i))
  (i ::= integer))

@centered{@render-language[A-concrete]}

A datatype for representing expressions can be defined as:

@codeblock-include["abscond/ast.rkt"]


The parser for Abscond checks that a given s-expression is
an integer and constructs an instance of the AST datatype if
it is, otherwise it signals an error:
@codeblock-include["abscond/parse.rkt"]

@section{Meaning of Abscond programs}

The meaning of an Abscond program is simply the number itself.  So
@racket[(Int 42)] evaluates to @racket[42].

We can write an ``interpreter'' that consumes an expression and
produces it's meaning:

@codeblock-include["abscond/interp.rkt"]

@#reader scribble/comment-reader
(examples #:eval ev
(interp (Int 42))
(interp (Int -8))
)

We can add a command line wrapper program for interpreting Abscond
programs saved in files:

@codeblock-include["abscond/interp-file.rkt"]

The details here aren't important (and you won't be asked to write
this kind of code), but this program @racket[read]s the contents of a
file given on the command line.  If it's an integer, i.e. a
well-formed Abscond program, then it runs the intepreter and displays
the result.

For example, interpreting the program @tt{42.rkt} shown above:
@shellbox["racket -t interp-file.rkt -m 42.rkt"]

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
  #:mode (𝑨 I O)
  #:contract (𝑨 e i)
  [----------
   (𝑨 (Int i) i)])

@(centered (render-judgment-form 𝑨))

Here, we are defining a binary relation, called
@render-term[A 𝑨], and saying every integer literal
expression is paired with the integer itself in the
relation. So @math{((Int 2),2)} is in @render-term[A 𝑨],
@math{((Int 5),5)} is in @render-term[A 𝑨], and so on.

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


@bold{Interpreter Correctness}: @emph{For all expressions @racket[e]
and integers @racket[i], if (@racket[e],@racket[i]) in @render-term[A
𝑨], then @racket[(interp e)] equals @racket[i].}

We now have a complete (if overly simple) programming language with an
operational semantics and an interpreter, which is (obviously)
correct.  Now let's write a compiler.

@section{Toward a Compiler for Abscond}

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
(source-interp e) = (target-interp (source-compile e))
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

@filebox-include[fancy-c "abscond/main.c"]

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

@shellbox["gcc -m64 -c -o main.o main.c"]

This creates @tt{main.o}; linking this file together with an object
file that contains @tt{enter_abscond} will produce an executable that,
when run, will carry out the execution of an Abscond program.

@section{An Example}

Before trying to write the Abscond compiler, let's first make an
example of what we would like the compiler to produce for a particular
example.  Let's say the Abscond program is @racket[42].  What should
the assembly code for this program look like?  Here we have to learn a
bit about the x86-64 assembly language.

@filebox-include[fancy-nasm "abscond/42.s"]

@margin-note{Note: on macOS, labels must be prepended with @tt{_},
while on Linux they are not; e.g. @tt{_entry} vs @tt{entry}.}

Above is a x86-64 program, written in NASM syntax.  We will be using
@tt{nasm} as our assembler in this class because it is widely used and
available on most platforms.

@itemlist[#:style 'numbered

@item{The first line declares a global label (@tt{entry}), an
entry point in to the code below.}

@item{The next line declares the start of a section of code consisting
of textual instructions.}

@item{The third line contains the @tt{entry} label, i.e. the start of
the @tt{entry} code.  When the run-time systems calls @tt{entry}, it
will jump to this point in the code.}

@item{The fourth line is an instruction to move the integer literal 42
into the @tt{rax} register.  By convention, whatever is in the
@tt{rax} register when code returns control to the caller will hold
the return value.}

@item{The final line is an instruction to return control to the
caller.}
]

To assemble this program into an object file, we can run the @tt{nasm}
assembler:

@shellbox[
  (format "nasm -f ~a -o 42.o 42.s"
          (case (system-type 'os)
            [(macosx) "macho64"]
            [(unix) "elf64"]))]

@margin-note{Note: on macOS, the format option @tt{-f} should be
@tt{macho64}; on Linux it should be @tt{elf64}.}

This creates @tt{42.o}, an object file containing the instructions
above (in binary format).

We can link this file with the run-time to produce an executable file:

@shellbox["gcc main.o 42.o -o 42.run"]

This creates the file @tt{42.run}, an exectuable program:

@shellbox["./42.run"]

We now have a working example.  The remaining work will be to design a
compiler that takes an Abscond program and emits a file like
@tt{42.s}, but with the appropriate integer literal.

@section{A Compiler for Abscond}

We will now write a compiler for Abscond.  To heart of the compiler
will be a function with the following signature:

@#reader scribble/comment-reader
(racketblock
;; Expr -> Asm
(define (compile e) ...)
)

Where @tt{Asm} is a data type for representing assembly programs,
i.e. it will be the AST of x86-64 assembly.  This datatype will evolve
as we see more X86 instructions, but for now it is very simple:

@codeblock-include["abscond/asm/ast.rkt"]

So the AST representation of our example is:

@racketblock[
(list (Label 'entry)
      (Mov 'rax 42)
      (Ret))
]

Writing the @racket[compile] function is easy:

@codeblock-include["abscond/compile.rkt"]

@#reader scribble/comment-reader
(examples #:eval ev 
(compile (Int 42))
(compile (Int 38))
)

To convert back to the concrete NASM syntax, we can write a (few)
function(s), which we'll place in its own module:

@codeblock-include["abscond/asm/printer.rkt"]

@margin-note{Note: the printer takes care of the macOS vs Linux label
convention by detecting the underlying system and printing
appropriately.}

@#reader scribble/comment-reader
(examples #:eval ev
(displayln (asm-string (compile (Int 42)))))
                   
Putting it all together, we can write a command line compiler much
like the command line interpreter before, except now we emit assembly
code:

@codeblock-include["abscond/compile-file.rkt"]

Example:

@shellbox["racket -t compile-file.rkt -m 42.rkt"]

Using a Makefile, we can capture the whole compilation dependencies as:

@margin-note{Note: the appropriate object file format is detected
based on the operating system.}

@filebox-include[fancy-make "abscond/Makefile"]

And now compiling Abscond programs is easy-peasy:

@shellbox["make 42.run" "./42.run"]

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

@shellbox["time -p racket -t interp-file.rkt -m 42.rkt"]

Compiling:

@shellbox["time -p ./42.run"]

Because Abscond is a subset of Racket, we can even compare results
against interpreting the program directly in Racket:

@shellbox["touch 42.rkt # forces interpreter to be used"
          "time -p racket 42.rkt"]

Moreover, we can compare our compiled code to code compiled by Racket:

@shellbox["raco make 42.rkt"
	  "time -p racket 42.rkt"]


@section{But is it @emph{Correct}?}

At this point, we have a compiler for Abscond.  But is it correct?

Here is a statement of compiler correctness:

@bold{Compiler Correctness}: @emph{For all expressions @racket[e] and
integers @racket[i], if (@racket[e],@racket[i]) in @render-term[A
𝑨], then @racket[(asm-interp (compile e))] equals
@racket[i].}

Ultimately, we want the compiler to capture the operational semantics
of our language (the ground truth of what programs mean).  However,
from a practical stand-point, relating the compiler to the intepreter
may be more straightforward.  What's nice about the interpreter is we
can run it, so we can @emph{test} the compiler against the
interpreter.  Moreover, since we claimed the interpreter is correct
(w.r.t. to the semantics), testing the compiler against the interpreter
is a way of testing it against the semantics, indirectly.  If the
compiler and interpreter agree on all possible inputs, then the
compiler is correct with respect to the semantics since it is
equivalent to the interpreter, and the interpreter is correct.

So, in this setting, means we have the following equivaluence:

@verbatim{
(interp e) @emph{equals} (asm-interp (compile e))
}

But we don't actually have @racket[asm-interp], a function that
interprets the Asm code we generate.  Instead we printed the code and
had @tt{gcc} assembly and link it into an executable, which the OS
could run.  But this is a minor distinction.  We can write
@racket[asm-interp] to interact with the OS to do all of these steps.

Here's such a definition.  (Again: the details here are not important
and we won't ask you to write or understand this code, but roughly,
all it's doing is emitting assembly (to a temporary file) and
calling @tt{make} to build the executable, then running it and parsing
the result.)

@codeblock-include["abscond/asm/interp.rkt"]

This is actually a handy tool to have for experimenting with
compilation within Racket:


@examples[#:eval ev
(asm-interp (compile (Int 42)))
(asm-interp (compile (Int 37)))
(asm-interp (compile (Int -8)))
]

This of course agrees with what we will get from the interpreter:

@examples[#:eval ev
(interp (Int 42))
(interp (Int 37))
(interp (Int -8))
]

We can turn this in a @bold{property-based test}, i.e. a function that
computes a test expressing a single instance of our compiler
correctness claim:
@examples[#:eval ev
(define (check-compiler e)
  (check-eqv? (interp e)
              (asm-interp (compile e))))

(check-compiler (Int 42))
(check-compiler (Int 37))
(check-compiler (Int -8))
]

This is a powerful testing technique when combined with random
generation.  Since our correctness claim should hold for @emph{all}
Abscond programs, we can randomly generate @emph{any} Abscond program
and check that it holds.

@examples[#:eval ev
(check-compiler (Int (random 100)))

; test 10 random programs
(for ([i (in-range 10)])
  (check-compiler (Int (random 10000))))
]

The last expression is taking 10 samples from the space of Abscond
programs in @math{[0,10000)} and checking the compiler correctness
claim on them.  If the claim doesn't hold for any of these samples, a
test failure would be reported.

Finding an input to @racket[check-compiler] that fails would
@emph{refute} the compiler correctness claim and mean that we have a
bug.  Such an input is called a @bold{counter-example}. 

On the other hand we gain more confidence with each passing test.
While passing tests increase our confidence, we cannot test all
possible inputs this way, so we can't be sure our compiler is correct
by testing alone.  To really be sure, we'd need to write a
@emph{proof}, but that's beyond the scope of this class.

At this point we have not found a counter-example to compiler
correctness.  It's tempting to declare victory.  But... can you think
of a valid input (i.e. some integer) that might refute the correctness
claim?

Think on it.  In the meantime, let's move on.


