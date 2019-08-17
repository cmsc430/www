#lang scribble/manual
@title[#:style 'unnumbered #:tag "week2"]{Week 2: The First Few Compilers}
@(require redex/reduction-semantics
          redex/pict (only-in pict scale)
	  "../fancyverb.rkt"
	  "../utils.rkt"
	  "2/blackmail-semantics.rkt")

@(require (for-label racket))

@(require scribble/examples racket/sandbox)
@(define ev
  (call-with-trusted-sandbox-configuration
    (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 50]
		    [current-directory  (build-path (current-directory-for-user) "notes/2/")])
       (make-evaluator 'racket #:requires '("asm-printer.rkt" "asm-interp.rkt" "blackmail-interp.rkt" rackunit))))))

@(require (for-syntax racket/base racket/file))
@(define-syntax (filebox-include stx)
  (syntax-case stx ()
    [(_ form n fn)
     (let ((s (file->string (syntax->datum #'fn))))
       #`(filebox (tt n) (form #,s)))]))


@(define (shellbox . s)
   (parameterize ([current-directory (build-path (current-directory) "notes/2/")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(require (for-syntax "../utils.rkt"))
@(define-syntax (shell-expand stx)
   (syntax-case stx ()
     [(_ s ...)
      (parameterize ([current-directory (build-path (current-directory) "notes/2/")])
        (begin (apply shell (syntax->datum #'(s ...)))
	       #'(void)))]))

@;{ Have to compile 42.s (at expand time) before listing it }
@(shell-expand "echo 42 > 42.scm" "racket abscond-compile.rkt 42.scm > 42.s")

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

@filebox-include[codeblock "abscond-interp.rkt" "notes/2/abscond-interp.rkt"]

The details here aren't important (and you won't be asked to write
this kind of code), but this program @racket[read]s the contents of a
file given on the command line.  If it's an integer, i.e. a
well-formed Abscond program, then it runs the intepreter and displays
the result.

For example:
@shellbox["echo 42 > 42.scm" "racket abscond-interp.rkt 42.scm"]


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
   (𝑨 i i)])

@(centered (render-judgment-form 𝑨))

Here, we are defining a binary relation, called @render-term[A 𝑨],
and saying every integer paired with itself is in the relation.  So
@math{(2,2)} is in @render-term[A 𝑨], @math{(5,5)} is in
@render-term[A 𝑨], and so on.

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
𝑨], then @racket[(abscond-interp e)] equals @racket[i].}

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

@shellbox["gcc -m64 -c -o main.o main.c"]

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

@shellbox[
  (format "nasm -f ~a -o 42.o 42.s"
          (case (system-type 'os)
            [(macosx) "macho64"]
            [(unix) "elf64"]))]

This creates @tt{42.o}, an object file containing the instructions
above (in binary format).

We can link this file with the run-time to produce an executable file:

@shellbox["gcc main.o 42.o -o 42.run"]

This creates the file @tt{42.run}, an exectuable program:

@shellbox["./42.run"]

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

@shellbox["racket abscond-compile.rkt 42.scm"]

Using a Makefile, we can capture the whole compilation dependencies as:

@filebox-include[fancy-make "Makefile" "notes/2/Makefile"]

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

@shellbox["time -p racket abscond-interp.rkt 42.scm"]

Compiling:

@shellbox["time -p ./42.run"]

@subsection{But is it @emph{Correct}?}

At this point, we have a compiler for Abscond.  But is it correct?

Here is a statement of compiler correctness:

@bold{Compiler Correctness}: @emph{For all expressions @racket[e] and
integers @racket[i], if (@racket[e],@racket[i]) in @render-term[A
𝑨], then @racket[(asm-interp (abscond-compile e))] equals
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

@verbatim|{
(interp-abscond e) = (interp-asm (compile-abscond e))
}|

But we don't actually have @racket[interpret-asm], a function that
interprets the Asm code we generate.  Instead we printed the code and
had @tt{gcc} assembly and link it into an executable, which the OS
could run.  But this is a minor distinction.  We can write
@racket[interp-asm] to interact with the OS to do all of these steps.

Here's such a definition.  (Again: the details here are not important
and we won't ask you to write or understand this code, but roughly,
all it's doing is writing emitting assembly (to a temporary file) and
calling @tt{make} to build the executable, then running it and parsing
the result:

@filebox-include[codeblock "asm-interp.rkt" "notes/2/asm-interp.rkt"]

This is actually a handy tool to have for experimenting with
compilation within Racket:

@examples[#:eval ev
(interp-asm (compile-abscond 42))
(interp-asm (compile-abscond 37))
(interp-asm (compile-abscond -8))
]

This of course agrees with what we will get from the interpreter:

@examples[#:eval ev
(abscond-interp 42)
(abscond-interp 37)
(abscond-interp -8)
]

We can turn this in a @bold{property-based test}, i.e. a function that
computes a test expressing a single instance of our compiler
correctness claim:
@examples[#:eval ev
(define (check-compiler e)
  (check-eqv? (abscond-interp e)
              (interp-asm (compile-abscond e))))

(check-compiler 42)
(check-compiler 37)
(check-compiler -8)
]

This is a powerful testing technique when combined with random
generation.  Since our correctness claim should hold for @emph{all}
Abscond programs, we can randomly generate @emph{any} Abscond program
and check that it holds.

@examples[#:eval ev
(check-compiler (random 100))

; test 10 random programs
(for ([i (in-range 10)])
  (check-compiler (random 10000)))
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

@section{Let's Do It Again!}

We've seen all the essential peices (a grammar, an AST data type
definition, an operational semantics, an interpreter, a compiler,
etc.) for implementing a programming language, albeit for an amazingly
simple language.

We will now, through a process of @bold{iterative refinement}, grow
the language to have an interesting set of features.

@subsection{Blackmail: incrementing and decrementing}

Our second language, which subsumes Abscond, is @bold{Blackmail}.
Expressions in Blackmail include integer literals and increment and
decrement operations.  It's still a dead simple language, but at least
programs @emph{do} something.

@subsection{Abstract syntax for Blackmail}

A Blackmail program consists of a single expression, and the grammar
of expressions is:

@centered{@render-language[B]}

So, @racket[0], @racket[120], and @racket[-42] are Blackmail programs,
but so are @racket['(add1 0)], @racket['(sub1 120)], @racket['(add1
(add1 (add1 -42)))].

A datatype for representing expressions can be defined as:

@#reader scribble/comment-reader
(racketblock
;; type Expr = 
;; | Integer
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
)


@subsection{Meaning of Blackmail programs}

The meaning of a Blackmail program depends on the form of the expression:

@itemlist[
@item{the meaning of an integer literal is just the integer itself,}
@item{the meaning of an increment expression is one more than the meaning of its subexpression, and}
@item{the meaning of a decrement expression is one less than the meaning of its subexpression.}]

The operational semantics reflects this dependence on the form of the
expression by having three rules, one for each kind of expression:

@(require (for-syntax racket/match))
@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(with-unquote-rewriter
   (lambda (lw)
     (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
   (with-compound-rewriters (['+ (rewrite '+)]
                             ['- (rewrite '–)])
     (centered (begin (judgment-form-cases '(0)) (render-judgment-form 𝑩))
               (hspace 4)
               (begin (judgment-form-cases '(1)) (render-judgment-form 𝑩))
	       (hspace 4)
	       (begin (judgment-form-cases '(2)) (render-judgment-form 𝑩)))))

The first rule looks familiar; it's exactly the semantics of integers
from Abscond.  The second and third rule are more involved.  In
particular, they have @bold{premises} above the line.  If the premises
are true, the @bold{conclusion} below the line is true as well.  These
rules are @emph{conditional} on the premises being true.  This is in
contrast to the first rule, which applies unconditionally.

We can understand these rules as saying the following:
@itemlist[
@item{For all integers @math{i}, @math{(i,i)} is in @render-term[B 𝑩].}

@item{For expressions @math{e_0} and all integers @math{i_0} and
@math{i_1}, if @math{(e_0,i_0)} is in @render-term[B 𝑩] and @math{i_1
= i_0 + 1}, then @math{(@RACKET['(add1 (UNSYNTAX @math{e_0}))], i_1)}
is in @render-term[B 𝑩].}

@item{For expressions @math{e_0} and all integers @math{i_0} and
@math{i_1}, if @math{(e_0,i_0)} is in @render-term[B 𝑩] and @math{i_1
= i_0 - 1}, then @math{(@RACKET['(sub1 (UNSYNTAX @math{e_0}))], i_1)}
is in @render-term[B 𝑩].}
]

These rules are @bold{inductive}.  We start from the meaning of
integers and if we have the meaning of an expression, we can construct
the meaning of a larger expression.

This may seem a bit strange at the moment, but it helps to view the
semantics through its correspondence with an interpreter, which given
an expression @math{e}, computes an integer @math{i}, such that
@math{(e,i)} is in @render-term[B 𝑩].

Just as there are three rules, there will be three cases to the
interpreter, one for each form of expression:

@filebox-include[codeblock "blackmail-interp.rkt" "notes/2/blackmail-interp.rkt"]

@examples[#:eval ev
(blackmail-interp 42)
(blackmail-interp -7)
(blackmail-interp '(add1 42))
(blackmail-interp '(sub1 8))
(blackmail-interp '(add1 (add1 (add1 8))))
]

Here's how to connect the dots between the semantics and interpreter:
the interpreter is computing, for a given expression @math{e}, the
integer @math{i}, such that @math{(e,i)} is in @render-term[B 𝑩].  The
interpreter uses pattern matching to determine the form of the
expression, which determines which rule of the semantics applies.

@itemlist[

@item{if @math{e} is an integer @math{i}, then we're done: this is the
right-hand-side of the pair @math{(e,i)} in @render-term[B 𝑩].}

@item{if @math{e} is an expression @RACKET['(add1 (UNSYNTAX
@math{e_0}))], then we recursively use the interpreter to compute
@math{i_0} such that @math{(e_0,i_0)} is in @render-term[B 𝑩].  But
now we can compute the right-hand-side by adding 1 to @math{i_0}.}

@item{if @math{e} is an expression @RACKET['(sub1 (UNSYNTAX
@math{e_0}))], then we recursively use the interpreter to compute
@math{i_0} such that @math{(e_0,i_0)} is in @render-term[B 𝑩].  But
now we can compute the right-hand-side by substracting 1 from @math{i_0}.}

]

This explaination of the correspondence is essentially a proof (by
induction) of the interpreter's correctness:

@bold{Interpreter Correctness}: @emph{For all Blackmail expressions
@racket[e] and integers @racket[i], if (@racket[e],@racket[i]) in
@render-term[B 𝑩], then @racket[(blackmail-interp e)] equals
@racket[i].}



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