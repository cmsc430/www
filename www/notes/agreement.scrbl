#lang scribble/manual

@require[scriblib/footnote]

@define-footnote[footnote make-footnote]

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
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "agreement" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt" "ast.rkt" "parse.rkt"))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "agreement")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(require (for-syntax "../utils.rkt" racket/base "utils.rkt"))
@(define-syntax (shell-expand stx)
   (syntax-case stx ()
     [(_ s ...)
      (parameterize ([current-directory (build-path notes "agreement")])
        (begin (apply shell (syntax->datum #'(s ...)))
	       #'(void)))]))

@title[#:tag "Agreement"]{Agreement: a language of numbers between friends}

@emph{It's just a phase!}

@table-of-contents[]

@section[#:tag-prefix "agreement"]{Let's Agree}

After looking at @secref{Abscond} it may seem a little odd that we are building
both a compiler @emph{and} an interpreter. Furthermore, the @emph{difference}
between the two may not be immediately clear in our first encounter with both.

When implementing a language there are many reasons for choosing to implement
an interpreter or a compiler or, increasingly, both@footnote{In fact, many
modern language implementations have a compiler as part of their interpreter,
this is is known as
@link["https://en.wikipedia.org/wiki/Just-in-time_compilation"]{JIT
compilation}.}

@make-footnote[]

The language described here, Agreement, is a small change to Abscond that will
help us in differentiating between the interpreters and compilers in this
course. In particular we will explore the notion of a @emph{phase distinction},
by thinking about @emph{when} things happen. Additionally, there is a
distinction in @emph{how things run}, which we will explore below.

@section[#:tag-prefix "agreement"]{Abstract syntax for Agreement}

Abscond was a simple language with the following grammar:

@#reader scribble/comment-reader
(racketblock
;; type expr = integer
)

An Agreement program, like and Abscond program, consists of a single
expression, and the grammar of expressions introduces one new concept:

@#reader scribble/comment-reader
(racketblock
;; type expr = integer
;;           | get-int
)

So, @racket[0], @racket[120], @racket[-42], are programs, just as in Abscond,
but so is @racket[get-int]. @racket[get-int] is a primitive (something that we
the language implementers provide in the language) that retrieves an integer
from standard input (i.e. the user provides it).

Now that our programs can take more than one shape (barely!) it's a good time
to make our Abstract Syntax Tree explicit:

@codeblock-include["agreement/ast.rkt"]

We are using Racket's @racket[struct] feature to provide the nodes of our AST.
The @racket[(int-e)] (pronounced `int expression') struct takes a single
argument: the integer value, while the @racket[(get-int)] node takes no
arguments as we don't know what value it will represent.

@section[#:tag-prefix "agreement"]{Meaning of Agreement programs}

We can write an ``interpreter'' that consumes an expression and
produces it's meaning:

@codeblock-include["agreement/interp.rkt"]

Notice that the meaning of @racket[get-int] is `just' the call to the
@emph{racket} function @racket[get-int]. Understanding how that function is
implemented is not important for our purposes and we can treat it as a
black-box.

@#reader scribble/comment-reader
(examples #:eval ev
(interp (parse 42))
(interp (parse -8))
)

Earlier I mentioned that the compiler and interpreter differ in how things run.
Notice that for the above, we need a working Racket system in order to
@emph{run} the programs through the interpreter and get a result.  Soon we will
contrast this with how we run the program that the compiler produces.

We can add a command line wrapper program for interpreting Agreement
programs saved in files:

@codeblock-include["agreement/interp-file.rkt"]


For example:
@shellbox["echo '#lang racket\\nget-int' > example.agr"
          "echo 1024 | racket -t interp-file.rkt -m example.agr"
          "echo 2048 | racket -t interp-file.rkt -m example.agr"]

A few observations

@itemlist[

@item{Our program now uses our primitive: @racket[get-int]}

@item{When interpreting our program, we pipe the value @racket[1024] as input}

@item{We print the friendly message ``Interpreting...'' as we start interpreting our program}

@item{When we ran our program the second time, we saw ``Interpreting...'' again}

]

Unlike Abscond, we will not be providing an Operational Semantics for
agreement, as to do so fully would require some more advanced techniques that
are more appropriate for a latter stage of the course.

@section[#:tag-prefix "agreement"]{Running an Agreement}

Unlike in our interpreter, we cannot use a Racket function for
@racket[get-int]. This is because Racket functions are not available to use
when we execute our assembly programs. So instead we expand our runtime-system
to provide us with this functionality:


@filebox-include[fancy-c "agreement/main.c"]

The other side of this coin is that we do not need Racket in order to
@emph{run} our programs, only to compile them.

In Agreement, the runtime system calls the function and prints the result as
before, but now it also provides us with the implementation of the
@racket[get-int] primitive operation.

@section[#:tag-prefix "agreement"]{Compiling an Agreement}

The distinction between the compiler for Abscond and for Agreement is not
important for this stage in the course, and as such we omit the details here.
Later in the semester we will cover the techniques used in compiling this
primitive operation.

That said, we can still observe some important points from running some
examples. Let's start with compiling the same program we interpreted.

@shellbox["echo '#lang racket\\nget-int' > example.rkt"
          "make example.run"]

Notice that we now see ``Compiling...'' when our compiler is run on the input
program. Let's run the compiled program now:

@shellbox["echo '#lang racket\\nget-int' > example.rkt"
          "make example.run"
          "echo 1024 | ./example.run"
          "echo 2028 | ./example.run"]

Even though we run the program multiple times, we only see ``Compiling...'' when
we run our compiler. This helps us illustrate an important aspect of
compilation. For the most part, you compile a program once, but can run it many
times. In our case, because the target language is x86_64, we do not never need
Racket to be installed in order to run our programs. This means that we can
compile our programs on a system with a Racket environment, but run the program
on any system that is compatible with the executable format we get.

This is very different from how our interpreter works. In order to run a
program with our interpreter, we need Racket!

Below we have the rest of the necessary files for Agreement, in case you're interested:

@codeblock-include["agreement/asm/printer.rkt"]


@codeblock-include["agreement/compile-file.rkt"]

@filebox-include[fancy-make "agreement/Makefile"]
