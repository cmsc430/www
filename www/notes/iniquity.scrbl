#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  "hustle/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "iniquity" f))))))
	   '() #;'("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Iniquity"]{Iniquity: function definitions and calls}

@table-of-contents[]

@section[#:tag-prefix "iniquity"]{Functions}

Our programming languages so far have been impoverished in the
following sense: in order to process arbitrarily large data, the
programs themselves must be proportionally as large.  Want to compute
something over a billion element list?  You'll need a billion
expressions.  Consequently, the expressiveness of our language is
severely restricted.

Let's now remove that restriction by incorporating @bold{functions},
and in particular, @bold{recursive functions}, which will allow us to
compute over arbitrarily large data with finite-sized programs.

Let's call it @bold{Iniquity}.

We will extend the syntax by introducing a new syntactic category of
programs, which have the shape:

@racketblock[
(begin
  (define (_f0 _x0 ...) _e0)
  (define (_f1 _x1 ...) _e1)
  ...
  _e)]

And the syntax of expressions will be extended to include function calls:

@racketblock[
(_fi _e0 ...)
]

where @racket[_fi] is one of the function names defined in the program.

Note that functions can have any number of parameters and,
symmetrically, calls can have any number of arguments.  A program
consists of zero or more function definitions followed by an
expression.


@section[#:tag-prefix "iniquity"]{An Interpreter for Functions}

Writing an interpreter for Inquity is not too hard.  The main idea is
that the interpretation of expression is now parameterized by a set of
function definitions from the program.  It serves as a second kind of
environment that gets passed around and is used to resolve function
definitions when interpreting function calls.

The way a function call is interpreted is to first interpret all of
the arguments, building up a list of results.  Then the definition of
the function being called is looked up.  If the function has the same
number of parameters as there are arguments in the call, the body of
the function is interpreted in an enviorment that maps each parameter
to to the corresponding argument.  That's it.

@codeblock-include["iniquity/interp.rkt"]

A couple of things to note:

@itemlist[

@item{since the function definition environment is passed along even
when interpreting the body of function definitions, this
interpretation supports recursion, and even mutual recursion.}

@item{functions are @emph{not} values (yet).  We cannot bind a
variable to a function.  We cannot make a list of functions.  We
cannot compute a function.  The first position of a function call is a
function @emph{name}, not an arbitrary expression.  Nevertheless, we
have significantly increased the expressivity of our language.}

]

