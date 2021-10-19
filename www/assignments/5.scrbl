#lang scribble/manual
@title[#:tag "Assignment 5" #:style 'unnumbered]{Assignment 5: Arity Checking, Rest Arguments, and Case Functions}

@(require (for-label (except-in racket ...)))
@(require "../notes/ev.rkt"
          "../notes/utils.rkt")

@(ev `(current-directory ,(path->string (build-path notes "iniquity-plus"))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("ast.rkt" "parse.rkt" "interp.rkt"))


@bold{Due: Tuesday, November 2nd, 11:59PM EDT}

The goal of this assignment is to extend a compiler with arity
checking for function calls and to add new kinds of function parameter
features.

You are given a repository with a starter compiler similar to the
@seclink["Iniquity"]{Iniquity} language we studied in class.  You are
tasked with:

@itemlist[

@item{implementing run-time arity checking for function calls,}

@item{extending function definition to include ``rest argument''
parameters for writing variable arity functions,}

@item{extending function definitions to include
@racket[case-lambda]-style multiple-arity functions, and}

@item{extending the arity checking features to handle these new forms
of function definitions.}
]

Unlike previous assignments, you do not need to bring forward your
past features to this language; there is no need to implement
@racket[cond], @racket[case], etc.

Be sure to read the entire problem description before starting.  There
are a number of @secref[#:tag-prefixes '("a5-")]{Suggestions} on how to
approach the assignment near the end.

@section[#:tag-prefix "a5-" #:style 'unnumbered #:tag "update"]{Update a86}

There have been some changes to a86 that you'll need.  You can update
the @tt{langs} package with the following:

@verbatim|{raco pkg update langs}|

@section[#:tag-prefix "a5-" #:style 'unnumbered #:tag "arity"]{Checking arity}

In @seclink["Iniquity"]{Iniquity}, we implemented a language with
function definitions and calls.  We noted that bad things can happen
when a function is called with the incorrect number of arguments.
While it's possible to statically check this property of Iniquity
programs, it's not possible in more expressive languages and arity
checking must be done at run-time.  You are tasked with implementing
such a run-time arity checking mechanism.

Here is the basic idea.  You need to add a run-time checking mechanism
that will cause the following program to signal an error:

@#reader scribble/comment-reader
(racketblock
(define (f x y) (+ x y))
(f 1)
)

The function call knows how many arguments are given and the function
definition knows how many argument are expected.  The generated code
should check that these two things match when the function is called.

A simple way to do this is to pick a designated register that will be
used for communicating arity information.  The caller should set the
register to the number of arguments before jumping to the function.
The function should check this number against the expected number and
signal an error when they don't match.

[NOTE on stack alignment on errors.]

You should modify @racket[compile-app] and @racket[compile-define] to
implement this part of the assignment.

@section[#:tag-prefix "a5-" #:style 'unnumbered #:tag "rest"]{Rest
arguments}

Many languages including JavaScript, C, and Racket include a facilty
for defining functions that take a ``rest argument'' which allows the
function to be called with more arguments than expected and these
additional arguments will be bound to a single value that collects all
of these arguments.  In Iniquity, as in Racket, the obvious way of
collecting these arguments into a single value is to use a list.

Here are some examples:

@itemlist[

@item{@racket[(define (f . xs) ...)]: this function takes @emph{any} number
of arguments and binds @racket[xs] to a list containing all of them,}

@item{@racket[(define (f x . xs) ...)]: this function takes @emph{at
least} one argument and binds @racket[x] to the first argument and
@racket[xs] to a list containing the rest.  It's an error to call this function
with zero arguments.}

@item{@racket[(define (f x y z . xs) ...)]: this function takes
@emph{at least} three arguments and binds @racket[x], @racket[y], and
@racket[z] to the first three arguments and @racket[xs] to a list
containing the rest.  It's an error to call this function with 0, 1,
or 2 arguments.}
]

Here are some examples in Racket to get a sense of the behavior:

@ex[
(define (f . xs) (list xs))
(f)
(f 1)
(f 1 2)
(f 1 2 3)
(f 1 2 3 4)
(define (f x . xs) (list x xs))
(eval:error (f))
(f 1)
(f 1 2)
(f 1 2 3)
(f 1 2 3 4)
(define (f x y z . xs) (list x y z xs))
(eval:error (f))
(eval:error (f 1))
(eval:error (f 1 2))
(f 1 2 3)
(f 1 2 4)
]

The code generated for a function call should not change---other than
what you did for @secref[#:tag-prefixes '("a5-") "arity"]: it should
pass all of the arguments on the stack along with information about
the number of arguments.

The compilation of function definitions that use a rest argument
should generate code that check that the given number of arguments is
acceptable and should generate code to pop all ``extra'' arguments off
the stack and construct a list which is then bound to the rest
parameter.

It is worth remembering that arguments are pushed on the stack in such a
way that the last argument is at the top of the stack.  This has the
benefit of making it easy to pop off the extra arguments and to
construct a list with the elements in the proper order.

HINT: the function definition knows the number of ``required''
arguments, i.e. the minimum number of arguments the function can be
called with---call this @math{m}---and the caller communicates how
many actual arguments have been supplied---call this @math{n}.  The
compiler needs to generate a loop that pops @math{n-m} times,
constructing a list with with popped elements, and then finally pushes
this list in order to bind it to the rest parameter.

@section[#:tag-prefix "a5-" #:style 'unnumbered #:tag "case-lambda"]{Arity dispatch}

Some languages such as Java, Haskell, and Racket make it possible to
overload a single function name with multiple definitions where the
dispatch between these different definitions is performed based on the
number (or kind) of arguments given at a function call.

In Racket, this is accomplished with the @racket[case-lambda] form for
constructing multiple-arity functions.

Here is an example:

@ex[
(define f
  (case-lambda
    [(x) "got one!"]
    [(p q) "got two!"]))

(f #t)
(f #t #f)
(eval:error (f #t #f 0))
]

This function can accept @emph{either} one or two arguments.  If given
one argument, it evaluates the right-hand-side of the first clause
with @racket[x] bound to that argument.  If given two arguments, it
evaluates the right-hand-side of the second clause with @racket[p] and
@racket[q] bound to the arguments.  If given any other number of
arguments, it signals an error.

A @racket[case-lambda] form can have any number of clauses (including
zero!) and the first clause for which the number of arguments is
acceptable is taken when the function is called.

Note that @racket[case-lambda] can be combined with rest arguments so.
A clause that accepts any number of arguments is written by simply
listing a parameter name (no parentheses).  A clause that accepts some
non-zero minimum number of parameters is written with a dotted
parameter list.

For example:

@ex[
(define f
  (case-lambda
    [(x y z . r) (length r)]
    [(x) "just one!"]))

(f 1 2 3 4 5 6)
(f #t)
(eval:error (f))
(eval:error (f 1 2))]

This function takes three or more arguments @emph{or} one argument.  Any
other number of arguments (i.e. zero or two) results in an error.

@ex[
(define f
  (case-lambda
    [(x y z) "three!"]
    [xs (length xs)]))

(f)
(f 1 2)
(f 1 2 3)
(f 1 2 3 4 5 6)
]

This function takes any number of arguments, but when given three, it
produces @racket["three!"]; in all other cases it produces the number
of arguments.

@;{
@section[#:tag-prefix "a5-" #:style 'unnumbered #:tag "arity"]{Apply}

Apply is the yin to the yang of rest arguments (or maybe the other way
around).  Whereas a rest argument let's a function take arbitrarily
more arguments and package them up as a list, @racket[apply] will
apply a function to a list as though the elements of the list were
given as arguments.

@ex[
(define (f x y) (+ x y))
(apply f (list 1 2))
(define (flatten ls)
  (apply append ls))
(flatten (list (list 1 2) (list 3 4 5) (list 6)))
(define (sum ls)
  (apply + ls))
(sum (list 5 6 7 8))
]

Here you can see @racket[apply] taking two things: a function and
single argument which is a list.  It is apply to call the function
with the elements of the list as the arguments.

It turns out, @racket[apply] can also take other arguments in addition
to the list and pass them along to the function.

@ex[
(define (f x y) (+ x y))
(apply f 1 (list 2))
(apply list 1 2 3 4 (list 5 6 7))
]

Note that if the function expects a certain number of arguments and the list has
a different number of elements, it results in an arity error:

@ex[
(define (f x y) (+ x y))
(eval:error (apply f (list 1 2 3)))
]

A new form of expression has been added to the @tt{Expr} AST type:

@#reader scribble/comment-reader
(racketblock
;; type Expr = ...
;;           | (Apply ID [Listof Expr] Expr)
)

}

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Representing the
syntax of function definitions}

The @seclink["Iniquity"]{Iniquity} language has a single function
definition form: @racket[(define (_f _x ...) _e)] which is represented
with the following AST type:

@#reader scribble/comment-reader
(racketblock
;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)
)

Because there are three different forms of function definition in
Iniquity+, we use the following AST representation:

@#reader scribble/comment-reader
(racketblock
;; type Defn = (Defn Id Fun)
(struct Defn (f fun) #:prefab)

;; type Fun = (FunPlain [Listof Id] Expr)
;;          | (FunRest [Listof Id] Id Expr)
;;          | (FunCase [Listof FunCaseClause])
;; type FunCaseClause = (FunPlain [Listof Id] Expr)
;;                    | (FunRest [Listof Id] Id Expr)
(struct FunPlain (xs e)   #:prefab)
(struct FunRest  (xs x e) #:prefab)
(struct FunCase  (cs)     #:prefab)
)

What used to be represented as @racket[(Defn _f _xs _e)] is now
represented as @racket[(Defn _f (FunPlain _xs _e))].


The parser already works for these new form of function definitions.
Here are some examples of how function definitions are parsed, but you
are encouraged to try out more to get a better sense:

@ex[
(parse-define '(define (f x) x))
(parse-define '(define (f . xs) xs))
(parse-define '(define (f x y z . q) q))
(parse-define
  '(define f
     (case-lambda
       [(x y) 2]
       [(z) 1]
       [(a b c . d) "3+"]
       [q "other"])))
]

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Starter code}

The code given to you is just an implementation of Iniquity, but
updated to parse the new forms of function definitions and
re-organized slightly to match the new AST representation.

The code also includes a full implementation of the interpreter, so
you do not need to update @racket[interp.rkt] and can use the
interpreter to guide your implementation of the compiler.

@ex[
(interp
  (parse '[(define (f x) x)
           (f 1)]))
(interp
  (parse '[(define (f . x) x)
           (f 1)]))
(interp
  (parse '[(define (f . x) x)
           (f)]))
(interp
  (parse '[(define (f . x) x)
           (f 1 2 3 4 5)]))
(interp
  (parse '[(define f
             (case-lambda
               [(x y) 2]
               [(z) 1]
               [(a b c . d) "3+"]
               [q "other"]))
	    (cons (f 7)
	          (cons (f 3 4)
		        (cons (f)
			      (cons (f 7 8 9 10 11)
			            '()))))]))
]


Thus, you should only need to modify @racket[compile.rkt].

A small number of test cases are given as usual.


@section[#:tag-prefix "a5-" #:style 'unnumbered]{Suggestions}

This is a tricky assignment.  The amount of code you have to write is
pretty small, however you may spend a long time slogging through the
assignment if your approach is to hack first, think later.

Here are some suggestions for how to approach the assignment.  Make
sure you get each of the pieces working before moving on.

@itemlist[

@item{Start with @secref[#:tag-prefixes '("a5-") "arity"]; this should
be pretty easy.  Make sure it works for plain function definitions.}

@item{Move on to @secref[#:tag-prefixes '("a5-") "rest"].  You could
start by emitting code that checks that the arguments are acceptable,
popping the appropriate number of arguments off (and ignoring the
elements), the pushing the empty list.  This will work like a rest arg
in that it should accept any number of arguments beyond the required
minimum, but the rest argument will always be bound to empty.  Once
working, try to modify the code to build a list as it pops arguments.
Test that it works.}

@item{Move on to @secref[#:tag-prefixes '("a5-")
"case-lambda"]. Remember that you have a compiler for plain and rest
argument functions at this point.  That should come in handy.  Think
of @racket[case-lambda] as generating a set of function definitions
(with generated names), and then the main work of @racket[case-lambda]
is determing which of the generated functions to call, given the
specific number of arguments passed in by the caller.  When you find
the function that fits, jump to it.  You might start by only handling
plain function clauses in @racket[case-lambda] before moving on to
handling rest argument functions, too.}

]

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Submitting}

You should submit on Gradescope. You should submit a zip file that has
exactly the same structure that the stub contains. We will only use
the @tt{compile.rkt} files for grading, so make sure all your work is
contained there!












