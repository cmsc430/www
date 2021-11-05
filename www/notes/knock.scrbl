#lang scribble/manual

@(require (for-label (except-in racket compile ...) a86))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../fancyverb.rkt"	  
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "knock")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))


@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path notes "knock"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt" "unload-bits-asm.rkt"))

@(define this-lang "Knock")

@title[#:tag this-lang]{@|this-lang|: Pattern matching}

@table-of-contents[]

@section[#:tag-prefix "knock"]{Matching}

One feature we've taken advantage extensively in the writing of our
compilers is the Racket's @racket[match] facility for pattern
matching.

Let's add a similar feature to our own language.

We'll call it @bold{@this-lang}!

In @|this-lang|, we will support a limited form of pattern matching of
the form:

@racketblock[
(match _e
  [_p0 _e0]
  ...)
]

A pattern matching expression is used to perform case-analysis,
deconstruction, and binding on the value produced by @racket[_e].  A
match consists of any number of clauses, where each clause consists of
a pattern @racket[_pi] and expression @racket[_ei].  Each @racket[_pi]
is a @emph{pattern}, which can include literal booleans, characters,
integers, and the empty list, or can be a pattern variable, a
wildcard, a @racket[cons]-pattern, or a @racket[and]-pattern. Clauses
are matched in the order in which they appear and if a pattern matches
the value of @racket[_e], then the corresponding expression is
evaluated in an environment that binds any pattern variables to the
matching parts of @racket[_e]'s value.  If no patterns match
@racket[_e]'s value, an error is signalled.

The syntax is extended as follows:

@codeblock-include["knock/ast.rkt"]


@section[#:tag-prefix "knock"]{Match by Example}

Since we've been using pattern matching throughout the course, it
probably is pretty natural at this point, but let's quickly walk
through some examples to try and disentangle the different aspects of
@racket[match].

Perhaps the simplest form of a @racket[match]-expression uses a
pattern that just consists of a variable, e.g.

@racketblock[
(match _e
  [x _e0])]

This expression is equivalent to @racket[(let ((x _e)) _e0)] because a
pattern variable matches any value and binds that name in the scope of
its right-hand expression.  We can see from this example that
@racket[match] is doing variable binding.

Relatedly, a ``wildcard'' pattern can be used to match anything
@emph{without} binding the value to a name:

@racketblock[
(match _e
  [_ _e0])]

This expression is equivalent to @racket[(begin _e _e0)].

Another simple form of pattern is to use a literal such as an integer,
character, etc. which matches when the value is the same as the
literal.  This form of pattern doesn't bind any names, but is used to
discriminate between different cases of what the value may be.  For
example:

@racketblock[
(match _e
  [#f _e1]
  [_  _e2])]

This expression is equivalent to @racket[(if _e _e2 _e1)]. Here we can
see that @racket[match] is doing conditional evaluation, selecting
@racket[_e1] if @racket[_e] produces @racket[#f], and selecting
@racket[_e2] otherwise.

A more complicated pattern involves a constructor-style pattern like
@racket[cons]:

@racketblock[
(match _e
  [(cons x y) _e1]
  [_ _e2])
]

Here, the @racket[cons] pattern is both discriminating between
@racket[cons] and non-@racket[cons] values, matching only when
@racket[_e] is a pair, but also binding the names @racket[x] and
@racket[y] to the components of the pair when the value is in fact a
pair; these names are bound in the scope of @racket[_e1].  In this
way, the pattern is used to @emph{destructure} compound values such as
pair.

The @racket[x] and @racket[y] in this example are actually just
instances of patterns themselves, and patterns can be nested
arbitrarily deep.  So for example, if we wanted only to match
a pair containing @racket[1] and @racket[2], we could write:

@racketblock[
(match _e
  [(cons 1 2) _e1]
  [_ _e2])]

The @racket[and]-pattern is used to match the conjunction of two
patterns, so @racket[(and _p1 _p2)] matches whenever @racket[_p1] and
@racket[_p2] both match and binds all of the names in @racket[_p1] and
@racket[_p2].  For example,

@racketblock[
(match _e
  [(and (cons 1 x) (cons y 2)) _e1]
  [_ _e2])
]

The first clause matches when @racket[_e] evaluates to @racket[(cons 1
2)] and binds the name @racket[x] to @racket[2] and @racket[y] to
@racket[1] in the scope of @racket[_e1].


Here are some complete examples and how they are parsed:

@ex[
(parse-e '(match z [x x]))
(parse-e '(match z [_ #t]))
(parse-e '(match z [1 #t]))
(parse-e '(match z [1 #t] [2 #f]))
(parse-e '(match z [(cons x y) #t]))
(parse-e '(match z [(cons 1 2) #t]))
(parse-e '(match z [(and (cons x 2) (cons 1 y)) #t]))
(parse-define
  '(define (length xs)
     (match xs
       ['() 0]
       [(cons x xs)
        (add1 (length xs))])))
]

@section[#:tag-prefix "knock"]{An Interpreter for Pattern Matching}

At the heart of interpreter for @this-lang is the function:

@#reader scribble/comment-reader
(racketblock
;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r) ...)
)

This function takes a single pattern and value, along with an
environment, and determines whether the pattern matches the value, and
if so, an environment that binds the variables in the pattern to the
sub-parts of the value that match.  If the pattern doesn't match,
@racket[#f] is produced.

So for example, if the pattern is simply a variable @racket[x], the
function produces @racket[r] extended to bind @racket[x] to
@racket[v].  If the pattern is a wildcard, it produces @racket[r],
indicating a match, but with no new bindings.  Likewise, if the
pattern is a literal, it produces @racket[r] when the value is the
same as the literal.  The more interesting cases are of @racket[cons]-
and @racket[and]-patterns which recursively match the sub-patterns.


It's important to see that this function's return type is
communicating multiple things at the same time.  If the pattern
doesn't match, it produces @racket[#f].  If it produces an
environment, it means the pattern matched @emph{and} the environment
communicates the binding of the pattern variables to values.

Let's consider some examples:
@ex[
(interp-match-pat (PWild) 99 '())
]

Here the pattern matches, but binds no variables so the result is the
same environment as given.

@ex[
(interp-match-pat (PVar 'x) 99 '())
]

Here the pattern matches and binds @racket[x] to @racket[99], which is
reflected in the output environment.

@ex[
(interp-match-pat (PLit 99) 99 '())
]

Here the pattern matches but binds nothing.

@ex[
(interp-match-pat (PLit 100) 99 '())
]

Here the pattern doesn't match.


@ex[
(interp-match-pat (PAnd (PLit 99) (PVar 'x)) 99 '())
]

Here the pattern matches and binds @racket[x] to @racket[99].

@ex[
(interp-match-pat (PAnd (PLit 100) (PVar 'x)) 99 '())
]

Here the pattern doesn't match.

@ex[
(interp-match-pat (PCons (PVar 'x) (PVar 'y)) 99 '())
]

Here the pattern doesn't match.

@ex[
(interp-match-pat (PCons (PVar 'x) (PVar 'y)) (cons 99 100) '())
]

Here the pattern matches and binds @racket[x] to @racket[99] and
@racket[y] to @racket[100].

As you can see, the patterns can be nested arbitrarily deep but the
environment produced will bind each variable to the appropriate
sub-part of the given value:

@ex[
(interp-match-pat (PCons (PCons (PVar 'x) (PVar 'y))
                         (PCons (PVar 'p) (PVar 'q)))
                  (cons (cons 99 100)
                        (cons #t #f))
                  '())
]


With @racket[interp-match-pat], we can then build up the function for
interpreting a @racket[match] expression:

@#reader scribble/comment-reader
(racketblock
;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds) ...)
)

This function traverses the patterns in order until finding one that
matches (using @racket[interp-match-pat]) and then evaluating the
corresponding right-hand expression in the environment that
@racket[interp-match-pat] produced.

The complete interpreter:

@codeblock-include["knock/interp.rkt"]

@section[#:tag-prefix "knock"]{A Compiler for Pattern Matching}

The compilation of pattern matching expression is significantly more
complicated compared to interpretation.

Most of the complication is due to the fact that the computation of
the binding structure in the interpreter must be split and mirrored
across compile-time and run-time in the compiler.  Each
right-hand-side of a clause must be compiled in a static environment
that is dependent on the variables occurring in the left-hand-side
pattern.  At run-time, these variables will be bound by pushing parts
of the matched value on the stack.

To make matters worse, the stack will also be needed to save
intermediate results for later processing.  For example, in matching a
@racket[cons]-pattern, we must push the @racket[cdr] of the pair on
the stack while pattern-matching the @racket[car].

With these concerns in mind, here's the complete compiler:

@codeblock-include["knock/compile.rkt"]
