#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
          "mug/syntax.rkt"
          "mug/pat.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "mug" f))))))
	   '("interp.rkt" "interp-env.rkt" #;"compile.rkt" "syntax.rkt" "pat.rkt" #;"asm/interp.rkt" #;"asm/printer.rkt"))

@title[#:tag "Mug"]{Mug: matching, throwing, quoting}

@table-of-contents[]

@section[#:tag-prefix "mug"]{Scaling up with syntax}

We have developed a small, but representative functional programming
language.  But there's still a long way to go from our Loot language
to the kind of constructs we expect in a modern, expressive
programming language.  In particular, there's a fairly large gap
between Loot and the subset of Racket we've explored so far in this
class.

For example, our programs have made extensive use of pattern matching,
quotation, quasi-quotation, and lots of built-in functions.  In this
section, we'll examine how to scale Loot up to a language that's nicer
to program in.  As we'll see, much of this can be accomplished
@emph{without extending the compiler}.  Rather we can explain these
language features by @bold{elaboration} of fancier language syntax
into the existing core forms.

In this chapter, we'll explore several ideas at the level of an
interpreter, but the techniques should work just as well for the compiler.

@section[#:tag-prefix "mug"]{The Loot+ interpreter}

Let us start with an interprter for the Loot language, plus all of the
extensions considered in the various assignments up through
@seclink["Assignment 7"]{Assignment 7}.


@codeblock-include["mug/interp-env.rkt"]

@section[#:tag-prefix "mug"]{A bit more sugar}


As we saw in @seclink["Loot"]{Loot}, we can consider syntaxtic
extensions of language that elaborate into the core @tt{Expr} form of
a language.  We saw this with the @racket[define]-form that we rewrote
into @racket[letrec].  We can consider further extensions such as
@racket[and], @racket[or], and even @racket[cond].

Here are functions for transforming each of these forms into simpler
forms:

@#reader scribble/comment-reader
(ex
(define (cond->if c)
  (match c
    [`(cond (else ,e)) e]
    [`(cond (,c ,e) . ,r)
     `(if ,c ,e (cond ,@r))]))

(define (and->if c)
  (match c
    [`(and) #t]
    [`(and ,e) e]
    [`(and ,e . ,r)
     `(if ,e (and ,@r) #f)]))

(define (or->if c)
  (match c
    [`(or) #f]
    [`(or ,e) e]
    [`(or ,e . ,r)
     (let ((x (gensym)))
       `(let ((,x ,e))
          (if ,x ,x (or ,@r))))]))
)

Note that these functions do not necessarily eliminate @emph{all}
@racket[cond], @racket[and], or @racket[or] forms, but rather
eliminate @emph{one} occurrence, potentially creating a new occurrence
within a subexpression:

@ex[
(cond->if '(cond [(even? x) 8] [else 9]))
(cond->if '(cond [else 9]))
(and->if '(and))
(and->if '(and 8))
(and->if '(and 8 9))
(or->if '(or))
(or->if '(or 8))
(or->if '(or 8 9))
]

The idea is that another function will drive the repeated use of these
functions until all these extended forms are eliminated.

You may wonder why the @racket[or] elaboration is complicated by the
@racket[let]-binding. Consider a potential simpler approach:

@#reader scribble/comment-reader
(ex
(define (or->if-simple c)
  (match c
    [`(or) #f]
    [`(or ,e) e]
    [`(or ,e . ,r)
     `(if ,e ,e (or ,@r))]))
)

But compare the elaboration of the following exmample:

@ex[
(or->if-simple '(or (some-expensive-function) #t))
(or->if '(or (some-expensive-function) #t))
]

The second program is much more efficient.  Moreover, if
@racket[some-expensive-function] had side-effects, the first program
would duplicate them, thereby changing the program's intended
behavior.

We can incorporate these new functions into the desugar function,
which will transform extended programs into ``core'' expressions:

@#reader scribble/comment-reader
(ex
;; Expr+ -> Expr
(define (desugar e+)
  (match e+
    [`(begin ,@(list `(define (,fs . ,xss) ,es) ...) ,e)
     `(letrec ,(map (λ (f xs e) `(,f (λ ,xs ,(desugar e)))) fs xss es)
        ,(desugar e))]    
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`',(? symbol? s)      `',s]
    [`(,(? prim? p) . ,es) `(,p ,@(map desugar es))]    
    [`(if ,e0 ,e1 ,e2)     `(if ,(desugar e0) ,(desugar e1) ,(desugar e2))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(desugar e0))) ,(desugar e1))]
    [`(letrec ,bs ,e0)
     `(letrec ,(map (λ (b) (list (first b) (desugar (second b)))) bs)
        ,(desugar e0))]
    [`(λ ,xs ,e0)          `(λ ,xs ,(desugar e0))]
    [`(cond . ,_)          (desugar (cond->if e+))]
    [`(and . ,_)           (desugar (and->if e+))]
    [`(or . ,_)            (desugar (or->if e+))]
    [`(,e . ,es)           `(,(desugar e) ,@(map desugar es))]))
)

Note how a @racket[cond], @racket[and], or @racket[or] form are
transformed and then @racket[desugar]ed again.  This will take care of
eliminating any derived forms introduced by the transformation, which
is useful so that derived forms can be defined in terms of other
derived forms, including itself!

@ex[
(desugar '(cond [(even? x) 8] [else 9]))
(desugar '(cond [else 9]))
(desugar '(and))
(desugar '(and 8))
(desugar '(and 8 9))
(desugar '(or))
(desugar '(or 8))
(desugar '(or 8 9))
]


Derived forms that can be elaborated away by rewriting into more
primitive forms are sometimes called @bold{syntactic sugar} since they
are not fundamental but ``sweeten'' the experience of writing programs
with useful shorthands.  We call the elaboration function @racket[desugar]
to indicate that it is eliminating the syntactic sugar.

@section[#:tag-prefix "mug"]{Exceptional behavior}

To see an example of taking the idea of program transformation as a
method for implementing language features, let's consider the case of
exceptions and exception handlers, a common feature of modern
high-level languages.

Consider the following program for computing the product of all the
elements in a binary tree:

@#reader scribble/comment-reader
(ex
;; BT -> Number
;; Multiply all the numbers in given binary tree
(define (prod bt)
  (match bt
    ['leaf 1]
    [`(node ,v ,l ,r) (* v (* (prod l) (prod r)))]))

(prod 'leaf)
(prod '(node 8 leaf leaf))
(prod '(node 8 (node 2 leaf leaf) (node 4 leaf leaf)))
)

Now consider the work done in an example such as this:

@ex[
(prod '(node 9 (node 0 leaf leaf) (node 4 (node 2 leaf leaf) (node 3 leaf leaf))))
]

From a quick scan of the elements, we know the answer is 0 without
doing any arithmetic.  But the @racket[prod] function will do a bunch
of multiplication to actually figure this out.

To see, let's use a helper function to replace @racket[*] that prints
every it multiplies two numbers:

@#reader scribble/comment-reader
(ex
;; Number Number -> Number
(define (mult x y)
  (printf "mult: ~a x ~a\n" x y)
  (* x y))

;; BT -> Number
;; Multiply all the numbers in given binary tree
(define (prod bt)
  (match bt
    ['leaf 1]
    [`(node ,v ,l ,r) (mult v (mult (prod l) (prod r)))]))

(prod '(node 9 (node 0 leaf leaf) (node 4 (node 2 leaf leaf) (node 3 leaf leaf))))
)

This could potentially be bad if the tree were quite large.

How can we do better?  One option is to detect if the value at a node
is zero and simply avoid recurring on the left and right subtrees at
that point:

@#reader scribble/comment-reader
(ex
;; BT -> Number
;; Multiply all the numbers in given binary tree
(define (prod bt)
  (match bt
    ['leaf 1]
    [`(node ,v ,l ,r)
     (if (zero? v)
         0
	 (mult v (mult (prod l) (prod r))))]))
)

Does this help our answer?  Only slightly:

@ex[
(prod '(node 9 (node 0 leaf leaf) (node 4 (node 2 leaf leaf) (node 3 leaf leaf))))
]

Why?

The problem is that you may encounter the zero element deep within a
tree.  At that point you not only want to avoid doing the
multiplication of subtrees, but also of the elements surrounding the
zero.  But we seemingly don't have control over the context
surrounding the node with a zero in it, just the subtrees.  What can
we do?

One option, if the language provides it, is to @bold{raise an
exception}, signalling that a zero element has been found.  An outer
function can @bold{catch} that exception and produce zero.  Such a
program will avoid doing any multiplication in case there's a zero in
the tree.

Racket comes with an exception mechanism that uses @racket[raise] to
signal an exception, which is propagated to the nearest enclosing
exception handler.  If there is no such handler, an uncaught exception
error occurs.

@ex[

(eval:error (raise 5))
(eval:error (mult (raise 5) 2))
(eval:error (mult (raise (mult 5 3)) 2))

]

The general form of an exception handler uses the
@racket[with-handlers] form that includes a series of predicates and
handler expressions.  We'll consider a simpler form called
@racket[catch] that unconditionally catches any exception throw and
handles it with a function that takes the raised value as an argument.
It can be expressed in terms of the more sophisticated
@racket[with-handlers] form:

@ex[
(define-syntax-rule (catch e f)
  (with-handlers ([(λ (x) #t) f]) e))

(catch (raise 5) (λ (x) x))
(catch (mult (raise 5) 2) (λ (x) x))
(catch (mult (raise (mult 5 3)) 2) (λ (x) x))
(catch (mult (mult 5 3) 2) (λ (x) x))
(catch (mult (mult 5 3) 2) (λ (x) (mult x x)))
(catch (mult (raise (mult 5 3)) 2) (λ (x) (mult x x)))
]

Now we can solve our problem:

@#reader scribble/comment-reader
(ex
;; BT -> Number
;; Multiply all the numbers in given binary tree
(define (prod bt)
  (catch (prod/r bt) (λ (x) 0)))

;; BT -> Number
;; Throws: 0
(define (prod/r bt)
  (match bt
    ['leaf 1]
    [`(node ,v ,l ,r)
     (if (zero? v)
         (raise 0)
	 (mult v (mult (prod/r l) (prod/r r))))]))

(prod '(node 9 (node 0 leaf leaf) (node 4 (node 2 leaf leaf) (node 3 leaf leaf))))
)

(This code is a bit problematic for reasons that are beside the point
of this section, but... the problem is this will catch any exception,
including things like system signals, out of memory exceptions, etc.
A better solution would have the handler check that the exception
value was 0 and re-raise it if not.  That way it doesn't ``mask'' any
other exceptions.)

This code works great for our purposes, but what if the language
didn't provide an exception handling mechanism?  Could we achieve the
same effect without relying on exceptions?

One solution is to re-write the program in what's called
@bold{continuation passing style} (CPS). Continuation passing style
makes explicit what is implicit in the recursive calls to
@racket[prod] in our original program, which is that after recursively
computing the product of the subtree, we have to do more work such as
another recursive call and multiplication.  By making this work
explicit, we gain control over it and have the option to do things
like throw away this work.

Here is the basic idea.  We will write a version of @racket[prod] that
takes an additional argument which represents ``the work to be done
after this function call completes.'' It will take a single argument,
a number, which is the result of this function call, and it will
produce some final result for the computation (in this case, a number).

In general, we want @racket[(k (prod bt))] ≡ @racket[(prod/k bt k)]
for all functions @racket[k] and binary trees @racket[bt].

Starting from the spec, we have:

@#reader scribble/comment-reader
(ex
;; BT (Number -> Number) -> Number
(define (prod/k bt k)
  (k (prod bt)))
)

We can unroll the definition of @racket[prod]:

@#reader scribble/comment-reader
(ex
(define (prod/k bt k)
  (match bt
    ['leaf (k 1)]
    [`(node ,v ,l ,r)
     (k (mult v (mult (prod l) (prod r))))]))
)

Now we'd like to replace the calls to @racket[prod] with calls to
@racket[prod/k], which we can do by recognizing the work to be done
around the call to @racket[prod] and placing it in the
@bold{continuation} argument to @racket[prod/k].  Let's do the first call:

@#reader scribble/comment-reader
(ex
(define (prod/k bt k)
  (match bt
    ['leaf (k 1)]
    [`(node ,v ,l ,r)
     (prod/k l (λ (pl)
                 (k (mult v (mult pl (prod r))))))]))
)

Doing this again, we get:

@#reader scribble/comment-reader
(ex
(define (prod/k bt k)
  (match bt
    ['leaf (k 1)]
    [`(node ,v ,l ,r)
     (prod/k l (λ (pl)
                 (prod/k r (λ (pr)
                             (k (mult v (mult pl pr)))))))]))
)

Now we have a definition of @racket[prod/k] that is independent of
@racket[prod] that satisfies the spec we started with.

A couple of things to note:

@itemlist[
@item{Every call to @racket[prod/k] is a tail-call,}
@item{The context of the recursive calls are given explicitly as continuation arguments.}
]

We can recreate the original function by giving the appropriate initial continuation:

@#reader scribble/comment-reader
(ex
;; BT -> Number
(define (prod bt)
  (prod/k bt (λ (x) x)))
)

Now, this code doesn't do anything smart on zero elements; it does
exactly the same multiplications our first program does:

@ex[
(prod '(node 9 (node 0 leaf leaf) (node 4 (node 2 leaf leaf) (node 3 leaf leaf))))
]

However, with a small tweak, we can get the behavior of the exception-handling code.

Consider this definition:

@#reader scribble/comment-reader
(ex
;; BT (Number -> Number) -> Number
(define (prod/k bt k)
  (match bt
    ['leaf (k 1)]
    [`(node ,v ,l ,r)
     (if (zero? v)
         0
         (prod/k l (λ (pl)
                    (prod/k r (λ (pr)
                                (k (mult v (mult pl pr))))))))]))

;; BT -> Number
(define (prod bt)
  (prod/k bt (λ (x) x)))
)

Notice that this program, when the value in a node is zero,
immediately returns @racket[0].  It does not do any of the work
represented by @racket[k].  It does something akin to raising an
exception: it blows off all the work of the surround context and
returns a value to the ``handler'' (in this case, @racket[prod]).

Returning to our example, we can see that no multiplications occur:

@ex[
(prod '(node 9 (node 0 leaf leaf) (node 4 (node 2 leaf leaf) (node 3 leaf leaf))))
]

We've now achieved our original goal without the use of exception
handlers.  We achieved this by rewriting our program to make explicit
the work that remains to do, giving us the ability to avoid doing it
when necessary.  This is a slighly simplified version of the general
exception handling transformation, which we will look at next, since
there's only a single handler and all it does it produce 0.  But, the
by-hand transformation we did provides a useful blueprint for how can
generally transform programs that use exception handling into ones
that don't.


@section[#:tag-prefix "mug"]{Exceptional transformation}

Let's consider a very small subset of expressions, extended with
@racket[raise] and @racket[catch], and see how we can transform away
those added mechanisms:

@#reader scribble/comment-reader
(racketblock
;; An Expr is one of:
;; - Integer
;; - Variable
;; - `(if ,Expr ,Expr ,Expr)
;; - `(,Prim1 ,Expr)
;; - `(,Prim2 ,Expr ,Expr)
;; - `(raise ,Expr)
;; - `(catch ,Expr (λ (,Variable) ,Expr))
)

Here is the basic idea of the transformation, we transform every
expression into a function of two arguments.  The two arguments
represent the two ways an expression may produce results: either by
returning normally or by raising an exception.


So for example, if the original expression were @racket[1], we'd want
the transformed program to be
@racketblock[
'(λ (retn raze) (retn 1))
]

Why?  Because @racket[1] just produces @racket[1]; it can't possibly
raise an exception.  So given the two ways of producing a value, we
choose the @racket[ret] way and ``return'' by apply @racket[retn] to
the value we want to return: @racket[1].


Suppose the original expression is @racket[(raise 1)].  Then we want
to produce:
@racketblock[
'(λ (retn raze) (raze 1))
]

This is choosing to not return a value, but rather ``raise'' an
exception by calling the @racket[raze] function.

This is a lot like the by-hand transformation we did, except we now
have two continuations: one to represent work to do after
returning (normally) and one for work to do after raising an
exception.

At the top-level, to run an expression we simply plug in appropriate
definitions for @racket[retn] and @racket[raze].  The @racket[retn]
function should just produce the result, i.e. it should be @racket[(λ
(x) x)], while @racket[raze] should signal an uncaught exception.
Since our language has such a simple model of errors, we'll just cause
an error to occur, i.e. @racket[(λ (x) (add1 #f))].  Let's try our
examples.

@ex[
(interp-env '((λ (retn raze) (retn 1)) (λ (x) x) (λ (x) (add1 #f))) '())
(interp-env '((λ (retn raze) (raze 1)) (λ (x) x) (λ (x) (add1 #f))) '())
]

What about something like @racket[(add1 _e)]?

Well if @racket[_e] returns normally, then the whole thing should
produce one more than that value.  If @racket[_e] raises an exception,
then @racket[(add1 _e)] should raise that exception.

Suppose @racket[_t] where the transformed version of @racket[_e],
which means it is a function of two parameters: what to do if
@racket[_e] returns and what to do if @racket[_e] raises.

Then the transformation of @racket[(add1 _e)] is
@racketblock[
(λ (retn raze)
  (_t (λ (x) (retn (add1 x))) (λ (x) (raze x))))]

This can be simplified slightly by observing that @racket[(λ (x) (raze
x))] is equal to @racket[raze]:
@racketblock[
(λ (retn raze)
  (_t (λ (x) (retn (add1 x))) raze))]

How about something like @racket[(catch _e0 (λ (_x) _e1))]?  If
@racket[_e0] produces a value normally, then the whole expression
produces that value normally.  However if @racket[_e0] raises an
expression then the whole expression produces whatever @racket[_e1]
with @racket[x] bound to the raised value produces.  Let @racket[_t0]
and @racket[_t1] be the transformed versions of @racket[_e0] and
@racket[_e1].  Then transformation of the whole expressions should be

@racketblock[
(λ (retn raze)
  (_t0 retn (λ (_x) (_t1 retn raze))))
]

One thing to notice here is that we are running @racket[_t0] with a @racket[raze] function
that, if called, will run @racket[_t1] normally.

Guided by the examples, let's define the transformation (note: we have
to take care of avoiding unintended variable capture):

@#reader scribble/comment-reader
(ex
;; Expr -> Expr
(define (exn-transform e)
  (match e
    [(? integer? i) `(λ (retn raze) (retn ,i))]
    [(? symbol? x)
     (let ((retn (gensym 'retn))
           (raze (gensym 'raze)))
       `(λ (,retn ,raze) (,retn ,x)))]
    [`(if ,e0 ,e1 ,e2)
     (let ((t0 (exn-transform e0))
           (t1 (exn-transform e1))
           (t2 (exn-transform e2))
           (retn (gensym 'retn))
           (raze (gensym 'raze)))                  
       `(λ (,retn ,raze)
          (,t0
           (λ (x)
             ((if x ,t1 ,t2) ,retn ,raze))
           ,raze)))]
    [`(,(? prim? p) ,e0)
     (let ((t0 (exn-transform e0))
           (retn (gensym 'retn))
           (raze (gensym 'raze)))                             
       `(λ (,retn ,raze)
          (,t0 (λ (v) (,retn (,p v)))
               ,raze)))]
    [`(,(? prim? p) ,e0 ,e1)
     (let ((t0 (exn-transform e0))
           (t1 (exn-transform e1))
           (retn (gensym 'retn))
           (raze (gensym 'raze))
           (v0 (gensym 'v0)))
       `(λ (,retn ,raze)
          (,t0 (λ (,v0)
                 (,t1 (λ (v1) (,retn (,p v0 v1)))
                      ,raze))
               ,raze)))]
    [`(raise ,e)
     (let ((t (exn-transform e))
           (retn (gensym 'retn))
           (raze (gensym 'raze)))       
       `(λ (,retn ,raze)
          (,t ,raze ,raze)))]
    [`(catch ,e0 (λ (,x) ,e1))
     (let ((t0 (exn-transform e0))
           (t1 (exn-transform e1))
           (retn (gensym 'retn))
           (raze (gensym 'raze)))                 
       
       `(λ (,retn ,raze)
          (,t0 ,retn
               (λ (,x)
                 (,t1 ,retn ,raze)))))]))
)

Here's what the transformation looks like on examples:

@ex[
(exn-transform '1)
(exn-transform '(raise 1))
(exn-transform '(catch (raise 1) (λ (x) x)))
(exn-transform '(catch (raise 1) (λ (x) (add1 x))))
(exn-transform '(catch (add1 (raise 1)) (λ (x) 1)))
(exn-transform '(catch (add1 (raise 1)) (λ (x) (raise x))))
]

Now let's give it a spin:

@ex[
;; Expr -> Value
(define (run e)
  (interp-env `(,(exn-transform e) (λ (x) x) (λ (x) (add1 #f))) '()))

(run '1)
(run '(raise 1))
(run '(catch (raise 1) (λ (x) x)))
(run '(catch (raise 1) (λ (x) (add1 x))))
(run '(catch (add1 (raise 1)) (λ (x) 1)))
(run '(catch (add1 (raise 1)) (λ (x) (raise x))))
(run '(if (raise 0) 1 2))
(run '(if (zero? 0) (raise 1) 2))
]

@section[#:tag-prefix "mug"]{Quotation}

We have seen how to interpret limited uses of @racket[quote], such as
in @racket[(quote ())] and @racket[(quote x)], i.e. the empty list and symbols.

But we've also been using @emph{using} @racket[quote] more generally
where we can write down an arbitrary constant s-expression within a
@racket[quote]:

@ex[
'#t
'5
'(1 2 3)
'(add1 x)
'(car '(1 2 3))
'(((1) 2) 3)
'(1 . 2)
'("asdf" fred ((one)))
]

We can understand the more general @racket[quote] form as a shorthand
for expressions that construct an equivalent list to the one denoted
by the s-expression.

For example,
@itemlist[

@item{@racket['1] is shorthand for @racket[1],}

@item{@racket['(1 . 2)] is shorthand for @racket[(cons '1 '2)], which
is shorthand for @racket[(cons 1 2)],}

@item{@racket['(1 2 3)] is shorthand for @racket[(cons '1 '(2 3))],
which is shorthand for @racket[(cons 1 (cons '2 '(3)))], which is
shorthand for @racket[(cons 1 (cons 2 (cons '3 '())))], which is
shorthand for @racket[(cons 1 (cons 2 (cons 3 '())))],}

@item{@racket['()] is as simple as possible (the empty list),}

@item{@racket['x] is as simple as possible (a symbol), and}

@item{@racket[5] is as simple as possible.}
]

Guided by these examples, we can write a function that transforms the
s-expression inside of a @racket[quote] into an equivalent expression
that only uses @racket[quote] for constructing symbols and the empty
list:


@#reader scribble/comment-reader
(ex
;; S-Expr -> Expr
;; Produce an expression that evaluates to given s-expression, without
;; use of quote (except for symbols and empty list)
(define (quote->expr d)
  (match d
    [(? boolean?) d]
    [(? integer?) d]
    [(? string?) d]
    [(? char?) d]
    [(? symbol?) (list 'quote d)]
    [(cons x y) (list 'cons (quote->expr x) (quote->expr y))]
    ['() ''()]))


(quote->expr 'x)
(quote->expr 5)
(quote->expr "Fred")
(quote->expr '(1 . 2))
(quote->expr '(1 2 3))
(quote->expr '(car '(1 2 3)))
(quote->expr '(((1) 2) 3))
(quote->expr '(1 . 2))
(quote->expr '("asdf" fred ((one))))
)

We can now incorporate this into @racket[desugar] to eliminate uses of
compound-data @racket[quote]:

@#reader scribble/comment-reader
(ex
;; Expr+ -> Expr
(define (desugar e+)
  (match e+
    [`(begin ,@(list `(define (,fs . ,xss) ,es) ...) ,e)
     `(letrec ,(map (λ (f xs e) `(,f (λ ,xs ,(desugar e)))) fs xss es)
        ,(desugar e))]    
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`',(? symbol? s)      `',s]
    [`',d                  (quote->expr d)]
    [`(,(? prim? p) . ,es) `(,p ,@(map desugar es))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(desugar e0) ,(desugar e1) ,(desugar e2))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(desugar e0))) ,(desugar e1))]
    [`(letrec ,bs ,e0)
     `(letrec ,(map (λ (b) (list (first b) (desugar (second b)))) bs)
        ,(desugar e0))]
    [`(λ ,xs ,e0)          `(λ ,xs ,(desugar e0))]
    [`(cond . ,_)          (desugar (cond->if e+))]
    [`(and . ,_)           (desugar (and->if e+))]
    [`(or . ,_)            (desugar (or->if e+))]
    [`(,e . ,es)           `(,(desugar e) ,@(map desugar es))]))
)

And now we can @racket[desugar] programs such as these:

@ex[
(desugar '(map f '(1 2 3)))
(desugar '(map f '(and 1 2)))
(desugar '(if x '(1 . 2) 3))
]

And our prior interpterter will work just fine on these programs:

@ex[
(interp-env (desugar '(map f '(1 2 3))) `((map ,map) (f ,add1)))
(interp-env (desugar '(map f '(and 1 2))) `((map ,map) (f ,identity)))
(interp-env (desugar '(if x '(1 . 2) 3)) `((x #t)))
]

And:

@ex[
(interp-env (desugar ''(((1) 2) 3)) '())
(interp-env (desugar ''(1 . 2)) '())
(interp-env (desugar ''("asdf" fred ((one)))) '())
]

@section[#:tag-prefix "mug"]{Pattern matching}

One of the most ubiquitous language features we've used, but not
implemented, is pattern matching with the @racket[match] form.

Pattern matching too can be seen as syntactic sugar since it's easy to
imagine how you could rewrite uses of @racket[match] into equivalent
expressions that didn't involve @racket[match].

For example, consider the program:

@#reader scribble/comment-reader
(racketblock
;; BT -> Number
;; Multiply all the numbers in given binary tree
(define (prod bt)
  (match bt
    ['leaf 1]
    [`(node ,v ,l ,r) (* v (* (prod l) (prod r)))]))
)

An alternative, equivalent, formulation that doesn't use
@racket[match] is the following:

@#reader scribble/comment-reader
(racketblock
;; BT -> Number
;; Multiply all the numbers in given binary tree
(define (prod bt)
  (cond
    [(eq? 'leaf bt) 1]
    [(and (list? bt)
          (= 4 (length bt))
          (eq? 'node (first bt)))
     (let ((v (second bt))
           (l (third bt))
           (r (fourth bt)))
       (* v (* (prod l) (prod r))))]
    ; corresponds to a match failure
    [else (add1 #f)]))
)

This code is less nice to read and write, but essentially does the
same thing the pattern-matching code does.

In this example, each @racket[match]-clause becomes a
@racket[cond]-clause.  The question-part of each @racket[cond]-clause
is an expression that determines whether the corresponding
pattern-part of the @racket[match-clause] matches.  The answer-part of
each @racket[cond]-clause corresponds to the expression-part of the
@racket[match]-clause, with an add @racket[let]-form that destructures
the scrutinee and binds the pattern variables of the pattern-part.


Let's consider the following extension to the grammar of @tt{Expr+} to
include a simplified version of the pattern matchin form we've been
using:

@#reader scribble/comment-reader
(racketblock
;; type Expr+ =
;; ....
;; | Match

;; type Match = (match ,Expr+ ,(list Pat Expr+) ...)

;; type Pat =
;; | #t
;; | #f
;; | Integer
;; | String
;; | Variable
;; | `_
;; | `'()
;; | `(quote ,Symbol)
;; | `(cons ,Pat ,Pat)
;; | `(list ,Pat ...)
;; | `(? ,Expr ,Pat ...)
 )

A @racket[match] form consists of an expression to match against,
sometimes callsed the @bold{scrutinee}, followed by some number of
pattern-matching clauses; each one consists of a pattern and
expression to evaluate should the pattern match the scrutinee's value.

Here a pattern can either be a literal boolean, integer, string, empty
list, or symbol, or a pattern variable, which matches anything and
binds the value to the variable, a ``wildcard'' which matches anything
and binds nothing, a @racket[cons] pattern which matches pairs of
things that match the subpatterns, @racket[list] pattern which matches
lists of a fixed-size where elements matches the subpatterns, or a
@racket[?] pattern which matches if the results of evaluated the first
subexpression applied to scrutinee produces true and all of the
subpatterns match.

This doesn't include the @racket[quasiquote]-patterns we used above,
but still this is a useful subset of pattern matching and allows us to
write programs such as:

@#reader scribble/comment-reader
(racketblock
;; BT -> Number
;; Multiply all the numbers in given binary tree
(define (prod bt)
  (match bt
    ['leaf 1]
    [(list 'node v l r) (* v (* (prod l) (prod r)))]))
)

As alluded to above, each pattern plays two roles: it used to
determine whether the scrutinee matches the pattern, and it used to
bind variable names (in the scope of the RHS expression) to sub-parts
of the scrutinee when it does match.

We can write two helper functions to accomplish each of these tasks:
@itemlist[

@item{rewrite patterns into Boolean valued expressions that answer
whether the pattern matches the scrutinee,}

@item{rewrite pattern and RHS in to expressions in which the pattern
variables of pattern are bound to the appropriately deconstructed
parts of the scrutinee.}

]


Assume: the scrutinee is a variable. (It's easy to establish this assumption in general.)

We need two functions:

@#reader scribble/comment-reader
(racketblock
;; Pat Variable -> Expr
;; Produces an expression determining if p matches v
(define (pat-match p v) ...)

;; Pat Variable Expr -> Expr
;; Produce an expression that deconstructs v and binds pattern variables
;; of p in scope of e.
;; ASSUME: v matches p
(define (pat-bind p v e) ...)
)

Let's turn to @racket[pat-match] first.

Suppose the pattern is a literal @racket[#t].  When does @racket[v]
match it?  When @racket[v] is @racket[eq?] to @racket[#t].

So an expression that produces true when this pattern matches is @racket[(eq? #t v)].

Handling @racket[#f], integers, characters, symbols, and the empty list is similar.

What about variables?  Suppose the pattern is @racket[x].  When does
@racket[v] match it?  Always.  So @racket[#t] is an expression that
produces true with this pattern matches.

Wildcards are the same.

What about when the pattern is a @racket[cons]-pattern?  Suppose the
pattern is @racket[(cons _p1 _p2)] for some patterns @racket[_p1] and
@racket[_p2].  When does @racket[v] match @racket[(cons _p1 _p2)]?
When @racket[v] is a pair and @racket[(car v)] matches @racket[_p1]
and @racket[(cdr v)] matches @racket[_p2].

A @racket[list] pattern is similar, except that the scrunity must be a
list with as many elements as there are patterns, and the elements
must match the corresponding subpattern.

What about a @racket[?] pattern?  When does @racket[v] match it?
Suppose the pattern is @racket[(? even?)].  When does @racket[v] match
it?  When @racket[(even? v)] is true.  If the pattern had a non-empty
list of sub-patterns they would all need to match @racket[v], too.


We can now formulate a defintion of @racket[pat-match]:

@#reader scribble/comment-reader
(racketblock
;; Pat Variable -> Expr
;; Produces an expression determining if p matches v
(define (pat-match p v)
  (match p
    [#t `(eq? #t ,v)]
    [#f `(eq? #f ,v)]
    [(? integer? i) `(eq? ,i ,v)]
    [(? string? s)
     `(and (string? ,v)
           (string=? ,s ,v))]
    [(list 'quote '()) `(eq? '() ,v)]    
    [(? symbol?) #t]
    [(list 'quote (? symbol? s)) `(eq? ,v ',s)]
    [(list 'cons p1 p2)
     (let ((v1 (gensym))
           (v2 (gensym)))
       `(and (cons? ,v)
             (let ((,v1 (car ,v))
                   (,v2 (cdr ,v)))             
               (and ,(pat-match p1 v1)
                    ,(pat-match p2 v2)))))]
    [(cons 'list ps)
     `(and (list? ,v)
           (= (length ,v) ,(length ps))
           ,(pat-match-list ps v))]
    [(cons '? (cons e ps))
     `(and (,e ,v)
           ,(pats-match ps v))]))
)

The @racket[list]-pattern case relies on a helper function
@racket[pat-match-list] and the @racket[?]-pattern case relies on
@racket[pats-match], both defined below:


@#reader scribble/comment-reader
(racketblock
;; (Listof Pat) Variable -> Expr
;; Produces an expression determining if every ps matches x
(define (pats-match ps v)
  (match ps
    ['() #t]
    [(cons p ps)
     `(and ,(pat-match p v)
           ,(pats-match ps v))]))

;; (Listof Pat) Variable -> Expr
;; Produces an expression determining if each ps matches each element of list v
(define (pat-match-list ps v)
  (match ps
    ['() #t]
    [(cons p ps)
     (let ((v1 (gensym))
           (v2 (gensym)))
       `(let ((,v1 (car ,v))
              (,v2 (cdr ,v)))
          (and ,(pat-match p v1)
               ,(pat-match-list ps v2))))]))
)

Here are some examples:

@ex[

(pat-match 'leaf 'bt)
(pat-match '(list 'node v l r) 'bt)
(pat-match '(list 'node (? even? v) l r) 'bt)

]

These aren't very readable programs that emerge, however, we check
that they're doing the right thing.  Note that the elaboration
requires a few functions to be available, such as @racket[list?],
and @racket[length].  We make these available in an initial
environment:

@ex[
(define env0
  `((length ,length)
    (list? ,list?)))
(interp-env (desugar `(let ((bt 'leaf)) ,(pat-match 'leaf 'bt))) env0)
(interp-env (desugar `(let ((bt 'leaf)) ,(pat-match 8 'bt))) env0)
(interp-env (desugar
             `(let ((bt '(node 1 leaf leaf)))
               ,(pat-match '(list 'node v l r) 'bt)))
            env0)
(interp-env (desugar
             `(let ((bt '(node 1 leaf leaf)))
               ,(pat-match '(list 'node (? zero?) l r) 'bt)))
            env0)
(interp-env (desugar
             `(let ((bt '(node 0 leaf leaf)))
               ,(pat-match '(list 'node (? zero?) l r) 'bt)))
            env0)
]

Now moving on to @racket[pat-bind], it follows a similar structure,
but we always assume the given pattern matches the scrutinee.  The
addition @tt{Expr} argument represents the right-hand-side expression
of the @racket[match]-clause.  The idea is that the pattern variables
of @racket[p] are bound to sub-parts of @racket[v] in @racket[e].

@#reader scribble/comment-reader
(racketblock
;; Pat Variable Expr -> Expr
;; Produce an expression that deconstructs v and binds pattern variables
;; of p in scope of e.
;; ASSUME: v matches p
(define (pat-bind p v e)
  (match p
    [#t e]
    [#f e]
    [(? integer?) e]
    [(? string?) e]
    [(list 'quote '()) e]
    ['_ e]
    [(? symbol? x) `(let ((,x ,v)) ,e)]
    [(list 'quote (? symbol?)) e]
    [(list 'cons p1 p2)
     (let ((v1 (gensym))
           (v2 (gensym)))
       `(let ((,v1 (car ,v))
              (,v2 (cdr ,v)))
          ,(pat-bind p1 v1
                     (pat-bind p2 v2 e))))]
    [(cons 'list ps)
     (pat-bind-list ps v e)]
    [(cons '? (cons _ ps))
     (pats-bind ps v e)]))
)

Here are some examples:

@ex[

(pat-bind 'leaf 'bt 'bt)
(pat-bind '(list 'node v l r) 'bt 'v)
(pat-bind '(list 'node (? even? v) l r) 'bt 'v)

]

These are tough to read, but we can confirm what they compute:

@ex[
(interp-env (desugar
             `(let ((bt '(node 0 leaf leaf)))
                ,(pat-bind '(list 'node (? zero? z) l r) 'bt 'z)))
            '())
]

Putting the pieces together, we can now write a @racket[match->cond] function
that rewrites a @racket[match]-expression into a @racket[cond]-expression:

@#reader scribble/comment-reader
(racketblock
;; Match -> Expr
;; Rewrite match expression into an equivalent cond expression
(define (match->cond m)
  (match m
    [(cons 'match (cons e mcs))
     (let ((x (gensym)))
       `(let ((,x ,e))
          (cond ,@(map (λ (mc)
                         (match mc
                           [(list p e)
                            (list (pat-match p x) (pat-bind p x e))]))
                       mcs)
                ;; fall through to error
                [else (add1 #f)])))]))
)

@ex[
(match->cond '(match '(node 2 leaf leaf)
                ['leaf 0]
                [(list 'node v l r) v]))
]

Finally, we can incorporate @racket[match->cond] into @racket[desugar]:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Expr+ -> Expr
(define (desugar e+)
  (match e+
    [`(begin ,@(list `(define (,fs . ,xss) ,es) ...) ,e)
     `(letrec ,(map (λ (f xs e) `(,f (λ ,xs ,(desugar e)))) fs xss es)
        ,(desugar e))]
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`',(? symbol? s)      `',s]
    [`',d                  (quote->expr d)]
    [`(,(? prim? p) . ,es) `(,p ,@(map desugar es))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(desugar e0) ,(desugar e1) ,(desugar e2))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(desugar e0))) ,(desugar e1))]
    [`(letrec ,bs ,e0)
     `(letrec ,(map (λ (b) (list (first b) (desugar (second b)))) bs)
        ,(desugar e0))]
    [`(λ ,xs ,e0)          `(λ ,xs ,(desugar e0))]
    [`(cond . ,_)          (desugar (cond->if e+))]
    [`(and . ,_)           (desugar (and->if e+))]
    [`(or . ,_)            (desugar (or->if e+))]
    [`(match . ,_)         (desugar (match->cond e+))]  ; new
    [`(,e . ,es)           `(,(desugar e) ,@(map desugar es))]))
)

Now we can interpret programs such as this:

@ex[

(interp-env
 (desugar
  '(begin (define (prod bt)
            (match bt
              ['leaf 1]
              [(list 'node v l r)
               (* v (* (prod l) (prod r)))]))
          
          (prod '(node 3 (node 4 leaf leaf) leaf))))
 `((* ,*) (list? ,list?) (length ,length)))

]
         
                          
