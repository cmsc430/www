#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  (except-in "fraud/semantics.rkt" ext lookup)
	  (prefix-in sem: (only-in "fraud/semantics.rkt" ext lookup))
          #;(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "fraud" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title{Fraud: local binding and variables}

@;defmodule[(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")]
@;declare-exporting[(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")]
@;defidform/inline[interp]

Let's now consider add a notion of @bold{local binding} to our target
language.


We'll call it @bold{Fraud}.

We will use the following syntax to bind local variables:

@verbatim{
(let ((@math{id_0} @math{e_0}))
  @math{e})
}

This form binds the identifier @math{i_0} to value of @math{e_0}
within the scope of @math{e}.

This is a specialization of Racket's own local binding form, which
allows for any number of bindings to be made with @racket[let]:

@verbatim{
(let ((@math{id_0} @math{e_0}) ...)
  @math{e})
}

We adopt this specialization of Racket's let syntax so that you can
always take a Fraud program and run it in Racket to confirm what it
should produce.

Adding a notion of variable binding also means we need to add
variables to the syntax of expressions.

Together this leads to the following grammar for Fraud:

@centered{@render-language[D-pre]}

Which can be modeled with the following data type definition:

@codeblock-include["fraud/ast.rkt"]

We will also need a predicate for well-formed Fraud expressions, but
let's return to this after considering the semantics and interpreter.

@section{Meaning of Fraud programs}

@;(declare-exporting ,`(file ,(path->string (build-path notes "fraud/interp.rkt"))))



The meaning of Fraud programs depends on the form of the expression and
in the case of integers, increments, and decrements, the meaning is
the same as in the prior languages.

The two new forms are let-expressions and variables.

@itemlist[

@item{the meaning of a let expression @tt{(let ((@math{x} @math{e_0}))
@math{e})} is the meaning of @math{e} (the @bold{body} of the let)
when @math{x} means the value of @math{e_0} (the @bold{right hand
side} of the let),}

@item{the meaning of a variable @math{x} depends on the context in
which it is bound.  It means the value of the right-hand side of the
nearest enclosing let expression that binds @math{x}.  If there is no
such enclosing let expression, the variable is meaningless.}

]

Let's consider some examples:

@itemlist[

@item{@tt{x}: this expression is meaningless on its own.}

@item{@tt{(let ((x 7)) x)}: this means 7, since the body
expression, @tt{x}, means 7 because the nearest enclosing binding for
@tt{x} is to @tt{7}, which means 7.}

@item{@tt{(let ((x 7)) 2)}: this means @tt{2} since the body
expression, @tt{2}, means 2.}

@item{@tt{(let ((x 7)) (add1 x))}: this means 8 since the body
expression, @tt{(add1 x)}, means one more than @tt{x} and @tt{x} means
7 because the nearest enclosing binding for @tt{x} is to @tt{7}.}

@item{@tt{(let ((x (add1 7))) x)}: this means 8 since the body
expression, @tt{x}, means 8 because the nearest enclosing binding for
@tt{x} is to @tt{(add1 7)} which means 8.}

@item{@tt{(let ((x 7)) (let ((y 2)) x))}: this means 7 since the body
expression, @tt{(let ((y 2)) x)}, means 2 since the body expression,
@tt{x}, means 7 since the nearest enclosing binding for @tt{x} is to
@tt{7}.}

@item{@tt{(let ((x 7)) (let ((x 2)) x))}: this means 2 since the body
expression, @tt{(let ((x 2)) x)}, means 2 since the body expression,
@tt{x}, means 7 since the nearest enclosing binding for @tt{x} is to
@tt{2}.}

@item{@tt{(let ((x (add1 x))) x)}: this is meaningless, since the
right-hand side expression, @tt{(add1 x)} is meaningless because
@tt{x} has no enclosing let that binds it.}

@item{@tt{(let ((x 7)) (let ((x (add1 x))) x))}: this means 8 because
the body expression @tt{(let ((x (add1 x))) x)} means 8 because the
body expression, @tt{x}, is bound to @tt{(add1 x)} is in the nearest
enclosing let expression that binds @tt{x} and @tt{(add1 x)} means 8
because it is one more than @tt{x} where @tt{x} is bound to @tt{7} in
the nearest enclosing let that binds it.}

]

Make sure you have a good understanding of how binding work in these
examples before moving on.  Remember: you can always check your
understanding by pasting expressions into Racket and seeing what it
produces.

One thing that should be clear from these examples is that the meaning
of a sub-expression is not determined by the form of that expression
alone.  For example, @tt{x} could mean 7, or it could mean 8, or it
could be meaningless, or it could mean 22, etc.  It depends on the
context in which it occurs.  So in formulating the meaning of an
expression, we will have to have take this context into account.

Thinking more about what information we need to keep track of reveals
that when considering the meaning of a let's body, we need to know
that the variable it's binding means the value of the right hand
expression.  Since a program potentially consists of nested let
expressions, we will need to keep track of some number of pairs of
variables and their meaning.  We will refer to this contextual
information as an @bold{environment}.

The meaning of a variable is resolved by looking up its meaning in the
environment.  The meaning of a let will depend on the meaning of its
body with an extended environment that associates its variable binding
to the value of the right hand side.

The heart of the semantics is an auxiliary relation, @render-term[D
𝑫𝒓], which relates an expression and an environement to the integer
the expression evaluates to (in the given environment):

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name i j)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)])
        (apply centered
	   (add-between 
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

@(show-judgment 𝑫𝒓 0 3)
@(show-judgment 𝑫𝒓 3 5)

It relies on two functions: one for extending an environment with a
variable binding and one for lookup up a variable binding in an
environment:

@centered{
@render-metafunction[sem:ext #:contract? #t]

@(with-atomic-rewriter
  'undefined
  "⊥"
  (render-metafunction sem:lookup #:contract? #t))}

The operational semantics for Fraud is then defined as a binary relation
@render-term[D 𝑫], which says that @math{(e,i)} in @render-term[D 𝑫],
only when @math{e} evaluates to @math{i} in the empty environment
according to @render-term[D 𝑫𝒓]:

@(show-judgment 𝑫 0 1)

The interpreter closely mirrors the semantics.  The top-level
@racket[interp] function relies on a helper function
@racket[interp-env] that takes an expression and environment and
computes the result.  It is defined by structural recursion on the
expression.  Environments are represented as lists of associations
between variables and integers.  There are two helper functions for
@racket[ext] and @racket[lookup]:

@codeblock-include["fraud/interp.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
(eval:error (interp 'x))
(interp '(let ((x 7)) x))
(interp '(let ((x 7)) 2))
(interp '(let ((x 7)) (add1 x)))
(interp '(let ((x (add1 7))) x))
(interp '(let ((x 7)) (let ((y 2)) x)))
(interp '(let ((x 7)) (let ((x 2)) x)))
(eval:error (interp '(let ((x (add1 x))) x)))
(interp '(let ((x 7)) (let ((x (add1 x))) x)))
]

@bold{Interpreter Correctness}: @emph{For all Fraud expressions
@racket[e] and integers @racket[i], if (@racket[e],@racket[i]) in
@render-term[D 𝑫], then @racket[(interp e)] equals
@racket[i].}

@section{An Example of Fraud compilation}

Suppose we want to compile @racket['(let ((x 7)) (add1 x))].  There
are two new forms we need to compile: the @racket['(let ((x ...))
...)] part and the @racket['x] part in the body.

We already know how to compile the @racket['(add1 ...)] part and the
@racket[7] part.

What needs to happen?  Compiling the @racket[7] part will emit
instructions that, when run, leave @racket[7] in the @racket['rax]
register.  Compiling the @racket['(add1 ...)] part relies on the
result of evaluating it's subexpression to be in @racket['rax] when it
increments it.  So, compile the variable binding needs to stash the
@racket[7] somewhere and compiling the variable occurrence needs to
retrieve that stashed value.  After the let expression has been run,
the stashed value should go away since the variable is no longer in
scope.

This ``stashing'' of values follows a stack discipline.  When entering
a let, after the right-hand side has been run, the result should be
pushed.  When evaluating a variable occurrence, the bound value is on
the stack.  After exiting the let, the stack can be popped.

Suppose we want to compile @racket['(let ((x 7)) (let ((y 2)) (add1
x)))].  Using the intuition developed so far, we should push 7, push
8, and then run the body.  But notice that the value of @racket['x] is
no longer on the top of the stack; @racket['y] is.  So to retrieve the
value of @racket['x] we need jump past the @racket['y].  But
calculating these offsets is pretty straightforward.  In this example
there is one binding between the binding of @racket['x] and this
occurrence.  Since we push every time we enter a let and pop every
time we leave, the number of bindings between an occurrence and its
binder is exactly the offset from the top of the stack we need use.

@filebox-include-fake[codeblock "fraud/asm/ast.rkt"]{
#lang racket
;; type Arg =
;; ...
;; | `(offset ,Reg ,Integer)
 
;; type Reg =
;; ...
;; | `rsp
}

@codeblock-include["fraud/asm/printer.rkt"]
@codeblock-include["fraud/compile.rkt"]


@ex[
(eval:error (asm-display (compile 'x)))
(asm-display (compile '(let ((x 7)) x)))
(asm-display (compile '(let ((x 7)) 2)))
(asm-display (compile '(let ((x 7)) (add1 x))))
(asm-display (compile '(let ((x (add1 7))) x)))
(asm-display (compile '(let ((x 7)) (let ((y 2)) x))))
(asm-display (compile '(let ((x 7)) (let ((x 2)) x))))
(eval:error (asm-display (compile '(let ((x (add1 x))) x))))
(asm-display (compile '(let ((x 7)) (let ((x (add1 x))) x))))
]
