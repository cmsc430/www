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
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt" "ast.rkt" "syntax.rkt"))

@title[#:tag "Fraud"]{Fraud: local binding and variables}

@emph{To be is to be the value of a variable.}

@table-of-contents[]

@section{Variables}

@;defmodule[(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")]
@;declare-exporting[(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")]
@;defidform/inline[interp]

Let's now consider add a notion of @bold{local binding} to our target
language.


We'll call it @bold{Fraud}.

We will use the following syntax to bind local variables:

@racketblock[
(let ((_id0 _e0))
  _e)
]

This form binds the identifier @racket[_i0] to value of @racket[_e0]
within the scope of @racket[_e].

This is a specialization of Racket's own local binding form, which
allows for any number of bindings to be made with @racket[let]:

@racketblock[
(let ((_id0 _e0) ...)
  _e)
]

We adopt this specialization of Racket's @racket[let] syntax so that
you can always take a Fraud program and run it in Racket to confirm
what it should produce.

Adding a notion of variable binding also means we need to add
variables to the syntax of expressions.

Together this leads to the following grammar for Fraud:

@centered{@render-language[F-pre]}

Which can be modeled with the following data type definition:

@codeblock-include["fraud/ast.rkt"]

We will also need a predicate for well-formed Fraud expressions, but
let's return to this after considering the semantics and interpreter.

@section{Meaning of Fraud programs}

The meaning of Fraud programs depends on the form of the expression and
in the case of integers, increments, and decrements, the meaning is
the same as in the prior languages.

The two new forms are let-expressions and variables.

@itemlist[

@item{the meaning of a let expression @racket[(let ((_x _e0))
_e)] is the meaning of @racket[_e] (the @bold{body} of the @racket[let])
when @racket[_x] means the value of @racket[_e0] (the @bold{right hand
side} of the @racket[let]),}

@item{the meaning of a variable @racket[_x] depends on the context in
which it is bound.  It means the value of the right-hand side of the
nearest enclosing @racket[let] expression that binds @racket[_x].  If
there is no such enclosing @racket[let] expression, the variable is
meaningless.}

]

Let's consider some examples:

@itemlist[

@item{@racket[x]: this expression is meaningless on its own.}

@item{@racket[(let ((x 7)) x)]: this means @racket[7], since the body
expression, @racket[x], means @racket[7] because the nearest enclosing binding for
@racket[x] is to @racket[7], which means @racket[7].}

@item{@racket[(let ((x 7)) 2)]: this means @racket[2] since the body
expression, @racket[2], means @racket[2].}

@item{@racket[(let ((x 7)) (add1 x))]: this means @racket[8] since the
body expression, @racket[(add1 x)], means one more than @racket[x] and @racket[x]
means @racket[7] because the nearest enclosing binding for @racket[x] is to
@racket[7].}

@item{@racket[(let ((x (add1 7))) x)]: this means @racket[8] since the
body expression, @racket[x], means @racket[8] because the nearest
enclosing binding for @racket[x] is to @racket[(add1 7)] which means
@racket[8].}

@item{@racket[(let ((x 7)) (let ((y 2)) x))]: this means @racket[7] since the body
expression, @racket[(let ((y 2)) x)], means @racket[2] since the body expression,
@racket[x], means @racket[7] since the nearest enclosing binding for @racket[x] is to
@racket[7].}

@item{@racket[(let ((x 7)) (let ((x 2)) x))]: this means 2 since the
body expression, @racket[(let ((x 2)) x)], means @racket[2] since the
body expression, @racket[x], means @racket[7] since the nearest
enclosing binding for @racket[x] is to @racket[2].}

@item{@racket[(let ((x (add1 x))) x)]: this is meaningless, since the
right-hand side expression, @racket[(add1 x)] is meaningless because
@racket[x] has no enclosing @racket[let] that binds it.}

@item{@racket[(let ((x 7)) (let ((x (add1 x))) x))]: this means
@racket[8] because the body expression @racket[(let ((x (add1 x))) x)]
means @racket[8] because the body expression, @racket[x], is bound to
@racket[(add1 x)] is in the nearest enclosing @racket[let] expression
that binds @racket[x] and @racket[(add1 x)] means @racket[8] because
it is one more than @racket[x] where @racket[x] is bound to @racket[7]
in the nearest enclosing @racket[let] that binds it.}

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

The heart of the semantics is an auxiliary relation, @render-term[F
𝑭-𝒆𝒏𝒗], which relates an expression and an environement to the integer
the expression evaluates to (in the given environment):

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name cases)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)])
        (apply centered
	   (add-between 
             (map (λ (c) (parameterize ([judgment-form-cases (list c)]
	                                [judgment-form-show-rule-names #f])
	                   (render-judgment-form name)))
	          cases)
             (hspace 4))))))

The rules for dealing with the new forms (variables and lets) are:
@(show-judgment 𝑭-𝒆𝒏𝒗 '("var" "let"))

These rely on two functions: one for extending an environment with a
variable binding and one for lookup up a variable binding in an
environment:

@centered{
@render-metafunction[sem:ext #:contract? #t]

@(with-atomic-rewriter
  'undefined
  "⊥"
  (render-metafunction sem:lookup #:contract? #t))}


The remaining rules are just an adaptation of the existing rules from
Extort to thread the environment through.  For example, here are just
a couple:
@(show-judgment 𝑭-𝒆𝒏𝒗 '("prim"))
@(show-judgment 𝑭-𝒆𝒏𝒗 '("if-true" "if-false"))

And rules for propagating errors through let:
@(show-judgment 𝑭-𝒆𝒏𝒗 '("let-err"))



The operational semantics for Fraud is then defined as a binary relation
@render-term[F 𝑭], which says that @math{(e,i)} in @render-term[F 𝑭],
only when @math{e} evaluates to @math{i} in the empty environment
according to @render-term[F 𝑭-𝒆𝒏𝒗]:

@(show-judgment 𝑭 '("mt-env"))

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
(eval:error (interp (sexpr->ast 'x)))
(interp (sexpr->ast '(let ((x 7)) x)))
(interp (sexpr->ast '(let ((x 7)) 2)))
(interp (sexpr->ast '(let ((x 7)) (add1 x))))
(interp (sexpr->ast '(let ((x (add1 7))) x)))
(interp (sexpr->ast '(let ((x 7)) (let ((y 2)) x))))
(interp (sexpr->ast '(let ((x 7)) (let ((x 2)) x))))
(eval:error (interp (sexpr->ast '(let ((x (add1 x))) x))))
(interp (sexpr->ast '(let ((x 7)) (let ((x (add1 x))) x))))
]

@bold{Interpreter Correctness}: @emph{For all Fraud expressions
@racket[e] and values @racket[v], if (@racket[e],@racket[v]) in
@render-term[F 𝑭], then @racket[(interp e)] equals
@racket[v].}

@section{Lexical Addressing}

Just as we did with @seclink["Dupe"], the best way of understanding
the forthcoming compiler is to write a ``low-level'' interpreter that
explains some of the ideas used in the compiler without getting bogged
down in code generation details.

At first glance at @racket[interp], it would seem we will need to
generate code for implementing the @tt{REnv} data structure and its
associated operations: @racket[lookup] and @racket[ext].  @tt{REnv} is
an inductively defined data type, represented in the interpreter as a
list of lists.  Interpreting a variable involves recursively scanning
the environment until the appropriate binding is found.  This would
take some doing to accomplish in x86.

However...

It is worth noting some invariants about environments created during
the running of @racket[interp].  Consider some subexpression
@racket[_e] of the program.  What environment will be used whenever
@racket[_e] is interpreted?  Well, it will be a mapping of every
variable bound outside of @racket[_e].  It's not so easy to figure out
@emph{what} these variables will be bound to, but the skeleton of the
environment can be read off from the program structure.

For example:

@racketblock[
(let ((x ...))
  (let ((y ...))
    (let ((z ...))
      _e)))
]

The subexpression @racket[_e] will @emph{always} be evaluated in an
environment that looks like:
@racketblock[
'((z ...) (y ...) (x ...))
]

Moreover, every free occurrence of @racket[x] in @racket[_e] will
resolve to the value in the third element of the environment; every
free occurrence of @racket[y] in @racket[_e] will resolve to the
second element; etc.

This suggests that variable locations can be resolved
@emph{statically} using @bold{lexical addresses}.  The lexical address
of a variable is the number of @racket[let]-bindings between the
variable occurrence and the @racket[let] that binds it.

So for example:

@itemlist[

@item{@racket[(let ((x ...)) x)]: the occurrence of @racket[x] has a
lexical address of @racket[0]; there are no bindings between the
@racket[let] that binds @racket[x] and its occurrence.}

@item{@racket[(let ((x ...)) (let ((y ...)) x))]: the occurrence of
@racket[x] has a lexical address of @racket[1] since the
@racket[let]-binding of @racket[y] sits between the
@racket[let]-binding of @racket[x] and its occurrence.}

]

We can view a variable @emph{name} as just a placeholder for the
lexical address; it tells us which binder the variable comes from.

Using this idea, let's build an alternative interpreter that operates
over an intermediate form of expression that has no notion of
variables, but just lexical addresses:

@#reader scribble/comment-reader
(racketblock
;; type IExpr =
;; | Integer
;; | Boolean
;; | `(address ,Natural)
;; | `(add1 ,Expr)
;; | `(sub1 ,Expr)
;; | `(zero? ,Expr)
;; | `(if ,Expr ,Expr ,Expr)
;; | `(let ((_ ,Expr)) ,Expr)
)

Notice that variables have gone away, replaced by a @racket[`(address
,Natural)] form.  The @racket[let] binding form no longer binds a
variable name either.

The idea is that we will translate expression (@tt{Expr}) like:

@racketblock[
(let ((x ...)) x)]

into intermediate expressions (@tt{IExpr}) like:

@racketblock[
(let ((_ ...)) (address 0))
]

And:

@racketblock[
(let ((x ...)) (let ((y ...)) x))
]

into:

@racketblock[
(let ((_ ...)) (let ((_ ...)) (address 1)))
]


Similar to how @racket[interp] is defined in terms of a helper
function that takes an environment mapping variables to their value,
the @racket[translate] function will be defined in terms of a helper
function that takes an environment mapping variables to their lexical
address.

The lexical environment will just be a list of variable names.  The
address of a variable occurrence is count of variable names that occur
before it in the list.  When a variable is bound (via-@racket[let])
the list grows:

@codeblock-include["fraud/translate.rkt"]

Notice that @racket[translate] is a kind of mini-compiler that
compiles @tt{Expr}s to @tt{IExpr}s.  It's only job is to eliminate
variable names by replacing variable occurrences with their lexical
addresses.  It does a minor amount of syntax checking while it's at it
by raising a (compile-time) error in the case of unbound variables.

The interpreter for @tt{IExpr}s will still have an environment data
structure, however it will be simpler the association list we started
with.  The run-time environment will consist only of a list of values;
the lexical address of (what used to be a) variable indicates the
position in this list.  When a value is bound by a @racket[let], the
list grows:

@codeblock-include["fraud/interp-lexical.rkt"]

Try to convince yourself that the two version of @racket[interp]
compute the same function.


@section{An Example of Fraud compilation}

Suppose we want to compile @racket[(let ((x 7)) (add1 x))].  There
are two new forms we need to compile: the @racket[(let ((x ...))
...)] part and the @racket[x] part in the body.

We already know how to compile the @racket[(add1 ...)] part and the
@racket[7] part.

What needs to happen?  Compiling the @racket[7] part will emit
instructions that, when run, leave @racket[7] in the @racket['rax]
register.  Compiling the @racket[(add1 ...)] part relies on the
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

Suppose we want to compile @racket[(let ((x 7)) (let ((y 2)) (add1
x)))].  Using the intuition developed so far, we should push 7, push
8, and then run the body.  But notice that the value of @racket['x] is
no longer on the top of the stack; @racket[y] is.  So to retrieve the
value of @racket[x] we need jump past the @racket[y].  But calculating
these offsets is pretty straightforward.  In this example there is one
binding between the binding of @racket[x] and this occurrence.  Since
we push every time we enter a let and pop every time we leave, the
number of bindings between an occurrence and its binder is exactly the
offset from the top of the stack we need use.

@filebox-include-fake[codeblock "fraud/asm/ast.rkt"]{
#lang racket
;; type Arg =
;; ...
;; | `(offset ,Reg ,Integer)
 
;; type Reg =
;; ...
;; | `rsp
}

@;codeblock-include["fraud/asm/printer.rkt"]

@codeblock-include["fraud/compile.rkt"]


@ex[
(eval:error (asm-display (compile (sexpr->ast 'x))))
(asm-display (compile             (sexpr->ast '(let ((x 7)) x))))
(asm-display (compile             (sexpr->ast '(let ((x 7)) 2))))
(asm-display (compile             (sexpr->ast '(let ((x 7)) (add1 x)))))
(asm-display (compile             (sexpr->ast '(let ((x (add1 7))) x))))
(asm-display (compile             (sexpr->ast '(let ((x 7)) (let ((y 2)) x)))))
(asm-display (compile             (sexpr->ast '(let ((x 7)) (let ((x 2)) x)))))
(eval:error (asm-display (compile (sexpr->ast '(let ((x (add1 x))) x)))))
(asm-display (compile             (sexpr->ast '(let ((x 7)) (let ((x (add1 x))) x)))))
]

And running the examples:
@ex[
(eval:error (asm-interp (compile (sexpr->ast 'x))))
(asm-interp (compile             (sexpr->ast '(let ((x 7)) x))))
(asm-interp (compile             (sexpr->ast '(let ((x 7)) 2))))
(asm-interp (compile             (sexpr->ast '(let ((x 7)) (add1 x)))))
(asm-interp (compile             (sexpr->ast '(let ((x (add1 7))) x))))
(asm-interp (compile             (sexpr->ast '(let ((x 7)) (let ((y 2)) x)))))
(asm-interp (compile             (sexpr->ast '(let ((x 7)) (let ((x 2)) x)))))
(eval:error (asm-interp (compile (sexpr->ast '(let ((x (add1 x))) x)))))
(asm-interp (compile             (sexpr->ast '(let ((x 7)) (let ((x (add1 x))) x)))))
]
