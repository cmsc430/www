#lang scribble/manual

@(require (for-label (except-in racket ... compile)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  (except-in "../../langs/fraud/semantics.rkt" ext lookup)
          (prefix-in sem: (only-in "../../langs/fraud/semantics.rkt" ext lookup))
          #;(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "fraud" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt" "ast.rkt" "parse.rkt"))

@title[#:tag "Fraud"]{Fraud: local binding, variables, and binary operations}

@emph{To be is to be the value of a variable.}

@table-of-contents[]

@verbatim|{
TODO:
* Update the "fake" ASM AST defn given
* Note that semantics is for (pure) subset
* Update random testing for full language
* Smooth out combination with Grift
}|

@section{Binding, variables, and binary operations}

@;defmodule[(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")]
@;declare-exporting[(file "/Users/dvanhorn/git/cmsc430-www/www/notes/fraud/interp.rkt")]
@;defidform/inline[interp]

Let's now consider add a notion of @bold{local binding} and
the ability to use binary operations to our target language
a


We'll call it @bold{Fraud}.

First, let's deal with the issue of variables and variable bindings.

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

@filebox-include-fake[codeblock "fraud/ast.rkt"]{
#lang racket
;; type Expr =
;; ...
;; | (Let Id Expr Expr)
;; | (Var Id)
;; type Id  = Symbol
...
(struct Let (x e1 e2) #:prefab)
(struct Var (x) #:prefab)
}
We will also need a predicate for well-formed Fraud expressions, but
let's return to this after considering the semantics and interpreter.


Now for binary operations...


You may have noticed that up until this point, evaluating
compound expressions in our language always depend upon the
result of a single subexpression. For example,
@racket[(add1 _e)] depends upon the result of @racket[_e],
@racket[(zero? _e)] depends upon @racket[_e], and so on.
Even expressions that involve multiple subexpressions such
as @racket[(if _e0 _e1 _e2)] really only depends on
@racket[_e0] to determine which of @racket[_e1] or
@racket[_e2] to evaluate. And in the case of
@racket[(begin _e0 _e1)], first we determine the value of
@racket[_e0] and then @racket[_e1], but these values are
never combined in any way.


Let's now consider what happens when we have @bold{multiple
subexpressions} whose results must be combined in order to evaluate an
expression.  As an example, consider @racket[(+ _e0 _e1)].  We must
evaluate @emph{both} @racket[_e0] and @racket[_e1] and sum their
results.

What's new are the following @emph{binary} operations:

@racketblock[
(+ _e0 _e1)
(- _e0 _e1)
]

This leads to the following grammar for Grift:

@centered[(render-language G)]

We can model it as a datatype as usual:

@filebox-include-fake[codeblock "fraud/ast.rkt"]{
#lang racket
;; type Expr =
;; ...
;; | (Prim2 Op2 Expr Expr)
;; type Op2 = '+ | '-
...
(struct Prim2 (p e1 e2) #:prefab)
}


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

The meaning of Grift programs is pretty straightforward.  For
@racket[(+ _e0 _e1)], the meaning is the sum of the meanings of
@racket[_e0] and @racket[_e1], when they mean integers, otherwise the
meaning is an error.


The handling of primitives occurs in the following rule:

@(show-judgment 𝑮-𝒆𝒏𝒗 '("prim"))

It makes use of an auxiliary judgment for interpreting primitives:

@centered[
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (render-metafunction 𝑮-𝒑𝒓𝒊𝒎 #:contract? #t))]




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
(eval:error (interp (parse 'x)))
(interp (parse '(let ((x 7)) x)))
(interp (parse '(let ((x 7)) 2)))
(interp (parse '(let ((x 7)) (add1 x))))
(interp (parse '(let ((x (add1 7))) x)))
(interp (parse '(let ((x 7)) (let ((y 2)) x))))
(interp (parse '(let ((x 7)) (let ((x 2)) x))))
(eval:error (interp (parse '(let ((x (add1 x))) x))))
(interp (parse '(let ((x 7)) (let ((x (add1 x))) x))))
]

We can see that it works as expected:

@ex[
(interp (parse '(+ 3 4)))
(interp (parse '(+ 3 (+ 2 2))))
(interp (parse '(+ #f 8)))
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
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim Op Expr)
;; | (If Expr Expr Expr)
;; | (Let '_ Expr Expr)
;; | (Var Addr)
;; type Addr = Natural
;; type prim = 'add1 | 'sub1 | 'zero?
)

Notice that variables have gone away, replaced by a @racket[(Var
Natural)] form.  The @racket[let] binding form no longer binds a
variable name either.

The idea is that we will translate expression (@tt{Expr}) like:

@racketblock[
(Let "x" (Int 7) (Var x))]

into intermediate expressions (@tt{IExpr}) like:

@racketblock[
(Let '_ (Int 7) (Var 0))
]

And:

@racketblock[
(Let "x" (Int 7) (Let "y" (Int 9) (Var "x")))
]

into:

@racketblock[
(Let '_ (Int 7) (Let "y" (Int 9) (Var 1)))
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
(define (show e)
  (displayln (asm-string (compile (parse e)))))
(eval:error (show 'x))
(show '(let ((x 7)) x))
(show '(let ((x 7)) 2))
(show '(let ((x 7)) (add1 x)))
(show '(let ((x (add1 7))) x))
(show '(let ((x 7)) (let ((y 2)) x)))
(show '(let ((x 7)) (let ((x 2)) x)))
(eval:error (show '(let ((x (add1 x))) x)))
(show '(let ((x 7)) (let ((x (add1 x))) x)))
]

And running the examples:
@ex[
(define (tell e)
  (asm-interp (compile (parse e))))
(eval:error (tell 'x))
(tell '(let ((x 7)) x))
(tell '(let ((x 7)) 2))
(tell '(let ((x 7)) (add1 x)))
(tell '(let ((x (add1 7))) x))
(tell '(let ((x 7)) (let ((y 2)) x)))
(tell '(let ((x 7)) (let ((x 2)) x)))
(eval:error (tell '(let ((x (add1 x))) x)))
(tell '(let ((x 7)) (let ((x (add1 x))) x)))
]



Binary expressions are easy to deal with at the level of the semantics
and interpreter.  However things are more complicated at the level of
the compiler.

To see the problem consider blindly following the pattern we used (and
ignoring type errors for the moment):

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (append (compile-e e0 c)
          (compile-e e1 c)
          (list (Add 'rax _????))))
)

The problem here is that executing @racket[c0] places its result in
register @racket['rax], but then executing @racket[c1] places its
result in @racket['rax], overwriting the value of @racket[e0].

It may be tempting to use another register to stash away the result of
the first subexpression:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (append (compile-e e0 c)
          (list (Mov 'rbx 'rax))
          (compile-e e1 c)
          (list (Add 'rax 'rbx))))
)

Can you think of how this could go wrong?

To come up with a general solution to this problem, we need to save
the result of @racket[_e0] and then retrieve it after computing
@racket[_e1] and it's time to sum.

Note that this issue only comes up when @racket[_e0] is a
@bold{serious} expression, i.e. an expression that must do some
computation.  If @racket[_e0] were a literal integer or a variable, we
could emit working code.  For example:


@#reader scribble/comment-reader
(racketblock
;; Integer Expr CEnv -> Asm
;; A special case for compiling (+ i0 e1)
(define (compile-+-int i0 e1 c)
  (append (compile-e e1 c)
          (list (Add 'rax (arithmetic-shift i0 imm-shift)))))

;; Id Expr CEnv -> Asm
;; A special case for compiling (+ x0 e1)
(define (compile-+-var x0 e1)
  (let ((i (lookup x0 c)))
    (append (compile-e e1 c)   
            (list (Add 'rax (Offset 'rsp (- (add1 i))))))))
)

The latter suggests a general solution could be to transform binary
primitive applications into a @racket[let] form that binds the first
subexpression to a variable and then uses the @racket[compile-+-var]
function above.  The idea is that every time the compiler encounters
@racket[(+ _e0 _e1)], we transform it to @racket[(let ((_x _e0)) (+ _x
_e1))].  For this to work out, @racket[_x] needs to be some variable
that doesn't appear free in @racket[_e1].  This transformation is
what's called @bold{ANF} (administrative normal form) and is a widely
used intermediate representation for compilers.


But, we can also solve the problem more directly by considering the
code that is generated for the ANF style expression above.

Consider the lexical address of @racket[_x] in the transformed code
above.  It is @emph{always} 0 because the transformation puts the
@racket[let] immediately around the occurrence of @racket[_x].  So if
we're compiling @racket[(+ _e0 _e1)] in environment @racket[_c] using
this approach, we know the value of @racket[_e0] will live at
@racket[`(offset rsp ,(- (add1 (length c))))].  There's no need for a
@racket[let] binding or a fresh variable name.  And this observation
enables us to write a general purpose compiler for binary primitives
that doesn't require any program transformation: we simply push the
value of @racket[e0] on the top of the stack and retrieve it later.

Here is a first cut:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((x (gensym))) ; generate a fresh variable
    (append (compile-e e0 c)
            (list (Mov (Offset 'rsp (add1 (- (length c)))) 'rax))
            (compile-e e1 (cons x c))
            (list (Add 'rax (Offset 'rsp (- (add1 (lookup x (cons x c))))))))))
)

There are a couple things to notice.  First: the @racket[(lookup x
(cons x c))] just produces @racket[(length c)].  Second, when
compiling @racket[_e1] in environment @racket[(cons x c)], we know
that no variable in @racket[_e1] resolves to @racket[x] because
@racket[x] is a freshly @racket[gensym]'d symbol.  Putting (an
unreferenced) @racket[x] in the environment serves only to ``bump up''
by one the offset of any variable bound after @racket[x] so as to not
override the spot where @racket[e0]'s values lives.  We can accomplish
the same thing by sticking in something that no variable is equal to:
@racket[#f]:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (append (compile-e e0 c)
          (list (Mov (Offset 'rsp (add1 (- (length c)))) 'rax))
          (compile-e e1 (cons #f c))
          (list (Add 'rax (Offset 'rsp (- (add1 (length c))))))))
)
