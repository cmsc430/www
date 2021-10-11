#lang scribble/manual

@(require (for-label (except-in racket compile ...) a86))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  #;(except-in "con/semantics.rkt" ext lookup)
	  #;(prefix-in sem: (only-in "con/semantics.rkt" ext lookup))
	  "utils.rkt"
	  "ev.rkt"
	  "../../langs/con/semantics.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))


@(ev '(require rackunit a86))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "con" f))))))
	   '("interp.rkt" "compile.rkt" "parse.rkt" "ast.rkt" "random.rkt"))


@title[#:tag "Con"]{Con: branching with conditionals}

@emph{When you come to a fork in the road, take it.}

@table-of-contents[]

@section{Conditional execution}

Let's now consider adding a notion of @bold{conditionals} to our target
language.

We'll call it @bold{Con}.

We will use the following concrete syntax: @racket[(if (zero? _e0) _e1 _e2)].

This leads to the following grammar for concrete Con:

@centered{@render-language[C-concrete]}

And abstract grammar:

@centered{@render-language[C]}

Which can be modeled with the following definitions:

@codeblock-include["con/ast.rkt"]

@;{
 We will also need a predicate for well-formed Con expressions, but
let's return to this after considering the semantics and interpreter.
}

The parser is similar to what we've seen before:

@codeblock-include["con/parse.rkt"]


@section{Meaning of Con programs}

The meaning of Con programs depends on the form of the expression and
the new form is an if-expression.

@itemlist[

@item{the meaning of a if expression @racket[(IfZero _e0 _e1 _e2)] is
the meaning of @racket[_e1] if the meaning of @racket[_e0] is 0 and is
the meaning of @racket[_e2] otherwise.}

]

Let's consider some examples (using concrete notation):

@itemlist[

@item{@racket[(if (zero? 0) (add1 2) 4)] means @racket[3].}
@item{@racket[(if (zero? 1) (add1 2) 4)] means @racket[4].}
@item{@racket[(if (zero? (if (zero? (sub1 1)) 1 0)) (add1 2) 4)] means @racket[4].}
@item{@racket[(if (zero? (add1 0)) (add1 2) (if (zero? (sub1 1)) 1 0))] means @racket[1].}

]


The semantics is inductively defined as before.  There are @emph{two}
new rules added for handling if-expressions: one for when the test
expression means @racket[0] and one for when it doesn't.

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
                                ['- (rewrite '–)]
                                ['= (rewrite '=)]
				['!= (rewrite '≠)])
        (apply centered
	   (add-between 
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

@(show-judgment 𝑪 0 3)
@(show-judgment 𝑪 3 5)


@;{
The heart of the semantics is an auxiliary relation, @render-term[C
𝑪𝒓], which relates an expression and an environement to the integer
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

@(show-judgment 𝑪𝒓 0 3)
@(show-judgment 𝑪𝒓 3 5)

It relies on two functions: one for extending an environment with a
variable binding and one for lookup up a variable binding in an
environment:

@centered{
@render-metafunction[sem:ext #:contract? #t]

@(with-atomic-rewriter
  'undefined
  "⊥"
  (render-metafunction sem:lookup #:contract? #t))}

The operational semantics for Con is then defined as a binary relation
@render-term[C 𝑪], which says that @math{(e,i)} in @render-term[C 𝑪],
only when @math{e} evaluates to @math{i} in the empty environment
according to @render-term[C 𝑪𝒓]:

@(show-judgment 𝑪 0 1)
}

The interpreter has an added case for if-expressions, which
recursively evaluates the test expression and branches based on its
value.

@codeblock-include["con/interp.rkt"]

We've also made one trivial change, which is to move @racket[interp-prim1] to its
own module.  This will be useful in the future when more primitive operations are
added, we won't have to clutter up the interpreter:

@codeblock-include["con/interp-prim.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier (using @racket[parse] to state the examples
with concrete notation):

@ex[
(interp (parse '(if (zero? 0) (add1 2) 4)))
(interp (parse '(if (zero? 1) (add1 2) 4)))
(interp (parse '(if (zero? (if (zero? (sub1 1)) 1 0)) (add1 2) 4)))
(interp (parse '(if (zero? (add1 0)) (add1 2) (if (zero? (sub1 1)) 1 0))))
]

The argument for the correctness of the interpreter follows the same
structure as for @seclink["Blackmail"]{Blackmail}, but with an added case for
if-expressions.

@section{An Example of Con compilation}

Suppose we want to compile @racket[(if (zero? 8) 2 3)]...

We already know how to compile the @racket[8], @racket[2], and
@racket[3] part.

What needs to happen?

@itemlist[
@item{Execute the code for @racket[8] leaving the result in @racket['rax],}
@item{check whether @racket['rax] holds zero,}
@item{if it does, execute the code for @racket[2],}
@item{if it doesn't, execute the code for @racket[3].}
]

We can determine whether @racket[8] evaluates to @racket[0] using a
comparison instruction: @racket[(Cmp rax 0)].  To do the conditional
execution, we will need to jump to different parts of the code to
either execute the code for @racket[2] or @racket[3].  There are
several ways we could accomplish this, but we take the following
approach: immediately after the comparison, do a conditional jump to
the code for the then branch when zero. Should the jump not occur,
the next instructions will carry out the evaluation of the else
branch, then (unconditionally) jump over the then branch code.

To accomplish this, we will need two new labels: one for the then
branch code and one for the end of the then branch code.  The
@racket[gensym] function can be used to generate symbols that have not
appeared before.

In total, the code for this example would look like:

@racketblock[
(let ((l0 (gensym))
      (l1 (gensym)))
  (list (Mov 'rax 8)
        (Cmp 'rax 0)
        (Je l0)
        (Mov 'rax 3)
        (Jmp l1)
        (Label l0)
        (Mov rax 2)
        (Label l1)))
]


@section{A Compiler for Con}

Notice that the @racket[(Mov 'rax 8)], @racket[(Mov rax 3)] and
@racket[(Mov rax 2)] parts are just the instructions generated by
compiling @racket[8], @racket[2] and @racket[3].  Generalizing from
this, we arrive at the following code for the compiler:

@racketblock[
(let ((l0 (gensym 'if))
      (l1 (gensym 'if)))
  (append (compile-e e1)
          (list (Cmp 'rax 0)
                (Je l0))
          (compile-e e3)
          (list (Jmp l1)
                (Label l0))
          (compile-e e2)
          (list (Label l1))))
]


This will require extending our use of a86 instructions; in
particular, we add @racket[Jmp], @racket[Je], and @racket[Cmp]
instructions.

The complete compiler code is:

@codeblock-include["con/compile.rkt"]

Mirroring the change we made to the interpreter, we separate out a
module for compiling primitives:

@codeblock-include["con/compile-prim.rkt"]

Let's take a look at a few examples:
@ex[
(define (show s)
  (compile-e (parse s)))
    
(show '(if (zero? 8) 2 3))
(show '(if (zero? 0) 1 2))
(show '(if (zero? 0) (if (zero? 0) 8 9) 2))
(show '(if (zero? (if (zero? 2) 1 0)) 4 5))
]

And confirm they are running as expected:
@ex[
(define (tell s)
  (asm-interp (compile (parse s))))

(tell '(if (zero? 8) 2 3))
(tell '(if (zero? 0) 1 2))
(tell '(if (zero? 0) (if (zero? 0) 8 9) 2))
(tell '(if (zero? (if (zero? 2) 1 0)) 4 5))
]


@section[#:tag-prefix "con"]{Correctness and random testing}

The statement of correctness follows the same outline as before:

@bold{Compiler Correctness}: @emph{For all expressions @racket[e] and
integers @racket[i], if (@racket[e],@racket[i]) in @render-term[C 𝑪],
then @racket[(asm-interp (compile e))] equals @racket[i].}

Again, we formulate correctness as a property that can be tested:

@ex[
(define (check-compiler e)
  (check-equal? (asm-interp (compile e))
                (interp e)
                e))]

Generating random Con programs is essentially the same as Blackmail
programs, and are provided in a @link["con/random.rkt"]{random.rkt}
module.

@ex[
(eval:alts (require "random.rkt") (void))
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(for ([i (in-range 10)])
  (check-compiler (random-expr)))
]
