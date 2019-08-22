#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  #;(except-in "dupe/semantics.rkt" ext lookup)
	  #;(prefix-in sem: (only-in "dupe/semantics.rkt" ext lookup))
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "dupe" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title{Conditional computation}

Let's now consider add a notion of @bold{conditionals} to our target
language.

@section{Dupe: branching with conditionals}

We'll call it @bold{Dupe}.

We will use the following syntax...

Together this leads to the following grammar for Con:

@;centered{@render-language[D-pre]}

Which can be modeled with the following data type definition:

@;codeblock-include["dupe/ast.rkt"]

We will also need a predicate for well-formed Con expressions, but
let's return to this after considering the semantics and interpreter.

@section{Meaning of Dupe programs}

The meaning of Dupe programs depends on the form of the expression and
the new form is an if-expression.

@itemlist[

@item{the meaning of a if expression @tt{(if (zero? e0) e1 e2)} is the
meaning of @math{e1} if the meaning of @tt{e1} if the meaning of
@tt{e0} is 0 and is the meaning of @math{e2} otherwise.}

]

Let's consider some examples:

@itemlist[

@item{...}

]


The semantics...

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

The interpreter ...

@codeblock-include["dupe/interp.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
'...
]

Correctness...
@;{
@bold{Interpreter Correctness}: @emph{For all Con expressions
@racket[e] and integers @racket[i], if (@racket[e],@racket[i]) in
@render-term[C 𝑪], then @racket[(con-interp e)] equals
@racket[i].}
}

@section{An Example of Dupe compilation}

Suppose we want to compile @racket['(if (zero? 8) 2 3)]...

We already know how to compile the @racket['8], @racket['2], and
@racket['3] part.

What needs to happen? ...

@filebox-include-fake[codeblock "dupe/asm/ast.rkt"]{
#lang racket
;; ... jmps, comparison
}

@codeblock-include["dupe/asm/printer.rkt"]
@codeblock-include["dupe/compile.rkt"]


@ex[
'...
]
