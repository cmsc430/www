#lang scribble/manual

@(require (for-label (except-in racket ... compile)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "dupe/semantics.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "dupe" f))))))
	   '("interp.rkt" "compile.rkt" "random.rkt" "asm/interp.rkt" "asm/printer.rkt"))


@title[#:tag "Dupe"]{Dupe: a duplicity of types}

Up until now we have worked to maintain a single type of values:
integers.  This led to some awkwardness in the design of conditionals.
Let us lift this restriction by considering a multiplicity of types.
To start, we will consider two: integers and booleans.

We'll call it @bold{Dupe}.

We will use the following syntax...

Together this leads to the following grammar for Dupe:

@centered{@render-language[D]}

Which can be modeled with the following data type definition:

@codeblock-include["dupe/ast.rkt"]

We also have a predicate for well-formed Dupe expressions:

@codeblock-include["dupe/syntax.rkt"]

@section{Meaning of Dupe programs}

The meaning of Dupe programs...

@itemlist[

@item{...}

]

Let's consider some examples:

@itemlist[

@item{...}

]

Once a multiplicity of types are introduced, we are forced to come to
grips with @bold{type mismatches}.  For example, what should
@racket['(add1 #f)] mean?  What about @racket['(if 7 #t -2)]?

Languages adopt several approaches:

@itemlist[

@item{Prohibit such programs statically with a type system (e.g. OCaml, Java)}
@item{Coerce values to different types (e.g. JavaScript)}
@item{Signal a run-time error (e.g. Racket)}
@item{Leave the behavior unspecified (e.g. Scheme, C)}

]

We are going to start by taking the last approach.  Later we can
reconsider the design, but for now this is a simple (and dangerous!)
approach.


The semantics...

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

@(show-judgment 𝑫 0 3)
@(show-judgment 𝑫 3 5)
@(show-judgment 𝑫 5 7)

This relies on two helper judgments:

@(show-judgment is-true 0 2)
@(show-judgment is-false 0 1)

Notice that here we are following the semantics of Racket, which says
that anything that is not @racket[#f] counts as @emph{true} in a
conditional.

Returning to the issue of type mismatches, what does the semantics say
about @racket['(add1 #f)]?  What about @racket['(if 7 #t -2)]?


The interpreter ...

@codeblock-include["dupe/interp.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
'...
]

Correctness...
@;{
@bold{Interpreter Correctness}: @emph{For all Dupe expressions
@racket[e] and integers @racket[i], if (@racket[e],@racket[i]) in
@render-term[C 𝑪], then @racket[(interp e)] equals
@racket[i].}
}

@section{An Example of Dupe compilation}

Suppose we want to compile ...

Run-time support...

@filebox-include[fancy-c "dupe/main.c"]


@section{A Compiler for Dupe}

What needs to happen? ...

@codeblock-include["dupe/asm/ast.rkt"]

We omit the printer code, which is mundane.  See
@link["dupe/asm/printer.rkt"]{@tt{asm/printer.rkt}} for details.

@codeblock-include["dupe/compile.rkt"]


@ex[
(asm-interp (compile #t))
(asm-interp (compile #f))
(asm-interp (compile '(zero? 0)))
(asm-interp (compile '(zero? -7)))
(asm-interp (compile '(if #t 1 2)))
(asm-interp (compile '(if #f 1 2)))
(asm-interp (compile '(if (zero? 0) (if (zero? 0) 8 9) 2)))
(asm-interp (compile '(if (zero? (if (zero? 2) 1 0)) 4 5)))
]


@section{Correctness and testing}

We can randomly generate Dupe programs.  The problem is many randomly
generated programs will have type errors in them:

@ex[
(eval:alts (require "random.rkt") (void))
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
]

When interpreting programs with type errors, we get @emph{Racket}
errors, i.e. the Racket functions used in the implementation of the
interpreter will signal an error:
@ex[
(eval:error (interp '(add1 #f)))
(eval:error (interp '(if (zero? #t) 7 8)))
]

On the hand, the compiler will simply do something by misinterpreting the meaning
of the bits:
@ex[
(asm-interp (compile '(add1 #f)))
(asm-interp (compile '(if (zero? #t) 7 8)))
]

@;codeblock-include["dupe/correct.rkt"]

This complicates testing the correctness of the compiler.  Consider
our usual appraoch:
@ex[
(define (check-correctness e)
  (check-eqv? (interp e)
              (asm-interp (compile e))))

(check-correctness '(add1 7))
(eval:error (check-correctness '(add1 #f)))
]

This isn't a counter-example to correctness because @racket['(add1
#f)] is not meaningful according to the semantics.  Consequently the
interpreter and compiler are free to do anything on this input.

Since we know Racket will signal an error when the interpreter tries
to interpret a meaningless expression, we can write an alternate
@racket[check-correctness] function that catches any exceptions and
produces void, effectively ignoring the test:

@ex[
(define (check-correctness e)
  (with-handlers ([exn:fail? void])
    (check-eqv? (interp e)
                (asm-interp (compile e)))))

(check-correctness '(add1 7))
(check-correctness '(add1 #f))
]

Using this approach, we check the equivalence of the results only when
the interpreter runs without causing an error.


