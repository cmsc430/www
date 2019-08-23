#lang scribble/manual

@(require (for-label (except-in racket ... compile)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "dupe/semantics.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "dupe" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))


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
