#lang scribble/manual

@(require "utils.rkt"
	  "../utils.rkt")


@(define codeblock-include (make-codeblock-include #'h))

@title[#:tag "Interlude"]{Refactoring the Compiler}

We've now written half-a-dozen compilers, each of which has centered
around a structurally recursive function over the syntax of
@tt{Expr}s.

In each case of this function, we emit the appropriate code for that
kind of expression.

This works fine while our language is small and the concepts are
simple enough that just a few instructions need to be produced.

But this leads to a monolithic and unwieldy function as the language
and the complexity of its constructs grow.

Moving forward we will refactor this main compiler function to call
out to a separate helper function for each kind of expression.

For example, here is the Fraud compiler written in the new style:


@codeblock-include["fraud/compile-refactor.rkt"]