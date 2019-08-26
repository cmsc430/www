#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  ;"hustle/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "hustle" f))))))
	   '() #;'("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Hustle"]{Hustle: heaps and lists}

@;codeblock-include["hustle/ast.rkt"]

@;codeblock-include["hustle/interp.rkt"]

@;codeblock-include["hustle/compile.rkt"]
