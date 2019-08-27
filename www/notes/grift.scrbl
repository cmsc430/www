#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  "grift/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "grift" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Grift"]{Grift: binary operations}

@codeblock-include["grift/ast.rkt"]

@centered[(render-language G)]

@(judgment-form-cases #f)

@centered[(render-judgment-form 𝑮-𝒆𝒏𝒗)]

@centered[(render-metafunction 𝑮-𝒑𝒓𝒊𝒎 #:contract? #t)]


@codeblock-include["grift/interp.rkt"]

@codeblock-include["grift/compile.rkt"]
