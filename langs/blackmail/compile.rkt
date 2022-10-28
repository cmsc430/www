#lang racket
(provide compile blackmail-compiler)
(require "ast.rkt" "../a86/ast.rkt"
         "../abscond/compile.rkt")

(define blackmail-compiler
  (class abscond-compiler
    (super-new)

    (inherit compile)

    ;; Expr -> Asm
    (define/override (compile-e e)
      (match e
        [(Prim1 p e) (compile-prim1 p e)]
        [_ (super compile-e e)]))

    ;; Op Expr -> Asm
    (define (compile-prim1 p e)
      (seq (compile-e e)
           (match p
             ['add1 (Add 'rax 1)]
             ['sub1 (Sub 'rax 1)])))))

(define compile
  (let ([compiler (new blackmail-compiler)])
    (λ (p) (send compiler compile p))))
