#lang racket
(provide compile abscond-compiler)
(require "ast.rkt" "../a86/ast.rkt")

(define abscond-compiler
  (class object%
    (super-new)

    ;; Expr -> Asm
    (define/public (compile e)
      (prog (Global 'entry)
            (Label 'entry)
            (compile-e e)
            (Ret)))

    ;; Expr -> Asm
    (define/public (compile-e e)
      (match e
        [(Int i) (seq (Mov 'rax i))]))))

(define compile
  (let ([compiler (new abscond-compiler)])
    (λ (p) (send compiler compile p))))
