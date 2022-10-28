#lang racket
(provide compile con-compiler)
(require "ast.rkt" "../a86/ast.rkt"
         "../blackmail/compile.rkt")

(define con-compiler
  (class blackmail-compiler
    (super-new)

    (inherit compile)

    ;; Expr -> Asm
    (define/override (compile-e e)
      (match e
        [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3)]
        [_ (super compile-e e)]))

    ;; Expr Expr Expr -> Asm
    (define (compile-ifzero e1 e2 e3)
      (let ((l1 (gensym 'if))
            (l2 (gensym 'if)))
        (seq (compile-e e1)
             (Cmp 'rax 0)
             (Je l1)
             (compile-e e3)
             (Jmp l2)
             (Label l1)
             (compile-e e2)
             (Label l2))))))

(define compile
  (let ([compiler (new con-compiler)])
    (λ (p) (send compiler compile p))))
