#lang crook
{:= A B C D0 D1}
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    {:> A D0}
    [(? exact-integer?) (Lit s)]
    {:> D0}
    [(? datum?)          (Lit s)]
    {:> B}
    [(list (? op1? o) e) (Prim1 o (parse e))]
    {:> C D0}
    [(list 'if (list 'zero? e1) e2 e3)
     (IfZero (parse e1) (parse e2) (parse e3))]
    {:> D0}
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [_ (error "Parse error")]))

{:> D0} ;; Any -> Boolean
{:> D0}
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      {:> D1}
      (char? x)))

{:> B} ;; Any -> Boolean
{:> B}
(define (op1? x)
  (memq x '(add1 sub1 {:> D0} zero? {:> D1} char? {:> D1} integer->char {:> D1} char->integer)))
