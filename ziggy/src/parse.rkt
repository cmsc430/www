#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1}
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    {:> E0}
    ['eof                (Eof)]
    {:> A D0}
    [(? exact-integer?) (Lit s)]
    {:> D0}
    [(? datum?)          (Lit s)]
    {:> F}
    [(? symbol?)         (Var s)]
    {:> H0}
    [(list 'quote (list)) (Empty)]
    {:> E0}
    [(list (? op0? o))   (Prim0 o)]
    {:> B}
    [(list (? op1? o) e) (Prim1 o (parse e))]
    {:> F}
    [(list (? op2? o) e1 e2) (Prim2 o (parse e1) (parse e2))]
    {:> H1}
    [(list (? op3? o) e1 e2 e3) (Prim3 o (parse e1) (parse e2) (parse e3))]
    {:> E0}
    [(list 'begin e1 e2) (Begin (parse e1) (parse e2))]
    {:> C D0}
    [(list 'if (list 'zero? e1) e2 e3)
     (IfZero (parse e1) (parse e2) (parse e3))]
    {:> D0}
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    {:> F}
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))]
    [_ (error "Parse error")]))

{:> D0} ;; Any -> Boolean
{:> D0}
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      {:> D1}
      (char? x)
      {:> H1}
      (string? x)))

{:> E0} ;; Any -> Boolean
{:> E0}
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

{:> B}
(define (op1? x)
  (memq x '(add1 sub1 {:> D0} zero? {:> D1} char? {:> D1} integer->char {:> D1} char->integer
                 {:> E0} write-byte {:> E0} eof-object?
                 {:> H0} box {:> H0} unbox {:> H0} empty? {:> H0} cons? {:> H0} box? {:> H0} car {:> H0} cdr
                 {:> H1} vector? {:> H1} vector-length {:> H1} string? {:> H1} string-length)))

{:> F}
(define (op2? x)
  (memq x '(+ - < = {:> H0} eq? {:> H0} cons
              {:> H1} make-vector {:> H1} vector-ref {:> H1} make-string {:> H1} string-ref)))

{:> H1}
(define (op3? x)
  (memq x '(vector-set!)))
