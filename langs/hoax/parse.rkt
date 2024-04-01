#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    ['eof                (Eof)]
    [(? datum?)          (Lit s)]
    [(? symbol?)         (Var s)]
    [(list 'quote (list)) (Lit '())]
    [(list (? op0? o))   (Prim0 o)]
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [(list (? op2? o) e1 e2) (Prim2 o (parse e1) (parse e2))]
    [(list (? op3? o) e1 e2 e3) (Prim3 o (parse e1) (parse e2) (parse e3))]
    [(list 'begin e1 e2) (Begin (parse e1) (parse e2))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))]
    [_ (error "Parse error")]))


;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)
      (string? x)))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?
                 box unbox empty? cons? box? car cdr
                 vector? vector-length string? string-length)))

(define (op2? x)
  (memq x '(+ - < = eq? cons
              make-vector vector-ref make-string string-ref)))

(define (op3? x)
  (memq x '(vector-set!)))

