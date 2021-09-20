#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(integer? s) (Int s)]
    [(boolean? s) (Bool s)]
    [(char? s)    (Char s)]    
    ['eof         (Eof)]
    [(list (? op0? o))    (Prim0 o)]
    [(list (? op1? o) e)  (Prim1 o (parse e))]
    [(list 'begin e1 e2)  (Begin (parse e1) (parse e2))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [_ (error "Parse error")]))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-bytes)))

;; Any -> Boolean
(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?)))
