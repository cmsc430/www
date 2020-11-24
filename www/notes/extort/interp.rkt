#lang racket
(provide interp)
(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; Expr -> Answer
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim1 p e0)
     (match (interp e0)       
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(If e1 e2 e3)
     (match (interp e1)
       ['err 'err]
       [v
        (if v
            (interp e2)
            (interp e3))])]
    [(Begin e1 e2)
     (begin (interp e1)
            (interp e2))]))

;; Op1 Value -> Answer
(define (interp-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (? integer?)) (add1 v)]
    [(list 'sub1 (? integer?)) (sub1 v)]
    [(list 'zero? (? integer?)) (zero? v)]
    [(list 'char? v) (char? v)]
    [(list 'char->integer (? char?)) (char->integer v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'eof-object? v) (eof-object? v)]
    [(list 'write-byte (? byte?)) (write-byte v)]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

