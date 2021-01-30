#lang racket
(require "ast.rkt")
(provide interp-prim1 interp-prim2)

;; Op1 Value -> Answer
(define (interp-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (? integer?))            (add1 v)]
    [(list 'sub1 (? integer?))            (sub1 v)]
    [(list 'zero? (? integer?))           (zero? v)]
    [(list 'char? v)                      (char? v)]
    [(list 'char->integer (? char?))      (char->integer v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'eof-object? v)                (eof-object? v)]
    [(list 'write-byte (? byte?))         (write-byte v)]
    [(list 'box v)                        (box v)]
    [(list 'unbox (? box?))               (unbox v)]
    [(list 'car (? pair?))                (car v)]
    [(list 'cdr (? pair?))                (cdr v)]
    [(list 'empty? v)                     (empty? v)]
    [_                                    'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (? integer?) (? integer?))  (+ v1 v2)]
    [(list '- (? integer?) (? integer?))  (- v1 v2)]
    [(list 'eq? v1 v2)                    (eqv? v1 v2)]
    [(list 'cons v1 v2)                   (cons v1 v2)]
    [_                                    'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
