#lang racket
(provide interp-prim0 interp-prim1 interp-prim2)

;; Op0 -> Answer
(define (interp-prim0 p0)
  (match p0
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void (void)]))

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

;; Op2 Value Value -> Answer
(define (interp-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (? integer?) (? integer?)) (+ v1 v2)]
    [(list '- (? integer?) (? integer?)) (- v1 v2)]
    [_ 'err]))


;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))