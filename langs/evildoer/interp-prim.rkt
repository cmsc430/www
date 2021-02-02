#lang racket
(provide interp-prim0 interp-prim1)

;; Op0 -> Value
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; Op1 Value -> Value
(define (interp-prim1 op v)
  (match op
    ['add1          (add1 v)]
    ['sub1          (sub1 v)]
    ['zero?         (zero? v)]
    ['char?         (char? v)]    
    ['integer->char (integer->char v)]
    ['char->integer (char->integer v)]
    ['write-byte    (write-byte v)]
    ['eof-object?   (eof-object? v)]))
