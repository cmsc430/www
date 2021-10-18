#lang racket
(provide interp-prim0 interp-prim1 interp-prim2)

;; Op0 -> Answer
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; Op1 Value -> Answer
(define (interp-prim1 op v)
  (match op
    ['add1          (if (integer? v) (add1 v) 'err)]
    ['sub1          (if (integer? v) (sub1 v) 'err)]
    ['zero?         (if (integer? v) (zero? v) 'err)]
    ['char?         (char? v)]
    ['char->integer (if (char? v) (char->integer v) 'err)]
    ['integer->char (if (codepoint? v) (integer->char v) 'err)]
    ['eof-object?   (eof-object? v)]
    ['write-byte    (if (byte? v) (write-byte v) 'err)]))

;; Op2 Value Value -> Answer
(define (interp-prim2 op v1 v2)
  (match op
    ['+ (if (and (integer? v1) (integer? v2)) (+ v1 v2) 'err)]
    ['- (if (and (integer? v1) (integer? v2)) (- v1 v2) 'err)]
    ['< (if (and (integer? v1) (integer? v2)) (< v1 v2) 'err)]
    ['= (if (and (integer? v1) (integer? v2)) (= v1 v2) 'err)]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
