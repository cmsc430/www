#lang racket
(provide interp interp-bits)
(require "ast.rkt" "types.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character

;; type Bits = Integer

;; Expr -> Value
(define (interp e)
  (bits->value (interp-bits e)))

;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Int i)  (value->bits i)]
    [(Char c) (value->bits c)]
    [(Bool b) (value->bits b)]
    [(Prim1 'add1 e0)
     (+ (interp-bits e0) (value->bits 1))]
    [(Prim1 'sub1 e0)
     (- (interp-bits e0) (value->bits 1))]
    [(Prim1 'zero? e)
     (if (zero? (interp-bits e))
         val-true
         val-false)]
    [(Prim1 'char? e0)
     (if (= type-char (bitwise-and (interp-bits e0) #b11))
         val-true
         val-false)]
    [(Prim1 'char->integer e0)
     (arithmetic-shift
      (arithmetic-shift (interp-bits e0) (- char-shift))
      int-shift)]
    [(Prim1 'integer->char e0)
     (bitwise-ior
      (arithmetic-shift
       (arithmetic-shift (interp-bits e0) (- int-shift))
       char-shift)
      type-char)]
    [(If e1 e2 e3)
     (if (= (interp-bits e1) val-false)
         (interp-bits e3)
         (interp-bits e2))]))
