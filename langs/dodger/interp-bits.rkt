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
    [(Lit d)  (value->bits d)]
    [(Prim1 'add1 e0)
     (+ (interp-bits e0) (value->bits 1))]
    [(Prim1 'sub1 e0)
     (- (interp-bits e0) (value->bits 1))]
    [(Prim1 'zero? e)
     (value->bits (zero? (interp-bits e)))]
    [(Prim1 'char? e0)
     (value->bits (char-bits? (interp-bits e0)))]
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
     (if (= (interp-bits e1) (value->bits #f))
         (interp-bits e3)
         (interp-bits e2))]))
