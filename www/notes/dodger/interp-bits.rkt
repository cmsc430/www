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
    [(Prim 'add1 e0)
     (+ (interp-bits e0) (value->bits 1))]
    [(Prim 'sub1 e0)
     (- (interp-bits e0) (value->bits 1))]
    [(Prim 'zero? e)
     (if (zero? (interp-bits e))
         val-true
         val-false)]
    [(Prim 'char? e0)
     (if (char-bits? (interp-bits e0))
         val-true
         val-false)]
    [(Prim 'char->integer e0)
     (integer->integer-bits      
      (char-bits->integer (interp-bits e0)))]
    [(Prim 'integer->char e0)
     (integer->char-bits
      (integer-bits->integer (interp-bits e0)))]
    [(If e1 e2 e3)
     (if (= (interp-bits e1) val-false)
         (interp-bits e3)
         (interp-bits e2))]))

(define (char-bits->integer b)
  (arithmetic-shift b (- char-shift)))

(define (integer-bits->integer b)
  (arithmetic-shift b (- int-shift)))

(define (integer->integer-bits b)
  (bitwise-ior type-int
               (arithmetic-shift b int-shift)))

(define (integer->char-bits b)    
  (bitwise-ior type-char
               (arithmetic-shift b char-shift)))               

(define (char-bits? b)
  (= (bitwise-and (sub1 (arithmetic-shift 1 char-shift)) b)
     type-char))

