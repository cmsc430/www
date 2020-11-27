#lang racket
(provide (all-defined-out))
;; Bit layout of values
;;
;; Values are either:
;; - Immediates: end in #b000
;; - Pointers: end in anything else
(define imm-shift 3)
(define imm-type-mask (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-tag #b000)

;; Immediates are either:
;; - Integers: end in #b0 (& immediate tag)
;; - Non-Integers: end in #b1 (& immediate tag)
(define int-shift     (add1 imm-shift))
(define int-type-mask (sub1 (arithmetic-shift 1 int-shift)))
(define int-type-tag
  (bitwise-ior (arithmetic-shift 0 (sub1 int-shift)) imm-type-tag))
(define nonint-type-tag
  (bitwise-ior (arithmetic-shift 1 (sub1 int-shift)) imm-type-tag))

;; Pointers are "tagged addresses"
;; - Boxes:   end in #b001
;; - Pairs:   end in #b010
;; - remaining bit patterns are reserved for future pointers     
;; To recover the address, xor the tag to zero it out
(define box-type-tag  #b001)
(define pair-type-tag #b010)

;; Non-Integers are either:
;; - Characters:     end in #b0 (& non-integer tag), code-point in remaining bits    
;; - Non-Characters: end in #b1 (& non-integer tag)
(define char-shift     (add1 int-shift))
(define char-type-mask (sub1 (arithmetic-shift 1 char-shift)))
(define char-type-tag
  (bitwise-ior (arithmetic-shift #b0 (sub1 char-shift)) nonint-type-tag))
(define nonchar-type-tag
  (bitwise-ior (arithmetic-shift #b1 (sub1 char-shift)) nonint-type-tag))

;; Non-Chacters are either:
;; - True:    #b0 (& non-character tag)
;; - False:   #b1 (& non-character tag)
;; - Empty:  #b10
;; - remaining bit patterns are reserved for future values.
(define singleton-shift (add1 char-shift))
(define val-true
  (bitwise-ior (arithmetic-shift #b00 (sub1 singleton-shift)) nonchar-type-tag))
(define val-false
  (bitwise-ior (arithmetic-shift #b01 (sub1 singleton-shift)) nonchar-type-tag))
(define val-empty
  (bitwise-ior (arithmetic-shift #b10 (sub1 singleton-shift)) nonchar-type-tag))
