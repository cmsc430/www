#lang racket
(require "ast.rkt"
         (prefix-in % "interp-prims.rkt"))
(provide interp-prim1 interp-prim2 interp-prim3)

;; Op1 Value -> Answer
(define (interp-prim1 p1 v) (%interp-prim1 p1 v))

;; Op2 Value Stack -> Answer
(define (interp-prim2 p v2 s)
  (%interp-prim2 p (first s) v2))

;; Op3 Value Stack -> Answer
(define (interp-prim3 p v3 s)
  (%interp-prim3 p (second s) (first s) v3))
