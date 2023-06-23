#lang crook
{:= B C D0 D1}
(provide interp-prim1)

{:> B D0} ;; Op1 Integer -> Integer
{:> B D0}
(define (interp-prim1 op i)
  (match op
    ['add1 (add1 i)]
    ['sub1 (sub1 i)]))

{:> D0} ;; Op1 Value -> Value
{:> D0}
(define (interp-prim1 op v)
  (match op
    ['add1 (add1 v)]
    ['sub1 (sub1 v)]
    ['zero? (zero? v)]
    {:> D1}
    ['char? (char? v)]
    {:> D1}
    ['integer->char (integer->char v)]
    {:> D1}
    ['char->integer (char->integer v)]))
