#lang crook
{:= B C D0 D1 E0 E1}
(provide interp-prim1 {:> E0} interp-prim0)

{:> E0} ;; Op0 -> Value
{:> E0}
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

{:> B D0} ;; Op1 Integer -> Integer
{:> B D0}
(define (interp-prim1 op i)
  (match op
    ['add1 (add1 i)]
    ['sub1 (sub1 i)]))

{:> D0 E1} ;; Op1 Value -> Value
{:> D0 E1}
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
    ['char->integer (char->integer v)]
    {:> E0}
    ['write-byte    (write-byte v)]
    {:> E0}
    ['eof-object?   (eof-object? v)]))

{:> E1} ;; Op1 Value -> Answer
{:> E1}
(define (interp-prim1 op v)
  (match (list op v)
    [(list 'add1 (? integer?))            (add1 v)]
    [(list 'sub1 (? integer?))            (sub1 v)]
    [(list 'zero? (? integer?))           (zero? v)]
    [(list 'char? v)                      (char? v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'char->integer (? char?))      (char->integer v)]
    [(list 'write-byte    (? byte?))      (write-byte v)]
    [(list 'eof-object? v)                (eof-object? v)]
    [_ 'err]))

{:> E1} ;; Any -> Boolean
{:> E1}
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
