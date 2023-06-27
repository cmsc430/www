#lang crook
{:= B C D0 D1 E0 E1 F H0 H1}
(provide {:> E0} interp-prim0 interp-prim1 {:> F} interp-prim2 {:> H1} interp-prim3)

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
    {:> H0} [(list 'box v)                        (box v)]
    {:> H0} [(list 'unbox (? box?))               (unbox v)]
    {:> H0} [(list 'car (? pair?))                (car v)]
    {:> H0} [(list 'cdr (? pair?))                (cdr v)]
    {:> H0} [(list 'empty? v)                     (empty? v)]
    {:> H0} [(list 'cons? v)                      (cons? v)]
    {:> H1} [(list 'box? v)                       (box? v)]
    {:> H1} [(list 'vector? v)                    (vector? v)]
    {:> H1} [(list 'vector-length (? vector?))    (vector-length v)]
    {:> H1} [(list 'string? v)                    (string? v)]
    {:> H1} [(list 'string-length (? string?))    (string-length v)]
    [_ 'err]))

{:> F} ;; Op2 Value Value -> Answer
{:> F}
(define (interp-prim2 op v1 v2)
  (match (list op v1 v2)
    [(list '+ (? integer?) (? integer?)) (+ v1 v2)]
    [(list '- (? integer?) (? integer?)) (- v1 v2)]
    [(list '< (? integer?) (? integer?)) (< v1 v2)]
    [(list '= (? integer?) (? integer?)) (= v1 v2)]
    {:> H0} [(list 'eq? v1 v2)                    (eq? v1 v2)]
    {:> H0} [(list 'cons v1 v2)                   (cons v1 v2)]
    {:> H1}
    [(list 'make-vector (? integer?) _)
     (if (<= 0 v1)
         (make-vector v1 v2)
         'err)]
    {:> H1}
    [(list 'vector-ref (? vector?) (? integer?))
     (if (<= 0 v2 (sub1 (vector-length v1)))
         (vector-ref v1 v2)
         'err)]
    {:> H1}
    [(list 'make-string (? integer?) (? char?))
     (if (<= 0 v1)
         (make-string v1 v2)
         'err)]
    {:> H1}
    [(list 'string-ref (? string?) (? integer?))
     (if (<= 0 v2 (sub1 (string-length v1)))
         (string-ref v1 v2)
         'err)]
    [_ 'err]))

{:> H1} ;; Op3 Value Value Value -> Answer
{:> H1}
(define (interp-prim3 p v1 v2 v3)
  (match (list p v1 v2 v3)
    [(list 'vector-set! (? vector?) (? integer?) _)
     (if (<= 0 v2 (sub1 (vector-length v1)))
         (vector-set! v1 v2 v3)
         'err)]
    [_ 'err]))

{:> E1} ;; Any -> Boolean
{:> E1}
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
