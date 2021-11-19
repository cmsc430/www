#lang racket
(require "ast.rkt")
(provide interp-prim)

;; type Struct = (StructVal Symbol (Vectorof Value))
(struct StructVal (name vals))

;; Op [Listof Value] -> Answer
(define (interp-prim p vs)
  (match (cons p vs)
    ;; Op0
    [(list 'void) (void)]
    [(list 'read-byte) (read-byte)]
    [(list 'peek-byte) (peek-byte)]
    ;; Op1
    [(list 'add1 (? integer? v))            (add1 v)]
    [(list 'sub1 (? integer? v))            (sub1 v)]
    [(list 'zero? (? integer? v))           (zero? v)]
    [(list 'char? v)                        (char? v)]
    [(list 'char->integer (? char? v))      (char->integer v)]
    [(list 'integer->char (? codepoint? v)) (integer->char v)]
    [(list 'eof-object? v)                  (eof-object? v)]
    [(list 'write-byte (? byte? v))         (write-byte v)]
    [(list 'box v)                          (box v)]
    [(list 'unbox (? box? v))               (unbox v)]
    [(list 'car (? pair? v))                (car v)]
    [(list 'cdr (? pair? v))                (cdr v)]
    [(list 'empty? v)                       (empty? v)]
    [(list 'cons? v)                        (cons? v)]
    [(list 'box? v)                         (box? v)]
    [(list 'vector? v)                      (vector? v)]
    [(list 'vector-length (? vector? v))    (vector-length v)]
    [(list 'string? v)                      (string? v)]
    [(list 'string-length (? string? v))    (string-length v)]
    [(list 'symbol? v)                      (symbol? v)]    
    [(list 'symbol->string (? symbol? v))   (symbol->string v)]
    [(list 'string->symbol (? string? v))   (string->symbol v)]
    [(list 'string->uninterned-symbol (? string? v))
     (string->uninterned-symbol v)]
    ;; Op2
    [(list '+ (? integer? v1) (? integer? v2))  (+ v1 v2)]
    [(list '- (? integer? v1) (? integer? v2))  (- v1 v2)]
    [(list '< (? integer? v1) (? integer? v2))  (< v1 v2)]
    [(list '= (? integer? v1) (? integer? v2))  (= v1 v2)]    
    [(list 'cons v1 v2)                   (cons v1 v2)]
    [(list 'eq? v1 v2)                    (eq? v1 v2)]    
    [(list 'make-vector (? integer? v1) v2)
     (if (<= 0 v1)
         (make-vector v1 v2)
         'err)]
    [(list 'vector-ref (? vector? v1) (? integer? v2))
     (if (<= 0 v2 (sub1 (vector-length v1)))
         (vector-ref v1 v2)
         'err)]
    [(list 'make-string (? integer? v1) (? char? v2))
     (if (<= 0 v1)
         (make-string v1 v2)
         'err)]
    [(list 'string-ref (? string? v1) (? integer? v2))
     (if (<= 0 v2 (sub1 (string-length v1)))
         (string-ref v1 v2)
         'err)]
    [(list 'struct? s v)
     (match v
       [(StructVal n _) (eq? s n)]
       [_ #f])]
    ;; Op3
    [(list 'vector-set! (? vector? v1) (? integer? v2) v3)
     (if (<= 0 v2 (sub1 (vector-length v1)))
         (vector-set! v1 v2 v3)
         'err)]
    [(list 'struct-ref s i (StructVal n vs))
     (if (and (eq? s n) (<= 0 i (sub1 (vector-length vs))))
         (vector-ref vs i)
         'err)]
    ;; OpN
    [(cons 'make-struct (cons (? symbol? n) vs))
     (StructVal n (list->vector vs))]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
