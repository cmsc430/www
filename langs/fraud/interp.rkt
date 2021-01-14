#lang racket
(provide interp interp-env interp-prim1)
(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type REnv = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    [(Let x e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]))

;; Op1 Value -> Answer
(define (interp-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (? integer?)) (add1 v)]
    [(list 'sub1 (? integer?)) (sub1 v)]
    [(list 'zero? (? integer?)) (zero? v)]
    [(list 'char? v) (char? v)]
    [(list 'char->integer (? char?)) (char->integer v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'eof-object? v) (eof-object? v)]
    [(list 'write-byte (? byte?)) (write-byte v)]
    [_ 'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (? integer?) (? integer?)) (+ v1 v2)]
    [(list '- (? integer?) (? integer?)) (- v1 v2)]
    [_ 'err]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r v val)
  (cons (list v val) r))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
