#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean

;; type REnv = (Listof (List Variable Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))


(define (interp-env e r)
  (match e
    [(var-e v) (lookup r v)]
    [(int-e i) i]
    [(bool-e b) b]
    [(prim-e p e)
      (if (memq p prims)
          (interp-prim p e r)
          'err)]
    [(if-e p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(let-e (list bnd) body)
       (match bnd
         [(binding v def)
              (match (interp-env def r)
                ['err 'err]
                [val  (interp-env body (ext r v val))])])]))

(define (interp-prim p e r)
  (match p
    ['add1
     (match (interp-env e r)
       [(? integer? i) (add1 i)]
       [_ 'err])]
    ['sub1
     (match (interp-env e r)
       [(? integer? i) (sub1 i)]
       [_ 'err])]
    ['zero?
     (match (interp-env e r)
       [(? integer? i) (zero? i)]
       [_ 'err])]))

(define (lookup r v)
  (lookup-prime r r v))

(define (lookup-prime init r v)
  (match r
    ['() (error (format "~a is not found in the environment. Current env: ~a" v init))]
    [(cons (list var val) rest) (if (symbol=? v var)
                                    val
                                    (lookup-prime init rest v))]))

(define (ext r v val)
  (cons (list v val) r))
