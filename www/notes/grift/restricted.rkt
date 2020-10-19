#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Transformation to kind-sorta-anf-but-not-really
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "ast.rkt")
(require "syntax.rkt")

(define (to-restricted e)
  (match e
    [(int-e i)     e]
    [(bool-e b)    e]
    [(var-e v)     e]
    [(prim-e p es) (gen-lets p (convert-subs es))]
    [(if-e e t f)  (if-e (to-restricted e) (to-restricted t) (to-restricted f))]
    [(let-e bs b)  (let-e (restrict-binds bs) (to-restricted b))]))

(define (gen-lets p xs)
  (match xs
    [(cons '() vs) (prim-e p vs)]
    [(cons newbs vs) (let-e newbs (prim-e p vs))]))

(define (convert-subs es)
  (match es
    ['() (cons '() '())]
    [(cons y ys)
      (match (convert-subs ys)
        [(cons bs cs)
          (match y
            [(var-e v) (cons bs (cons y cs))]
            [(int-e i) (cons bs (cons y cs))]
            [(bool-e b) (cons bs (cons y cs))]
            [_         (let* ((v (gensym))
                              (newb (binding v (to-restricted y))))
                             (cons (cons newb bs) (cons (var-e v) cs)))])])]))


(define (restrict-binds bnds)
  (match bnds
    ['() '()]
    [(cons (binding v e) bnds) (cons (binding v (to-restricted e))
                                     (restrict-binds bnds))]))
