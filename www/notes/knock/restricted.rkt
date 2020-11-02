#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Transformation to kind-sorta-anf-but-not-really
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "ast.rkt")
(require "syntax.rkt")

(define (to-restricted p)
  (match p
    [(prog ds e)
      (prog (map to-restricted-fun ds) (to-restricted-expr e))]))

(define (to-restricted-fun d)
  (match d
    [(fundef name args body)
      (fundef name args (to-restricted-expr body))]))

(define (to-restricted-expr e)
  (match e
    [(nil-e)       e]
    [(int-e i)     e]
    [(bool-e b)    e]
    [(var-e v)     e]
    [(char-e c)    e]
    [(fun-e f)     e]
    [(call-e f es) (gen-lets call-e f (convert-subs es))]
    [(app-e f es)  (gen-lets app-e f (convert-subs es))]
    [(prim-e p es) (gen-lets prim-e p (convert-subs es))]
    [(if-e e t f)  (if-e (to-restricted-expr e) (to-restricted-expr t) (to-restricted-expr f))]
    [(let-e bs b)  (let-e (restrict-binds bs) (to-restricted-expr b))]))

(define (gen-lets node p xs)
  (match xs
    [(cons '() vs) (node p vs)]
    [(cons newbs vs) (let-e newbs (node p vs))]))

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
                              (newb (binding v (to-restricted-expr y))))
                             (cons (cons newb bs) (cons (var-e v) cs)))])])]))


(define (restrict-binds bnds)
  (match bnds
    ['() '()]
    [(cons (binding v e) bnds) (cons (binding v (to-restricted-expr e))
                                     (restrict-binds bnds))]))
