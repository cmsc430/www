#lang racket
(require "../interp.rkt"
         (only-in "../semantics.rkt" G G-concrete 𝑮)
         "../parse.rkt"
         rackunit
         redex/reduction-semantics)

(define (run e)
  (interp (parse e)))

(check-equal? (run 7) 7)
(check-equal? (run -8) -8)
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)

;; Examples from the notes
(check-equal? (run '(let ((x 7)) x)) 7)
(check-equal? (run '(let ((x 7)) 2)) 2)
(check-equal? (run '(let ((x 7)) (add1 x))) 8)
(check-equal? (run '(let ((x (add1 7))) x)) 8)
(check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)


(check-equal? (run 'x) 'err)
(check-equal? (run '(add1 #f)) 'err)
(check-equal? (run '(+ 1 2)) 3)
(check-equal? (run '(zero? 0)) #t)
(check-equal? (run '(zero? 1)) #f)

;;; check totality of interpreter
(redex-check G-concrete e
             (check-not-exn (lambda () (run (term e))))
             #:print? #f)

;; DVH: there's a problem here between the AST and the AST-looking S-Expr used in the semantics.  Ugh.
;; Possible solutions:
;; (1) use an S-Expr based AST (i.e. get rid of prefab structs)
;; (2) write a parser for the AST-like S-Expr
;; (3) use prefabs in Redex models (not actually possible in Redex currently)

;;; check equivalence of interpreter and semantics
;(redex-check G e
;             (check-equal? (list (run (term e)))
;                           (judgment-holds (𝑮 e a) a))
;             #:print? #f)
