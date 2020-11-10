#lang racket
(require "../interp.rkt"
         "../parse.rkt"
         "../semantics.rkt"
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


;; JMCT: I don't know what's happening here :( It doesn't work after I
;; switch the the new AST even though I put `sexpr->ast` into `run` above

;;; check totality of interpreter
;(redex-check F-concrete e
;             (check-not-exn (lambda () (run (term e)))
;                            (term e))
;             #:print? #f)
;
;;; check equivalence of interpreter and semantics
;(redex-check F e
;             (check-equal? (list (run (term e)))
;                           (judgment-holds (𝑭 e a) a)
;                           (term e))
;             #:print? #f)
