#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../interp-defun.rkt"
         "../interp-io.rkt")

(define (closure->proc xs e r)
  ;; Could make this better by calling the interpreter,
  ;; but it's only used in tests where all we care about
  ;; is that you get a procedure.
  (lambda _
    (error "This function is not callable.")))

(test-runner
 (λ p
  (match (interp (parse p))
    [(Closure xs e r) (closure->proc xs e r)]
    [v v])))
(test-runner-io
 (λ (s . p)
  (match (interp/io (parse p) s)
    [(cons (Closure xs e r) o)
     (cons (closure->proc xs e r) o)]
    [r r])))
