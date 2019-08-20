#lang racket/base
(provide ev ex)
(require racket/sandbox scribble/manual scribble/examples)

(define-syntax-rule (ex e ...)
  (filebox (emph "Examples")
    (examples #:eval ev #:label #f e ...)))

(define ev
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 50])
       (make-evaluator 'racket)))))
