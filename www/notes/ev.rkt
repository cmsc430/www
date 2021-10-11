#lang racket/base
(provide ev ex)
(require racket/sandbox scribble/manual scribble/examples)

(define-syntax-rule (ex e ...)
  (filebox (emph "Examples")
    (examples #:eval ev #:label #f e ...)))

(define ev
  (make-base-eval #:lang 'racket))

(ev '(require (only-in racket/pretty pretty-print-columns)))
(ev '(pretty-print-columns 50))

  #|
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 50])
       (make-evaluator 'racket)))))
|#
