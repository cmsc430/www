#lang racket/base
(provide ev)
(require racket/sandbox)

(define ev
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 50])
       (make-evaluator 'racket)))))
