#lang racket
(provide interp/io)
(require "interp.rkt")

;; (Expr String -> String
;; Interpret e with given string as input,
;; collect output as string (including printed result)
(define (interp/io e in)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string in)))
    (cons (interp e)
          (get-output-string (current-output-port)))))
