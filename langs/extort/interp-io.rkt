#lang racket
(provide interp/io)
(require "interp.rkt")

;; Expr String -> (Cons Value String)
;; Interpret e with given string as input,
;; collect output as string (including printed result)
(define (interp/io e input)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string input)))
      (cons (interp e)
            (get-output-string (current-output-port)))))
