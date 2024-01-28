#lang racket
(provide interp/io)
(require "interp.rkt")

;; String Expr -> (Cons Value String)
;; Interpret e with given string as input,
;; return value and collected output as string
(define (interp/io e input)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string input)))
      (cons (interp e)
            (get-output-string (current-output-port)))))

