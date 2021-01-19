#lang racket
(provide interp/io)
(require "interp.rkt")

;; Expr String -> (Cons Value String)
;; Interpret e with given string as input,
;; collect output as string (including printed result)
(define (interp/io e input)
  (let ((out (open-output-string))
        (in  (open-input-string input)))
      (cons (parameterize ((current-output-port out)
                           (current-input-port in))
              (interp e))
            (get-output-string out))))
