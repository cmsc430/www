#lang racket
(provide interp/io)
(require "interp.rkt")
;; String Prog -> (Cons Value String)
;; Interpret p with given string as input,
;; return value and collected output as string
(define (interp/io p input)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string input)))
      (cons (interp p)
            (get-output-string (current-output-port)))))

