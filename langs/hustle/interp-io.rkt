#lang racket
(provide make-interp/io)

;; (Expr -> Answer) -> (Expr String -> String)
;; Interpret e with given string as input,
;; collect output as string (including printed result)
(define (make-interp/io interp)
  (λ (e in)
    (parameterize ((current-output-port (open-output-string))
                   (current-input-port  (open-input-string in)))
      (cons (interp e)
            (get-output-string (current-output-port))))))
