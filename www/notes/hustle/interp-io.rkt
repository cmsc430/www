#lang racket
(provide make-interp/io)

;; (Expr -> Answer) -> (Expr String -> String)
;; Interpret e with given string as input,
;; collect output as string (including printed result)
(define (make-interp/io interp)
  (λ (e in)
    (with-output-to-string
      (λ ()
        (with-input-from-string in
          (λ ()
            (let ((r (interp e)))
              (if (void? r)
                  r
                  (displayln r)))))))))
