#lang racket
(provide (all-defined-out))
(require "compile.rkt" "asm/printer.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((c (read-line)) ; ignore #lang racket line
            (p (read)))
        (unless (integer? p) (error "syntax error" p))
        (asm-display (abscond-compile p))))))
