#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "asm/printer.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((_ (read-line))) ; ignore #lang racket line
        (displayln (asm-string (compile (parse (read)))))))))
