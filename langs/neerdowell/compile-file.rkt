#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "read-all.rkt" a86/printer)

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (asm-display (compile (parse (read-all p))))
      (close-input-port p))))
