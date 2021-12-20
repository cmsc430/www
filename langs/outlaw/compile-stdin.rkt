#lang racket
(require "stdlib.rkt" "parse.rkt" "compile.rkt" "read-all.rkt" "a86/printer.rkt")
(provide main)

;; -> Void
;; Compile contents of stdin
;; emit asm code on stdout
(define (main)
  (begin
    (read-line) ; ignore #lang racket line
    (current-shared? #t)
    (displayln (asm-string (compile (parse (read-all)))))))
