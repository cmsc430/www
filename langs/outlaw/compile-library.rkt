#lang racket
(require "parse.rkt" "compile.rkt" "read-all.rkt" "a86/printer.rkt")
(provide main)

;; Compile contents of stdin
;; emit asm code on stdout
(define (main)
  (begin
    (read-line) ; ignore #lang racket line
    (current-shared? #t)
    (asm-display (compile-library (parse-library (read-all))))))
