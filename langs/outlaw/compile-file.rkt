#lang racket
(provide main)
(require "parse.rkt"
         "compile.rkt"
         "read-all.rkt"
         "a86/printer.rkt")

;; -> Void
;; Compile contents of stdin
;; emit asm code on stdout
(define (main)
  (begin
    (read-line) ; ignore #lang racket line
    (displayln (asm-string (compile (parse (read-all)))))))
