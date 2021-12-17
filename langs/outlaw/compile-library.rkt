#lang racket
(require "parse.rkt" "compile.rkt" "read-all.rkt" "a86/printer.rkt")

;; Compile contents of stdin
;; emit asm code on stdout
(begin
  (void (read-line)) ; ignore #lang racket line
  (display (asm-string (compile-library (parse-library (read-all))))))
