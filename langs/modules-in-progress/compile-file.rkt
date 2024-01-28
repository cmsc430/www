#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" a86/printer)

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (displayln (asm-string (compile-module (parse-module-file fn)))))
;(parse-module-file (simplify-path (path->complete-path fn))))
