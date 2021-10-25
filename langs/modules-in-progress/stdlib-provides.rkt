#lang racket
(provide stdlib-provides)
(require "parse.rkt" "ast.rkt" racket/runtime-path)

(define-runtime-path stdlib "stdlib.rkt")

(define stdlib-provides
  (Module-ps (parse-module-file stdlib)))

