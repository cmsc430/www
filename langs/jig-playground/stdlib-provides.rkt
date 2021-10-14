#lang racket
(provide stdlib-provides)
(require "parse.rkt" "ast.rkt")

(define stdlib-provides
  (Module-ps (parse-module-file "stdlib.rkt")))

