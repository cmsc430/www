#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (if (integer? s)
      (Int s)
      (error "Parse error")))