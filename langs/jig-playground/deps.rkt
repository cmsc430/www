#lang racket
(provide main)
(require "parse.rkt" "ast.rkt")
(define (main fn)
  (match (parse-module-file fn)
    [(Module ps rs ds)
     (for-each (lambda (r)
                 (begin (display (.o (first r)))
                        (display " ")
                        (main (first r))))
               rs)]))

(define (.o f)
  (string-append (substring f 0 (- (string-length f) 3)) "o"))

                
