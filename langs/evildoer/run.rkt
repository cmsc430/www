#lang racket
(require a86/interp)
(require "types.rkt")
(require "build-runtime.rkt")
(provide run run/io)
;; Asm -> Value
(define (run is)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (bits->value (asm-interp is))))

;; Asm String -> (cons Answer String)
(define (run/io is in)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp/io is in)
      [(cons b out)
       (cons (bits->value b) out)])))

