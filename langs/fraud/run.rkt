#lang racket
(require a86/interp)
(require "types.rkt")
(require "build-runtime.rkt")
(provide run run/io)
;; Asm -> Answer
(define (run is)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp is)
      ['err 'err]
      [b (bits->value b)])))
;; Asm String -> (cons Answer String)
(define (run/io is in)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp/io is in)
      [(cons 'err out) (cons 'err out)]
      [(cons b out)
       (cons (bits->value b) out)])))

