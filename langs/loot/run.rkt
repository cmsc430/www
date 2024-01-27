#lang racket
(provide run run/io)
(require "types.rkt" "build-runtime.rkt"
         a86/interp)

;; Asm -> Answer
(define (run is)
  (parameterize ((current-objs (list runtime-path)))
    (match (asm-interp is)
      ['err 'err]
      [b (bits->value b)])))

;; Asm String -> (cons Answer String)
(define (run/io is s)
  (parameterize ((current-objs (list runtime-path)))
    (match (asm-interp/io is s)
      [(cons 'err o) (cons 'err o)]
      [(cons b o)    (cons (bits->value b) o)])))
