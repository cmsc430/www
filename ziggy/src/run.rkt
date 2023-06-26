#lang crook
{:= A B C D0 D1 E0 E1 F}
(require a86/interp)
{:> D0} (require "types.rkt")
{:> E0} (require "build-runtime.rkt")
(provide run {:> E0} run/io)

{:> A D0}  ;; Asm -> Integer
{:> D0 E1} ;; Asm -> Value
{:> E1}    ;; Asm -> Answer
(define (run is)
  {:> A D0}
  (asm-interp is)
  {:> D0 E1}
  (bits->value (asm-interp is))
  {:> E0}
  (parameterize ((current-objs (list (path->string runtime-path))))
    {:> E0 E1}
    (bits->value (asm-interp is))
    {:> E1}
    (match (asm-interp is)
      ['err 'err]
      [b (bits->value b)])))

{:> E0} ;; Asm String -> (cons Answer String)
{:> E0}
(define (run/io is in)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp/io is in)
      {:> E1}
      [(cons 'err out) (cons 'err out)]
      [(cons b out)
       (cons (bits->value b) out)])))
