#lang crook
{:= A B C D0 D1 E0 E1 F H0}
(require "../interp.rkt")
{:> E0} (require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
 
(test (λ (e) (interp (parse e))))
{:> E0}
(test/io (λ (e in) (interp/io (parse e) in)))

