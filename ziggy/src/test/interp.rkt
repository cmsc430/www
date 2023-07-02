#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1 I J}
(require "../interp.rkt")
{:> E0} (require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
 
{:> A I}
(test (λ (e) (interp (parse e))))
{:> I}
(test (λ p (interp (apply parse p))))

{:> E0 I}
(test/io (λ (in e) (interp/io (parse e) in)))
{:> I}
(test/io (λ (in . p) (interp/io (apply parse p) in)))
