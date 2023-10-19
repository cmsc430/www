#lang crook
{:= A B C D0 D0.A D1 E0 E1 F H0 H1 I J K}
(require "../interp.rkt")
{:> E0} (require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
 
{:> A H0}
(test (λ (e) (interp (parse e))))
{:> I}
(test (λ p (interp (apply parse p))))

{:> E0 H0}
(test/io (λ (in e) (interp/io (parse e) in)))
{:> I}
(test/io (λ (in . p) (interp/io (apply parse p) in)))
