#lang crook
{:= A B C D0 D1 E0 E1 F H0 H1 I}
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")

{:> A I}
(test (λ (e) (run (compile (parse e)))))
{:> I}
(test (λ p (run (compile (apply parse p)))))

{:> E0 I}
(test/io (λ (in e) (run/io (compile (parse e)) in)))
{:> I}
(test/io (λ (in . p) (run/io (compile (apply parse p)) in)))
