#lang crook
{:= A B C D0 D0.A D1 E0 E1 F H0 H1 I J K}
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")

{:> A H0}
(test (λ (e) (run (compile (parse e)))))
{:> I}
(test (λ p (run (compile (apply parse p)))))

{:> E0 H0}
(test/io (λ (in e) (run/io (compile (parse e)) in)))
{:> I}
(test/io (λ (in . p) (run/io (compile (apply parse p)) in)))
