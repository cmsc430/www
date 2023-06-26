#lang crook
{:= A B C D0 D1 E0 E1 F}
(require "../compile.rkt")
(require "../parse.rkt")
(require "../run.rkt")
(require "test-runner.rkt")
 
(test (λ (e) (run (compile (parse e)))))
{:> E0}
(test/io (λ (e in) (run/io (compile (parse e)) in)))
