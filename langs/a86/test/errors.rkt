#lang racket
(require rackunit "../ast.rkt")
(check-exn exn:fail?
           (thunk (Mov (Offset 'rax 0) 100)))
