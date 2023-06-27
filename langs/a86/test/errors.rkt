#lang racket
(require rackunit "../ast.rkt")
(check-exn exn:fail?
           (thunk (Mov (Offset 'rax 0) 100)))

;; Checking literal widths
(check-exn exn:fail? (thunk (Mov 'rax (expt 2 64))))
(check-not-exn       (thunk (Mov 'rax (sub1 (expt 2 64)))))
(check-exn exn:fail? (thunk (Cmp 'rax (expt 2 32))))
(check-not-exn       (thunk (Cmp 'rax (sub1 (expt 2 32)))))
(check-exn exn:fail? (thunk (And 'rax (expt 2 32))))
(check-not-exn       (thunk (And 'rax (sub1 (expt 2 32)))))
(check-exn exn:fail? (thunk (Or 'rax (expt 2 32))))
(check-not-exn       (thunk (Or 'rax (sub1 (expt 2 32)))))
(check-exn exn:fail? (thunk (Xor 'rax (expt 2 32))))
(check-not-exn       (thunk (Xor 'rax (sub1 (expt 2 32)))))
(check-exn exn:fail? (thunk (Push (expt 2 32))))
(check-not-exn       (thunk (Push (sub1 (expt 2 32)))))
(check-exn exn:fail? (thunk (Add 'rax (expt 2 32))))
(check-not-exn       (thunk (Add 'rax (sub1 (expt 2 32)))))
(check-exn exn:fail? (thunk (Sub 'rax (expt 2 32))))
(check-not-exn       (thunk (Sub 'rax (sub1 (expt 2 32)))))

;; Check prog
(check-exn exn:fail? (thunk (prog (Ret))))
(check-exn exn:fail? (thunk (prog (Label 'start) (Ret))))
(check-exn exn:fail? (thunk (prog (Global 'foo) (Label 'start) (Label 'foo) (Ret))))
(check-not-exn       (thunk (prog (Global 'start) (Label 'start) (Ret))))
(check-not-exn       (thunk (prog (Label 'start) (Ret) (Global 'start))))


