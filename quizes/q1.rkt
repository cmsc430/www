#lang racket
(require a86/ast a86/printer a86/interp)

(define prog
 (list (Label 'entry)
       (Mov 'rax 21)  
       (Add 'rax 'rax)
       (Ret)))

(display (asm-string prog))
