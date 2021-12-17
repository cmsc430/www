#lang racket
(provide (all-defined-out))

(define rax 'rax) ; return
(define eax 'eax) ; 32-bit load/store
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg1
(define rsi 'rsi) ; arg2
(define rdx 'rdx) ; arg3
(define r8  'r8)  ; scratch
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch
(define r12 'r12) ; save across call to memcpy
(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack
