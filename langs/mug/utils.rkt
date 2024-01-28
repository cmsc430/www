#lang racket
(provide symbol->data-label lookup pad-stack unpad-stack)
(require a86/ast)

(define rsp 'rsp)
(define r15 'r15)

(define (symbol->data-label s)
  (symbol->label
   (string->symbol (string-append "data_" (symbol->string s)))))

;; Id CEnv -> [Maybe Integer]
(define (lookup x cenv)
  (match cenv
    ['() #f]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (match (lookup x rest)
             [#f #f]
             [i (+ 8 i)])])]))

;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))
