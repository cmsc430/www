#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "fv.rkt"
         "utils.rkt"
         "compile-expr.rkt"
         a86/ast)

(define r9 'r9)
(define r15 'r15)

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds)
  (match ds
    ['() '()]
    [(cons (Defn f l) ds)
     (cons f (define-ids ds))]))

;; [Listof Defn] GEnv -> Asm
(define (compile-defines ds g)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d g)
          (compile-defines ds g))]))

;; Defn GEnv -> Asm
(define (compile-define d g)
  (match d
    [(Defn f e)
     (seq (%%% (symbol->string f))
          (Data)
          (Label (symbol->label f))
          (Dq 0)
          (Text)
          (compile-e e '() g #f)
          (Mov (Offset (symbol->label f) 0) rax))]))

;; [Listof Lam] GEnv -> Asm
(define (compile-lambda-defines ls g)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l g)
          (compile-lambda-defines ls g))]))

;; Lambda GEnv -> Asm
(define (compile-lambda-define l g)
  (let ((fvs (fv- l g)))    
    (match l
      [(Lam f xs e)
       (let ((env (append (reverse fvs) (reverse xs) (list #f))))
         (seq (Label (symbol->label f))
              (Cmp r15 (length xs))
              (Jne 'raise_error_align)              
              (Mov rax (Offset rsp (* 8 (length xs))))
              (Xor rax type-proc)
              (copy-env-to-stack fvs 8)              
              (compile-e e env g #t)
              (Add rsp (* 8 (length env))) ; pop env
              (Ret)))]
      [(LamRest f xs x e)
       (let ((env (append (reverse fvs) (cons x (reverse xs)) (list #f))))
         (seq (Label (symbol->label f))
              (Cmp r15 (length xs))
              (Jl 'raise_error_align)
              
              (Sub r15 (length xs))
              (Mov rax val-empty)
              (let ((loop (gensym))
                    (done (gensym)))
                (seq (Label loop)
                     (Cmp r15 0)
                     (Je done)
                     (Mov (Offset rbx 0) rax)
                     (Pop rax)
                     (Mov (Offset rbx 8) rax)
                     (Mov rax rbx)
                     (Or rax type-cons)
                     (Add rbx 16)
                     (Sub r15 1)
                     (Jmp loop)
                     (Label done)))
              (Push rax)
              
              (Mov rax (Offset rsp (* 8 (add1 (length xs)))))
              (Xor rax type-proc)
              (copy-env-to-stack fvs 8)              
              (compile-e e env g #t)
              (Add rsp (* 8 (length env))) ; pop env
              (Ret)))]
    [(LamCase f cs)
     (seq (%%% "lamcase code")
          (Label (symbol->label f))
          (compile-fun-case-select cs)
          (Jmp 'raise_error_align)
          (compile-fun-case-clauses cs g))])))

(define (compile-fun-case-clauses cs g)
  (append-map (lambda (c) (compile-lambda-define c g)) cs))

(define (compile-fun-case-select cs)
  (append-map compile-fun-case-selector cs))

(define (compile-fun-case-selector c)
  (match c
    [(Lam f xs e)
     (seq (Cmp r15 (length xs))
          (Je (symbol->label f)))]
    [(LamRest f xs x e)
     (seq (Mov r9 (sub1 (length xs)))
          (Cmp r9 r15)
          (Jl (symbol->label f)))]))
