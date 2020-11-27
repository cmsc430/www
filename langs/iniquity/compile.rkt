#lang racket
(provide (all-defined-out))
(require "ast.rkt" "asm/ast.rkt" "types.rkt")

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and tag with pointer types.

;; type CEnv = (Listof (Maybe Id))

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (seq (compile-entry e)
          (compile-defines ds))]))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))
  
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs))
          (Ret))]))

;; Expr -> Asm
;; Compile e as the entry point
(define (compile-entry e)
  (seq (Label 'entry)
       (compile-e e '())
       (Ret)
       (Label 'err)
       (Push 'rbp)
       (Call 'error)
       (Ret)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(App f es)      (compile-app f es c)]
    [(Int i)         (compile-integer i)]
    [(Bool b)        (compile-boolean b)]
    [(Empty)         (compile-empty)]
    [(Prim1 p e)     (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    [(If e1 e2 e3)   (compile-if e1 e2 e3 c)] 
    [(Var x)         (compile-var x c)]
    [(Let x e1 e2)   (compile-let x e1 e2 c)]))

;; Id [Listof Expr] CEnv -> Asm
(define (compile-app f es c)
  (let ((stack-size (* 8 (length c))))
    (seq (compile-es es (cons #f c))
         (Sub 'rsp stack-size)
         (Call (symbol->label f))
         (Add 'rsp stack-size))))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Mov (Offset 'rsp (- (add1 (length c)))) 'rax)
          (compile-es es (cons #f c)))]))

;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (arithmetic-shift i int-shift))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (if b val-true val-false))))

;; -> Asm
(define (compile-empty)
  (seq (Mov 'rax val-empty)))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['box
     (seq (Mov (Offset 'rdi 0) 'rax)
          (Mov 'rax 'rdi)
          (Or 'rax box-type-tag)
          (Add 'rdi 8))]
    ['unbox
     (seq (assert-box 'rax)
          (Xor 'rax box-type-tag)
          (Mov 'rax (Offset 'rax 0)))]
    ['car
     (seq (assert-pair 'rax)
          (Xor 'rax pair-type-tag)
          (Mov 'rax (Offset 'rax 1)))]
    ['cdr
     (seq (assert-pair 'rax)
          (Xor 'rax pair-type-tag)
          (Mov 'rax (Offset 'rax 0)))]
    ['add1
     (seq (assert-integer 'rax)
          (Add 'rax (arithmetic-shift 1 int-shift)))]
    ['sub1
     (seq (assert-integer 'rax)
          (Sub 'rax (arithmetic-shift 1 int-shift)))]
    ['zero?
     (seq (assert-integer 'rax)
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1))))]
    ['empty?
     (seq (let ((l1 (gensym 'nempty)))
            (seq (Cmp 'rax val-empty)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1))))]))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (let ((i (- (add1 (length c)))))
    (seq (compile-e e1 c)
         (Mov (Offset 'rsp i) 'rax)
         (compile-e e2 (cons #f c))
         (compile-op2 p i))))

;; e1's value is [rsp + i]
;; e2's value is in rax
(define (compile-op2 p i)
  (match p
    ['+
     (seq (assert-integer (Offset 'rsp i))
          (assert-integer 'rax)   
          (Add 'rax (Offset 'rsp i)))]
    ['-
     (seq (assert-integer (Offset 'rsp i))
          (assert-integer 'rax)   
          (Sub (Offset 'rsp i) 'rax)
          (Mov 'rax (Offset 'rsp i)))]
    ['cons
     (seq (Mov (Offset 'rdi 0) 'rax)
          (Mov 'rax (Offset 'rsp i))
          (Mov (Offset 'rdi 1) 'rax)
          (Mov 'rax 'rdi)
          (Or 'rax pair-type-tag)
          (Add 'rdi 16))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Id CEnv -> Asm
(define (compile-var x c)
  (let ((pos (lookup x c)))
    (seq (Mov 'rax (Offset 'rsp (- (add1 pos)))))))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Mov (Offset 'rsp (- (add1 (length c)))) 'rax)
       (compile-e e2 (cons x c))))

;; Id CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t (length rest)]
       [#f (lookup x rest)])]))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov 'rbx arg)
         (And 'rbx mask)
         (Cmp 'rbx type)
         (Jne 'err))))

(define assert-integer (assert-type int-type-mask int-type-tag))
(define assert-box     (assert-type imm-type-mask box-type-tag))
(define assert-pair    (assert-type imm-type-mask pair-type-tag))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

