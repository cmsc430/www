#lang racket
(provide (all-defined-out))

;; An immediate is anything ending in #b000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)

(define imm-shift        (+ 3 result-shift))
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     (arithmetic-shift #b000 result-shift))
(define imm-val-true    (arithmetic-shift #b001 result-shift))
(define imm-val-false   (arithmetic-shift #b010 result-shift))
(define imm-val-empty   (arithmetic-shift #b011 result-shift))

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

;; type CEnv = (Listof (Maybe Variable))
;; type Imm = Integer | Boolean | '()

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp)
    (call error)
    ret))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? imm? i)            (compile-imm i)]
    [(? symbol? x)         (compile-var x c)]    
    [`(box ,e0)            (compile-box e0 c)]
    [`(unbox ,e0)          (compile-unbox e0 c)]
    [`(cons ,e0 ,e1)       (compile-cons e0 e1 c)]
    [`(car ,e0)            (compile-car e0 c)]
    [`(cdr ,e0)            (compile-cdr e0 c)]
    [`(add1 ,e0)           (compile-add1 e0 c)]
    [`(sub1 ,e0)           (compile-sub1 e0 c)]
    [`(zero? ,e0)          (compile-zero? e0 c)]
    [`(map-zero? ,e0)      (compile-map-zero? e0 c)]
    [`(if ,e0 ,e1 ,e2)     (compile-if e0 e1 e2 c)]
    [`(let ((,x ,e0)) ,e1) (compile-let x e0 e1 c)]
    [`(+ ,e0 ,e1)          (compile-+ e0 e1 c)]
    [`(- ,e0 ,e1)          (compile-- e0 e1 c)]))

;; Any -> Boolean
(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (equal? ''() x)))

;; Imm -> Asm
(define (compile-imm i)
  `((mov rax
         ,(match i
            [(? integer? i) (arithmetic-shift i imm-shift)]
            [(? boolean? b) (if b imm-val-true imm-val-false)]
            [''()           imm-val-empty]))))

;; Variable CEnv -> Asm
(define (compile-var x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

;; Expr CEnv -> Asm
(define (compile-box e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-box)
      (add rdi 8)))) ; allocate 8 bytes

;; Expr CEnv -> Asm
(define (compile-unbox e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

;; Expr Expr CEnv -> Asm
(define (compile-cons e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1
      (mov (offset rdi 0) rax)
      (mov rax (offset rsp ,(- (add1 (length c)))))
      (mov (offset rdi 1) rax)
      (mov rax rdi)
      (or rax ,type-pair)
      (add rdi 16))))

;; Expr CEnv -> Asm
(define (compile-car e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 1)))))

(define (compile-map-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (lend (gensym "end"))
        (lz1 (gensym "nz"))
        (lz2 (gensym "z"))
        (lempty (gensym "empty"))
        (loop (gensym "loop_start")))
   `(,@c0

     (cmp rax ,imm-val-empty) ; if the input list is empty, we're done
     (je ,lend)
     ,@assert-pair            ; otherwise, it should be a pair (cons)

     (mov (offset rsp ,(- (add1 (length c)))) rdi) ; store initial heap pointer

     ,loop                    ; Start of our loop

     (xor rax ,type-pair)     ; untag the pair

     (mov rcx rax)            ; store pointer in rcx

     (mov rax (offset rax 1)) ; check that `car` is an integer
     ,@assert-integer
     ,@(zero?-asm lz1 lz2)       ; rax is zero?, leave the bool in rax

     (mov (offset rdi 1) rax) ; put the boolean in the car of the new list cell

     (mov rax (offset rcx 0)) ; move cdr of input into rax 
     
     (cmp rax ,imm-val-empty) ; if the cdr is the empty list, finish up
     (je ,lempty)

     ,@assert-pair            ; if it's not empty, it better be a pair

     (mov rdx rdi)            ; move a copy of the heap pointer into rdx
     (add rdx 16)             ; figure out where the next cons is going to be
     (or rdx ,type-pair)      ; tag the next cons as a pair
     (mov (offset rdi 0) rdx) ; the cdr of our new cons is where the next one goes

     (add rdi 16)             ; bump the heap pointer

                              ; since rax wasn't the empty list
                              ; it's a pointer to out next cons
     (jmp ,loop)              ; iterate 

     ,lempty
     (mov rax ,imm-val-empty)
     (mov (offset rdi 0) rax)
     (mov rax rdi)
     (add rdi 16)

     (mov rax (offset rsp ,(- (add1 (length c))))) ; Get initial heap pointer back
     (or rax ,type-pair)      ; tag the initial thing as a pair
     ,lend
     
    )))

(define (zero?-asm l0 l1)
    `(,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0))

;; Expr CEnv -> Asm
(define (compile-cdr e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 0)))))   

;; Expr CEnv -> Asm
(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@(zero?-asm l0 l1))))


;; Expr Expr Expr CEnv -> Asm
(define (compile-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c))
        (c2 (compile-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax ,imm-val-false)
      (je ,l0)
      ,@c1
      (jmp ,l1)
      ,l0
      ,@c2
      ,l1)))

;; Variable Expr Expr CEnv -> Asm
(define (compile-let x e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons x c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1)))

;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c) 
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

;; Expr Expr CEnv -> Asm
(define (compile-- e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (sub rax (offset rsp ,(- (add1 (length c))))))))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

(define (assert-type mask type)
  `((mov rbx rax)
    (and rbx ,mask)
    (cmp rbx ,type)
    (jne err)))

(define assert-integer (assert-type imm-type-mask imm-type-int))
(define assert-box     (assert-type result-type-mask type-box))
(define assert-pair    (assert-type result-type-mask type-pair))
