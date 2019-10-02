#lang racket
(provide (all-defined-out))

;; An immediate is anything ending in #b000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)
(define type-string      #b011)

(define imm-shift        (+ 2 result-shift))
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     (arithmetic-shift #b00 result-shift))
(define imm-type-bool    (arithmetic-shift #b01 result-shift))
(define imm-type-char    (arithmetic-shift #b10 result-shift))
(define imm-type-empty   (arithmetic-shift #b11 result-shift))

(define imm-val-false    imm-type-bool)
(define imm-val-true     (bitwise-ior (arithmetic-shift 1 (add1 imm-shift)) imm-type-bool))

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

;; type CEnv = (Listof (Maybe Variable))
;; type LEnv = (Listof (List Variable Label))
;; type Imm = Integer | Boolean | Char | ''()

;; Expr LEnv -> Asm
(define (compile-l e l)
  `(entry
    ,@(compile-e e '() l)
    ret

    #|
    ;; Integer -> Integer
    ;; Add seven to argument passed on stack
    plusseven
    (mov rax (offset rsp -1))
    (add rax ,(arithmetic-shift 7 imm-shift))
    ret
    |#

    err
    (push rbp)
    (call error)
    ret))


;; Expr CEnv LEnv -> Asm
(define (compile-e e c l)
  (match e
    [(? symbol? x)         (compile-variable x c)]
    [(? imm? i)            (compile-imm i)]
    [`(box ,e0)            (compile-box e0 c l)]
    [`(unbox ,e0)          (compile-unbox e0 c l)]
    [`(cons ,e0 ,e1)       (compile-cons e0 e1 c l)]
    [`(car ,e0)            (compile-car e0 c l)]
    [`(cdr ,e0)            (compile-cdr e0 c l)]
    [`(add1 ,e0)           (compile-add1 e0 c l)]
    [`(sub1 ,e0)           (compile-sub1 e0 c l)]
    [`(zero? ,e0)          (compile-zero? e0 c l)]
    [`(empty? ,e0)         (compile-empty? e0 c l)]
    [`(if ,e0 ,e1 ,e2)     (compile-if e0 e1 e2 c l)]
    [`(+ ,e0 ,e1)          (compile-+ e0 e1 c l)]
    [`(,(? (label-var? l) f) . ,es)
     (compile-call-label f es c l)]))


(define (compile-plusseven e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      (sub rsp ,(* 8 (length c)))
      (mov (offset rsp -2) rax)
      (call plusseven)
      (add rsp ,(* 8 (length c))))))

#;
(define (compile-call-label f e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      (sub rsp ,(* 8 (length c)))
      (mov (offset rsp -2) rax)
      (call ,(label-env-lookup l f))
      (add rsp ,(* 8 (length c))))))









'(begin (define (addseven x)
          (+ x 7))

        (addseven 3))

    


;; Prog1 -> Asm
;; Compile program with one unary function definition
(define (compile1 p)
  (match p
    [(list 'begin `(define (,f ,x) ,e) e0)
     (let ((l (list (list f (gensym)))))
       (let ((ds (compile-define1 f x e l))
             (c0 (compile-l e0 l)))
         `(,@c0
           ,@ds)))]))




;; Variable Variable Expr LEnv -> Asm
(define (compile-define1 f x e0 l)
  (let ((c0 (compile-e e0 (list x) l)))
    `(,(label-env-lookup l f)
      ,@c0
      ret)))







;; type Prog =
;; | Expr
;; | `(begin ,@(Listof (define (,Variable ,@(Listof Variable)) ,Expr))
;;           ,Expr)           

#|
    ;; Label calls
    [(list* (? (label-var? l) f) es)
     (compile-call-label f es c l)]))
|#

;; LEnv -> (Any -> Boolean)
(define (label-var? l)
  (lambda (x)
    (and (symbol? x)
         (assq x l))))








;; Prog -> Asm
(define (compile p)
  (match p
    [(list 'begin `(define (,fs . ,xss) ,es) ... e0)
     (let ((l (make-label-env fs)))
       (let ((ds (compile-defines fs xss es l))
             (c0 (compile-l e0 l)))
         `(,@c0
           ,@ds)))]
    [e (compile-l e '())]))


;; Label (Listof Expr) CEnv LEnv -> Asm
;; Statically know the label we're calling
(define (compile-call-label f es c l)
  (let ((cs (compile-es es (cons #f c) l))       
        (stack-size (* 8 (length c))))
    `(,@cs
      (sub rsp ,stack-size)
      (call ,(label-env-lookup l f))
      (add rsp ,stack-size))))

;; (Listof Expr) CEnv LEnv -> Asm
(define (compile-es es c l)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c l))
           (cs (compile-es es (cons #f c) l)))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@cs))]))


;; Variable (Listof Variable) Expr LEnv -> Asm
(define (compile-define f xs e0 l)
  (let ((c0 (compile-e e0 (reverse xs) l))
        (label (label-env-lookup l f)))
    `(,label
      ,@c0
      ret)))

;; (Listof Variable) (Listof (Listof Variable)) (Listof Expr) LEnv -> Asm
(define (compile-defines fs xss es l)
  (apply append
         (map (lambda (f xs e) (compile-define f xs e l)) fs xss es)))

;; (Listof Label) -> LEnv
(define (make-label-env fs)
  (map (lambda (f) (list f (gensym))) fs))




;; Any -> Boolean
(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (equal? ''() x)))

;; Any -> Boolean
(define (type-pred? x)
  (memq x '(integer?
            char?
            empty?)))

;; Imm -> Asm
(define (compile-imm i)
  `((mov rax ,(imm->bits i))))

;; Imm -> Integer
(define (imm->bits i)
  (match i
    [(? integer? i) (arithmetic-shift i imm-shift)]
    [(? char? c)    (+ (arithmetic-shift (char->integer c) imm-shift) imm-type-char)]
    [(? boolean? b) (if b imm-val-true imm-val-false)]
    [''()           imm-type-empty]))

;; Variable CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

;; Expr CEnv LEnv -> Asm
(define (compile-box e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-box)
      (add rdi 8)))) ; allocate 8 bytes

;; Expr CEnv LEnv -> Asm
(define (compile-unbox e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

;; Expr Expr CEnv LEnv -> Asm
(define (compile-cons e0 e1 c l)
  (let ((c0 (compile-e e0 c l))
        (c1 (compile-e e1 (cons #f c) l)))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1
      (mov (offset rdi 0) rax)
      (mov rax (offset rsp ,(- (add1 (length c)))))
      (mov (offset rdi 1) rax)
      (mov rax rdi)
      (or rax ,type-pair)
      (add rdi 16))))

;; Expr CEnv LEnv -> Asm
(define (compile-car e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair)
      (mov rax (offset rax 1)))))

;; Expr CEnv LEnv -> Asm
(define (compile-cdr e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair)
      (mov rax (offset rax 0)))))

;; Expr CEnv LEnv -> Asm
(define (compile-empty? e0 c l)
  (let ((c0 (compile-e e0 c l))
        (l0 (gensym)))
    `(,@c0
      (and rax ,imm-type-mask)
      (cmp rax ,imm-type-empty)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; Expr CEnv LEnv -> Asm
(define (compile-add1 e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv LEnv -> Asm
(define (compile-sub1 e0 c l)
  (let ((c0 (compile-e e0 c l)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv LEnv -> Asm
(define (compile-zero? e0 c l)
  (let ((c0 (compile-e e0 c l))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; Expr Expr Expr CEnv LEnv -> Asm
(define (compile-if e0 e1 e2 c l)
  (let ((c0 (compile-e e0 c l))
        (c1 (compile-e e1 c l))
        (c2 (compile-e e2 c l))
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

;; Expr Expr CEnv LEnv -> Asm
(define (compile-+ e0 e1 c l)
  (let ((c1 (compile-e e1 c l))
        (c0 (compile-e e0 (cons #f c) l)))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))


;; SOLN
(define (type-pred->mask p)
  (match p
    [(or 'box? 'cons? 'string?) result-type-mask]
    [_ imm-type-mask]))

;; SOLN
(define (type-pred->tag p)
  (match p
    ['box?     type-box]
    ['cons?    type-pair]
    ['string?  type-string]
    ['integer? imm-type-int]
    ['empty?   imm-type-empty]
    ['char?    imm-type-char]
    ['boolean? imm-type-bool]))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

(define (label-env-lookup l f)
  (second (assq f l)))

(define (assert-type p)
  `((mov rbx rax)
    (and rbx ,(type-pred->mask p))
    (cmp rbx ,(type-pred->tag p))
    (jne err)))

(define assert-integer (assert-type 'integer?))
(define assert-box     (assert-type 'box?))
(define assert-pair    (assert-type 'cons?))
(define assert-string  (assert-type 'string?))
(define assert-char    (assert-type 'char?))

;; Asm
(define assert-natural
  `(,@assert-integer
    (cmp rax -1)
    (jle err)))

;; Asm
(define assert-integer-codepoint
  `((mov rbx rax)
    (and rbx ,imm-type-mask)
    (cmp rbx 0)
    (jne err)
    (cmp rax ,(arithmetic-shift -1 imm-shift))
    (jle err)
    (cmp rax ,(arithmetic-shift #x10FFFF imm-shift))
    (mov rbx rax)
    (sar rbx ,(+ 11 imm-shift))
    (cmp rbx #b11011)
    (je err)))


#|
;; TAIL CALLS

;; Expr CEnv LEnv -> Asm
;; Compile e assuming it is in tail position
(define (compile-tail-e e c l)
  (match e
    [(list* (? (label-var? l) f) es)
     (compile-tail-call-label f es c l)]
    [else
     (compile-e e c l)]))

;; Label (Listof Expr) CEnv LEnv -> Asm
(define (compile-tail-call-label label es c l)
  (let ((cs (compile-es es c l)))
    `(,@cs
      ,(gensym 'begin_stack_move)
      ,@(stack-move (add1 (length c)) (length es))
      ,(gensym 'end_stack_move)
      ,(gensym 'tail_call)
      (jmp ,(label-env-lookup l label)))))

(define (stack-move start count)
  (define (stack-move i)
    (if (= i count)
        '()
        `((mov rax (offset rsp ,(- (+ start i))))
          (mov (offset rsp ,(- (add1 i))) rax)
          ,@(stack-move (add1 i)))))
  (stack-move 0))
|#

