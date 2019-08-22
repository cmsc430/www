#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret))


(define true-rep  #b10011111)
(define false-rep #b00011111)
(define fixnum-shift 2)

;; ANF CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? integer? i) `((mov rax ,(arithmetic-shift i fixnum-shift)))]
    [#t `((mov rax ,true-rep))]
    [#f `((mov rax ,false-rep))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         (add rax ,(arithmetic-shift 1 fixnum-shift))))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         (sub rax ,(arithmetic-shift 1 fixnum-shift ))))]

    ;; FIXME
    [`(zero? ,e0) '...]    
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c)))


       ;; FIXME
       (match (gen-if-labels)
         [(list if-t if-f if-x)       
          `(,@c0
            (cmp rax 0)
            (jne ,if-f)
            ,if-t
            ,@c1
            (jmp ,if-x)
            ,if-f
            ,@c2
            ,if-x)]))]))

;; -> [List Label Label]
;; Guaranteed to be unique on each call
(define gen-if-labels
  (let ((i 0))
    (λ ()
      (set! i (add1 i))
      (list (lab "f" i)
            (lab "x" i)))))

;; String Integer -> Symbol
(define (lab s i)
  (string->symbol (string-append "if_" s "_" (number->string i))))
