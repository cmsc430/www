#lang racket
(provide compile-datum)
(require "types.rkt"         
         a86/ast)

;; Registers used
(define rax 'rax) ; return

(define (compound? x)
  (or (string? x)
      (cons? x)
      (vector? x)
      (box? x)))

;; Datum -> Asm
(define (compile-datum d)
  (if (compound? d)
      (compile-compound-datum d)
      (seq (Mov rax (imm->bits d)))))

(define (compile-compound-datum d)
  (match (compile-quoted d)
    [(cons l is)
     (seq (Data)
          is
          (Text)
          (Lea rax l))]))

;; Datum -> (cons AsmExpr Asm)
(define (compile-quoted c)
  (cond
    [(string? c) (compile-datum-string c)]
    [(vector? c) (compile-datum-vector (vector->list c))]
    [(box? c)    (compile-datum-box (unbox c))]
    [(cons? c)   (compile-datum-cons (car c) (cdr c))]
    [else        (cons (imm->bits c) '())]))

;; String -> (cons AsmExpr Asm)
(define (compile-datum-string c)
  (let ((l (gensym 'string)))
    (cons (Plus l type-str)
          (seq (Label l)
               (Dq (string-length c))
               (compile-string-chars (string->list c))
               (if (odd? (string-length c))
                   (seq (Dd 0))
                   (seq))))))

;; [Listof Datum] -> (cons AsmExpr Asm)
(define (compile-datum-vector ds)
  (match ds
    ['() (cons type-vect '())]
    [_
     (let ((l (gensym 'vector))
           (cds (map compile-quoted ds)))
       (cons (Plus l type-vect)
             (seq (Label l)
                  (Dq (length ds))
                  (map (λ (cd) (Dq (car cd))) cds)
                  (append-map cdr cds))))]))

;; Datum -> (cons AsmExpr Asm)
(define (compile-datum-box c)
  (match (compile-quoted c)
    [(cons l1 is1)
     (let ((l (gensym 'box)))
       (cons (Plus l type-box)
             (seq (Label l)
                  (Dq l1)
                  is1)))]))

;; Datum Datum -> (cons AsmExpr Asm)
(define (compile-datum-cons c1 c2)
  (match (compile-quoted c1)
    [(cons l1 is1)
     (match (compile-quoted c2)
       [(cons l2 is2)
        (let ((l (gensym 'cons)))
          (cons (Plus l type-cons)
                (seq (Label l)
                     (Dq l2)
                     (Dq l1)
                     is1
                     is2)))])]))

;; [Listof Char] -> Asm
(define (compile-string-chars cs)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Dd (char->integer c))
          (compile-string-chars cs))]))
