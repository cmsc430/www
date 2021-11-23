#lang racket
(provide compile-datum)
(require "types.rkt"
         "utils.rkt"
         a86/ast)

;; Registers used
(define rax 'rax) ; return

;; Datum -> Asm
(define (compile-datum d)
  (cond
    [(string? d)   (seq (Lea rax (load-string d)))]
    [(symbol? d)   (seq (Lea rax (load-symbol d)))]
    [(compound? d) (compile-compound-datum d)]
    [else          (compile-atom d)]))

(define (load-symbol s)
  (Plus (symbol->data-label s) type-symb))

(define (load-string s)
  (Plus (symbol->data-label (string->symbol s)) type-str))

;; Value -> Asm
(define (compile-atom v)
  (seq (Mov rax (imm->bits v))))

;; Datum -> Boolean
(define (compound? d)
  (or (box? d)
      (cons? d)
      (vector? d)))

;; Datum -> Asm
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
    [(vector? c) (compile-datum-vector (vector->list c))]
    [(box? c)    (compile-datum-box (unbox c))]
    [(cons? c)   (compile-datum-cons (car c) (cdr c))]
    [(symbol? c) (cons (load-symbol c) '())]
    [(string? c) (cons (load-string c) '())]
    [else        (cons (imm->bits c) '())]))

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
