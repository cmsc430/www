#lang racket
(provide compile-datum compile-literals)
(require "types.rkt"
         "intern.rkt"
         a86/ast)

;; Registers used
(define rax 'rax) ; return

;; QEnv -> Asm
(define (compile-literals q)
  (match q
    ['() (seq)]
    [(cons (cons s (Ref l _)) q)
     (seq (compile-literal (to-string s) l)
          (compile-literals q))]))

;; String Label -> Asm
(define (compile-literal s l)
  (seq (Label l)
       (Dq (string-length s))
       (compile-string-chars (string->list s))
       (if (odd? (string-length s))
           (seq (Dd 0))
           (seq))))

(define (compound? x)
  (or ;(string? x)
      ;(symbol? x)
      (cons? x)
      (vector? x)
      (box? x)))

;; Datum -> Asm
(define (compile-datum d)
  (cond
    [(compound? d)
     (compile-compound-datum d)]
    [(Ref? d)
     (seq (Lea rax (Plus (Ref-label d) (Ref-type-tag d))))]
    [else
     (seq (Mov rax (imm->bits d)))]))

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
    ;[(string? c) (compile-datum-string c)]
    ;[(symbol? c) (compile-datum-symbol (symbol->string c))]
    [(vector? c) (compile-datum-vector (vector->list c))]
    [(box? c)    (compile-datum-box (unbox c))]
    [(cons? c)   (compile-datum-cons (car c) (cdr c))]
    [(Ref? c)    (cons (Plus (Ref-label c) (Ref-type-tag c)) '())]
    [else        (cons (imm->bits c) '())]))

;; String -> (cons AsmExpr Asm)
#;
(define (compile-datum-string c)
  (let ((l (gensym 'string)))
    (cons (Plus l type-str)
          (seq (Label l)
               (Dq (string-length c))
               (compile-string-chars (string->list c))
               (if (odd? (string-length c))
                   (seq (Dd 0))
                   (seq))))))

;; String -> (cons AsmExpr Asm)
#;
(define (compile-datum-symbol c)
  (let ((l (gensym 'symbol)))
    (cons (Plus l type-symb)
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

;; (U String Symbol) -> String
(define (to-string s)
  (if (symbol? s)
      (symbol->string s)
      s))
