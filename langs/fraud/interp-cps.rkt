#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '() (λ (v) v)))

;; Expr Env Cont -> Answer
(define (interp-env e r k)
  (match e
    [(Int i)  (k i)]
    [(Bool b) (k b)]
    [(Char c) (k c)]
    [(Eof)    (k eof)]
    [(Var x)  (lookup r x k)]
    [(Prim0 p) (interp-prim0 p k)]
    [(Prim1 p e)
     (interp-env e r
                 (λ (v)
                   (interp-prim1 p v k)))]
    [(Prim2 p e1 e2)
     (interp-env e1 r
                 (λ (v1)
                   (interp-env e2 r
                               (λ (v2)
                                 (interp-prim2 p v1 v2 k)))))]
    [(If p e1 e2)
     (interp-env p r
                 (λ (v)
                   (if v
                       (interp-env e1 r k)
                       (interp-env e2 r k))))]
    [(Begin e1 e2)
     (interp-env e1 r
                 (λ (v)
                   (interp-env e2 r k)))]
    [(Let x e1 e2)
     (interp-env e1 r
                 (λ (v1)
                   (interp-env e2 (ext r x v1))))]))

;; Env Id Cont -> Value
(define (lookup r x k)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         (k val)
         (lookup r x k))]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))


;; Op0 Cont -> Answer
(define (interp-prim0 op k)
  (match op
    ['read-byte (k (read-byte))]
    ['peek-byte (k (peek-byte))]
    ['void      (k (void))]))

;; Op1 Value Cont -> Answer
(define (interp-prim1 op v k)
  (match op
    ['add1          (if (integer? v) (k (add1 v)) 'err)]
    ['sub1          (if (integer? v) (k (sub1 v)) 'err)]
    ['zero?         (if (integer? v) (k (zero? v)) 'err)]
    ['char?         (k (char? v))]
    ['char->integer (if (char? v) (k (char->integer v)) 'err)]
    ['integer->char (if (codepoint? v) (k (integer->char v)) 'err)]
    ['eof-object?   (k (eof-object? v))]
    ['write-byte    (if (byte? v) (k (write-byte v)) 'err)]))

;; (Label interp_prim1)
;; op: rax
;; v:  r8
;; k:  r9
;; (Label 'case_add1)
;; (Cmp rax 'add1)
;; (Jne 'case_sub1)
;; if integer? r8, Add r8 1, Mov rax r8, Jmp k
;; else Jmp err
;; (Label case_sub1)
;; (Cmp rax 'sub1)
;; (Jne 'case_zero?)




;; Op2 Value Value Cont -> Answer
(define (interp-prim2 op v1 v2 k)
  (match op
    ['+ (if (and (integer? v1) (integer? v2)) (k (+ v1 v2)) 'err)]
    ['- (if (and (integer? v1) (integer? v2)) (k (- v1 v2)) 'err)]
    ['< (if (and (integer? v1) (integer? v2)) (k (< v1 v2)) 'err)]
    ['= (if (and (integer? v1) (integer? v2)) (k (= v1 v2)) 'err)]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))