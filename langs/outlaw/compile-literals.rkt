#lang racket
(provide compile-literals init-symbol-table literals)
(require "ast.rkt"
         "utils.rkt"
         a86/ast)

(define rdi 'rdi)

;; Prog -> Asm
(define (compile-literals p)
  (append-map compile-literal (literals p)))

;; Symbol -> Asm
(define (compile-literal s)
  (let ((str (symbol->string s)))
    (seq (Label (symbol->data-label s))
         (Dq (string-length str))
         (compile-string-chars (string->list str))
         (if (odd? (string-length str))
             (seq (Dd 0))
             (seq)))))

;; Prog -> Asm
;; Call intern_symbol on every symbol in the program
(define (init-symbol-table p)
  (match (symbols p)
    ['() (seq)]
    [ss  (seq (Sub 'rsp 8)
              (append-map init-symbol ss)
              (Add 'rsp 8))]))

;; Symbol -> Asm
(define (init-symbol s)
  (seq (Lea rdi (symbol->data-label s))
       (Call 'intern_symbol)))

;; Prog -> [Listof Symbol]
(define (literals p)
  (remove-duplicates
   (map to-symbol (literals* p))))

;; Prog -> [Listof Symbol]
(define (symbols p)
  (remove-duplicates (filter symbol? (literals* p))))

;; (U String Symbol) -> Symbol
(define (to-symbol s)
  (if (string? s)
      (string->symbol s)
      s))

;; Prog -> [Listof (U Symbol String)]
(define (literals* p)
  (match p
    [(Prog ds e)
     (append (append-map literals-d ds) (literals-e e))]))

;; Defn -> [Listof (U Symbol String)]
(define (literals-d d)
  (match d
    [(Defn f l)
     (literals-e l)]))

;; Expr -> [Listof (U Symbol String)]
(define (literals-e e)
  (match e
    [(Quote d) (literals-datum d)]
    [(Prim p es)
     (append-map literals-e es)]
    [(If e1 e2 e3)
     (append (literals-e e1) (literals-e e2) (literals-e e3))]
    [(Begin e1 e2)
     (append (literals-e e1) (literals-e e2))]
    [(Let xs es e)
     (append (append-map literals-e es) (literals-e e))]
    [(App e1 es)
     (append (literals-e e1) (append-map literals-e es))]
    [(Lam f xs e)
     (literals-e e)]
    [(LamRest f xs x e1)
     (literals-e e1)]
    [(LamCase f cs)
     (append-map literals-e cs)]
    [(Match e ps es)
     (append (literals-e e) (append-map literals-match-clause ps es))]
    [_ '()]))

;; Pat Expr -> [Listof Symbol]
(define (literals-match-clause p e)
  (append (literals-pat p) (literals-e e)))

;; Pat -> [Listof (U Symbol String)]
(define (literals-pat p)
  (match p
    [(PSymb s) (list s)]
    [(PStr s) (list s)]
    [(PBox p) (literals-pat p)]
    [(PCons p1 p2) (append (literals-pat p1) (literals-pat p2))]
    [(PAnd p1 p2) (append (literals-pat p1) (literals-pat p2))]
    [_ '()]))

;; Datum -> [Listof (U Symbol String)]
(define (literals-datum d)
  (cond
    [(string? d) (list d)]
    [(symbol? d) (list d)]
    [(cons? d)
     (append (literals-datum (car d))
             (literals-datum (cdr d)))]
    [(box? d)
     (literals-datum (unbox d))]
    [(vector? d)
     (append-map literals-datum (vector->list d))]
    [else '()]))

;; [Listof Char] -> Asm
(define (compile-string-chars cs)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Dd (char->integer c))
          (compile-string-chars cs))]))
