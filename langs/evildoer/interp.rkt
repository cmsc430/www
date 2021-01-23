#lang racket
(provide interp)
(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; Expr -> Value
(define (interp e)
  (match e
    [(Eof) eof]
    [(Int i) i]    
    [(Bool b) b]
    [(Char c) c]
    [(Prim0 'read-byte)
     (read-byte)]
    [(Prim0 'peek-byte)     
     (peek-byte)]
    [(Prim0 'void)
     (void)]
    [(Prim1 'write-byte e0)     
     (write-byte (interp e0))]
    [(Prim1 'eof-object? e0)     
     (eof-object? (interp e0))]
    [(Prim1 'add1 e0)
     (add1 (interp e0))]
    [(Prim1 'sub1 e0)
     (sub1 (interp e0))]
    [(Prim1 'zero? e)
     (zero? (interp e))]
    [(Prim1 'integer->char e)
     (integer->char (interp e))]
    [(Prim1 'char->integer e)
     (char->integer (interp e))]
    [(Prim1 'char? e)
     (char? (interp e))]    
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    [(Begin e1 e2)
     (begin (interp e1)
            (interp e2))]))
