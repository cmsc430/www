#lang racket
(provide interp interp-env-heap)
(require "types.rkt"
         "env.rkt"
         "heap-bits.rkt"
         "interp-prims-heap-bits.rkt"
         "unload-bits.rkt"
         "ast.rkt")

;; type Answer* =
;; | (cons Heap ValueBits*)
;; | 'err

;; Expr -> Answer
(define (interp e)  
  (unload (interp-env-heap e '() '())))

;; Expr REnv Heap -> Answer
(define (interp-env-heap e r h)
  (match e    
    [(Int i)  (cons h (value->bits i))]
    [(Bool b) (cons h (value->bits b))]
    [(Char c) (cons h (value->bits c))]
    [(Eof)    (cons h (value->bits eof))]
    [(Empty)  (cons h (value->bits '()))]
    [(Var x)  (cons h (lookup r x))]
    [(Prim0 'void) (cons h (value->bits (void)))]
    [(Prim0 'read-byte) (cons h (value->bits (read-byte)))]
    [(Prim0 'peek-byte) (cons h (value->bits (peek-byte)))]
    [(Prim1 p e)
     (match (interp-env-heap e r h)
       ['err 'err]
       [(cons h a)
        (interp-prim1 p a h)])]
    [(Prim2 p e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h a1)        
        (match (interp-env-heap e2 r h)
          ['err 'err]
          [(cons h a2)
           (interp-prim2 p a1 a2 h)])])]
    [(If p e1 e2)
     (match (interp-env-heap p r h)
       ['err 'err]
       [(cons h v)
        (if (= v (value->bits #f))
            (interp-env-heap e2 r h)
            (interp-env-heap e1 r h))])]
    [(Begin e1 e2)     
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h _) (interp-env-heap e2 r h)])]
    [(Let x e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v)
        (interp-env-heap e2 (ext r x v) h)])]))

