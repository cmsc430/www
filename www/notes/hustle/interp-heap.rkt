#lang racket
(provide interp interp-env-heap)
(require "heap.rkt"
         "env.rkt"
         "unload.rkt"
         "interp-prims-heap.rkt"
         "ast.rkt")

;; type Answer* =
;; | (cons Heap Value*)
;; | 'err

;; type Value* =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (list 'box  Address)
;; | (list 'cons Address)

;; type Heap = (Listof Value*)
;; type REnv = (Listof (List Id Value*))

;; Expr -> Answer
(define (interp e)  
  (unload (interp-env-heap e '() '())))

;; Expr REnv Heap -> Answer*
(define (interp-env-heap e r h)
  (match e
    [(Int i)  (cons h i)]
    [(Bool b) (cons h b)]
    [(Char c) (cons h c)]
    [(Eof)    (cons h eof)]
    [(Empty)  (cons h '())]
    [(Var x)  (cons h (lookup r x))]
    [(Prim0 'read-byte) (cons h (read-byte))]
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
        (if v
            (interp-env-heap e1 r h)
            (interp-env-heap e2 r h))])]
    [(Begin e1 e2)     
     (match (interp-env-heap e1 r)
       ['err 'err]
       [_    (interp-env-heap e2 r)])]
    [(Let x e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v)
        (interp-env-heap e2 (ext r x v) h)])]))
