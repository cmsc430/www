#lang racket
(provide (all-defined-out))

(require "ast.rkt")
(require "dot.rkt")

(define (render-prog p fn)
  (match p
    [(prog '() e)
      (match (render-expr e)
        [(cons _ res) (graph fn res)])]
    [(prog ds e)
      (match (render-expr e)
        [(cons _ res) (graph fn res)])]))

(define (render-fun f)
  (match f
    [(fundef name args body)
      (match (render-expr e)
        [(cons _ res) (subgraph name res)])]))

; render-expr : Expr -> (ID, [Content])
(define (render-expr e)
  (let ((i (gensym)))
  (match e
    [(nil-e)       (cons i (list (label "nil" i)))]
    [(int-e v)     (cons i (list (label (~a v) i)))]
    [(bool-e b)    (cons i (list (label (~a b) i)))]
    [(var-e v)     (cons i (list (label (symbol->string v) i)))]
    [(char-e c)    (cons i (list (label (~a c) i)))]
    [(fun-e f)     (cons i (list (label (~a "fun " f) i)))]
    [(call-e f es) (cons i (render-subs i (~a "(call " f " ...)") es))]
    [(app-e f es)  (cons i (render-subs i (~a "(" f " ...)") es))]
    [(prim-e p es) (cons i (render-subs i (~a "(" (symbol->string p) " ...)") es))]
    [(if-e e t f)  (cons i (render-subs i "if" (list e t f)))]
    [(let-e bs b)  (cons i (render-let i "let" bs b))])))

; On big programs this will be bad (it's n^2, I think)
(define (render-subs pid str es)
    (let* ((subps (map render-expr es))
           (subs  (append* (map cdr subps)))
           (parent (label str pid))
           (ids (map car subps)))
          `(,@subs
            ,parent
            ,(tm pid
                 ids))))

(define (render-let pid str bs body)
  (let* ((bind (render-binding bs))
         (body (render-expr body))
         (parent (label str pid)))
  `(,@(cdr bind)
    ,@(cdr body)
    ,parent
    ,(tm pid (list (car body) (car bind))))))

(define (render-binding bs)
  (match bs
    [(cons (binding v body) '())
      (let* ((i (gensym))
             (vn (label (symbol->string v) i))
             (bn (render-expr body))
             (cid (car bn)))
            (cons i `(,@(cdr bn) ,vn ,(color "deeppink2" (e i cid)))))]))
