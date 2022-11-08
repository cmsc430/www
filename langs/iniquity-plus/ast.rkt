#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id Fun)
(struct Defn (f fun) #:prefab)

;; type Fun = (FunPlain [Listof Id] Expr)
;;          | (FunRest [Listof Id] Id Expr)
;;          | (FunCase [Listof FunCaseClause])
;; type FunCaseClause = (FunPlain [Listof Id] Expr)
;;                    | (FunRest [Listof Id] Id Expr)
(struct FunPlain (xs e)   #:prefab)
(struct FunRest  (xs x e) #:prefab)
(struct FunCase  (cs)     #:prefab)

;; type Expr = (Eof)
;;           | (Empty)
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (Str String)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (Prim3 Op3 Expr Expr Expr)
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let Id Expr Expr)
;;           | (Var Id)
;;           | (App Id (Listof Expr))
;;           | (Apply Id (Listof Expr) Expr)
;; type Id   = Symbol
;; type Op0  = 'read-byte
;; type Op1  = 'add1 | 'sub1 | 'zero?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox
;;           | 'empty? | 'cons? | 'box?
;;           | 'vector? | vector-length
;;           | 'string? | string-length
;; type Op2  = '+ | '- | '< | '=
;;           | 'cons
;;           | 'make-vector | 'vector-ref
;;           | 'make-string | 'string-ref
;; type Op3  = 'vector-set!
(struct Eof   ()           #:prefab)
(struct Empty ()           #:prefab)
(struct Int   (i)          #:prefab)
(struct Bool  (b)          #:prefab)
(struct Char  (c)          #:prefab)
(struct Str   (s)          #:prefab)
(struct Prim0 (p)          #:prefab)
(struct Prim1 (p e)        #:prefab)
(struct Prim2 (p e1 e2)    #:prefab)
(struct Prim3 (p e1 e2 e3) #:prefab)
(struct If    (e1 e2 e3)   #:prefab)
(struct Begin (e1 e2)      #:prefab)
(struct Let   (x e1 e2)    #:prefab)
(struct Var   (x)          #:prefab)
(struct App   (f es)       #:prefab)
(struct Apply (f es e)     #:prefab)

;; Prog -> Void
(define (check-syntax p)
  (match p
    [(Prog ds e)
     (let ((dr (defined-ids ds)))
       (check-syntax-unique-defines ds)
       (check-syntax-defines ds dr)
       (check-syntax-e e dr '()))]))

;; [Listof Defn] -> [Listof Id]
(define (defined-ids ds)
  (map (λ (d) (match d [(Defn f _) f]))
       ds))

;; [Listof Defn] -> Void
(define (check-syntax-unique-defines ds)
  (unless (= (length ds)
	     (length (remove-duplicates ds #:key Defn-f)))
    (error "duplicate definition for function")))

;; [Listof Defn] [Listof Id] -> Void
(define (check-syntax-defines ds r)
  (for-each (λ (d) (check-syntax-define d r)) ds))

;; Defn [Listof Id] -> Void
(define (check-syntax-define d dr)
  (match d
    [(Defn f (FunPlain xs e))
     (check-unique (cons f xs))
     (check-syntax-e e dr xs)]
    [(Defn f (FunRest xs x e))
     (check-unique (cons f (cons x xs)))
     (check-syntax-e e dr (cons x xs))]
    [(Defn f (FunCase '()))
     (void)]
    [(Defn f (FunCase (cons c cs)))
     (check-syntax-define (Defn f c) dr)
     (check-syntax-define (Defn f (FunCase cs)) dr)]))

;; [Listof Id] -> Void
(define (check-unique xs)
  (unless (= (length xs) (length (remove-duplicates xs)))
    (error "duplicate identifier")))

;; Expr [Listof Id] [Listof Id] -> Void
(define (check-syntax-e e dr r)
  (match e
    [(Eof) (void)]
    [(Empty) (void)]
    [(Int i) (void)]
    [(Bool b) (void)]
    [(Char c) (void)]
    [(Str s) (void)]
    [(Prim0 p) (void)]
    [(Prim1 p e) (check-syntax-e e dr r)]
    [(Prim2 p e1 e2)
     (check-syntax-e e1 dr r)
     (check-syntax-e e2 dr r)]
    [(Prim3 p e1 e2 e3)
     (check-syntax-e e1 dr r)
     (check-syntax-e e2 dr r)
     (check-syntax-e e3 dr r)]
    [(If e1 e2 e3)
     (check-syntax-e e1 dr r)
     (check-syntax-e e2 dr r)
     (check-syntax-e e3 dr r)]
    [(Begin e1 e2)
     (check-syntax-e e1 dr r)
     (check-syntax-e e2 dr r)]
    [(Let x e1 e2)
     (check-syntax-e e1 dr r)
     (check-syntax-e e2 dr (cons x r))]
    [(Var x)
     (unless (member x r)
       (error "unbound variable"))]
    [(App f es)
     (unless (member f dr)
       (error "undefined function"))
     (for-each (λ (e) (check-syntax-e e dr r)) es)]
    [(Apply f es e)
     (unless (member f dr)
       (error "undefined function"))
     (check-syntax-e e dr r)
     (for-each (λ (e) (check-syntax-e e dr r)) es)]))

(module+ test
  (require rackunit)
  (check-exn exn:fail? (λ () (check-syntax-e (Var 'x) '() '())))
  (check-exn exn:fail? (λ () (check-syntax-e (Var 'x) '(x) '())))
  (check-not-exn (λ () (check-syntax-e (Var 'x) '() '(x))))
  (check-not-exn (λ () (check-syntax-e (Let 'x (Int 1) (Var 'x)) '() '())))
  (check-not-exn (λ () (check-syntax-e (Let 'x (Int 1) (Let 'y (Int 2) (Var 'x))) '() '())))
  (check-not-exn (λ () (check-syntax-e (Let 'x (Int 1) (Let 'x (Int 2) (Var 'x))) '() '())))
  (check-not-exn (λ () (check-syntax-e (Let 'x (Int 1) (Let 'y (Int 2) (Var 'y))) '() '())))
  (check-exn exn:fail? (λ () (check-syntax (Prog (list (Defn 'f (FunPlain '() (Int 1)))) (Var 'f)))))
  (check-exn exn:fail? (λ () (check-syntax (Prog (list (Defn 'f (FunPlain '(f) (Int 1)))) (Int 1)))))
  (check-exn exn:fail? (λ () (check-syntax (Prog (list (Defn 'f (FunRest '(f) 'x (Int 1)))) (Int 1)))))
  (check-exn exn:fail? (λ () (check-syntax (Prog (list (Defn 'f (FunRest '() 'f (Int 1)))) (Int 1)))))
  (check-exn exn:fail? (λ () (check-syntax (Prog (list (Defn 'f (FunPlain '(x x) (Int 1)))) (Int 1)))))
  (check-exn exn:fail?
	     (λ () (check-syntax
		    (Prog (list (Defn 'f (FunPlain '(x) (Int 1)))
				(Defn 'f (FunPlain '(y) (Int 2))))
			  (Int 1)))))
  (check-exn exn:fail? (λ () (check-syntax (Prog '() (App 'f '())))))
  (check-exn exn:fail? (λ () (check-syntax (Prog '() (Apply 'f '() (Int 1))))))
  (check-not-exn (λ () (check-syntax (Prog (list (Defn 'f (FunPlain '() (Int 1)))) (App 'f '())))))
  (check-not-exn (λ () (check-syntax (Prog (list (Defn 'f (FunPlain '() (Int 1)))) (Apply 'f '() (Int 1)))))))
