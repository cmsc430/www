#lang racket
(provide parse parse-define parse-e)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match s
    [(cons (and (cons 'define _) d) s)
     (match (parse s)
       [(Prog ds e)
        (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (? symbol? f)
           (list-rest 'case-lambda cs))
     (Defn f (FunCase (parse-case-lambda-clauses cs)))]    
    [(list 'define (cons (? symbol? f) xs) e)
     (if (all symbol? xs)
         (Defn f (parse-param-list xs e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

;; like andmap, but work on improper lists too
(define (all p? xs)
  (match xs
    ['() #t]
    [(cons x xs) (and (p? x) (all p? xs))]
    [x (p? x)]))

;; S-Expr -> [Listof FunCaseClause]
(define (parse-case-lambda-clauses cs)
  (match cs
    ['() '()]
    [(cons c cs)
     (cons (parse-case-lambda-clause c)
           (parse-case-lambda-clauses cs))]
     [_
      (error "parse case-lambda error")]))

;; S-Expr -> FunRest
(define (parse-case-lambda-clause c)
  (match c
    [(list (? symbol? x) e)
     (FunRest '() x (parse-e e))]
    [(list xs e)
     (parse-param-list xs e)]))    

;; S-Expr S-Expr -> FunPlain or FunRest
(define (parse-param-list xs e)
  (match xs
    ['() (FunPlain '() (parse-e e))]
    [(cons x xs)
     (match (parse-param-list xs e)
       [(FunPlain xs e) (FunPlain (cons x xs) e)]
       [(FunRest xs y e) (FunRest (cons x xs) y e)])]
    [(? symbol? xs)
     (FunRest '() xs (parse-e e))]
    [_
     (error "parse parameter list error")]))
  

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (Str s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    [_ (error "Parse error" s)]))

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length))
(define op2
  '(+ - < = cons make-vector vector-ref make-string string-ref))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
