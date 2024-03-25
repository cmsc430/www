#lang crook
{:= A B C D0 D0.A D1 E0 E1 F H0 H1 I J K L}
(provide parse {:> I} parse-e {:> I} parse-define)
(require "ast.rkt")

{:> A I} ;; S-Expr -> Expr
{:> A I}
(define (parse s)
  (match s
    {:> E0}
    ['eof                (Eof)]
    {:> A D0}
    [(? exact-integer?) (Lit s)]
    {:> D0}
    [(? datum?)          (Lit s)]
    {:> F}
    [(? symbol?)         (Var s)]
    {:> H0}
    [(list 'quote (list)) (Lit '())]
    {:> E0}
    [(list (? op0? o))   (Prim0 o)]
    {:> B}
    [(list (? op1? o) e) (Prim1 o (parse e))]
    {:> F}
    [(list (? op2? o) e1 e2) (Prim2 o (parse e1) (parse e2))]
    {:> H1}
    [(list (? op3? o) e1 e2 e3) (Prim3 o (parse e1) (parse e2) (parse e3))]
    {:> E0}
    [(list 'begin e1 e2) (Begin (parse e1) (parse e2))]
    {:> C D0}
    [(list 'if (list 'zero? e1) e2 e3)
     (IfZero (parse e1) (parse e2) (parse e3))]
    {:> D0}
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    {:> F}
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))]
    [_ (error "Parse error")]))

{:> I} ;; S-Expr ... -> Prog
{:> I}
(define (parse . s)
  (match s
    [(cons (and (cons 'define _) d) s)
     (match (apply parse s)
       [(Prog ds e)
        (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

{:> I} ;; S-Expr -> Defn
{:> I}
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

{:> I} ;; S-Expr -> Expr
{:> I}
(define (parse-e s)
  (match s
    [(? datum?)               (Lit s)]
    ['eof                     (Eof)]
    [(? symbol?)              (Var s)]
    [(list 'quote (list))     (Lit '())]
    [(list (? op0? p0))       (Prim0 p0)]
    [(list (? op1? p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? op2? p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? op3? p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    {:> K}
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    {:> I L}
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    {:> L}
    [(list (or 'lambda 'λ) (? (lambda (xs) (and (list? xs) (andmap symbol? xs)) xs) xs) e)
     (Lam (gensym 'lambda) xs (parse-e e))]    
    {:> L}
    [(cons e es)
     (App (parse-e e) (map parse-e es))]
    [_ (error "Parse error" s)]))

{:> K} ;; Expr [Listof S-Expr]
{:> K}
(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]
    [_ (error "Parse match error" e ms)]))

{:> K} ;; S-Expr -> Pat
{:> K}
(define (parse-pat p)
  (match p
    [(? datum?) (Lit p)]
    [(? symbol?) (Var p)]
    [(list 'quote (list)) (Lit '())]
    [(list 'box p)
     (Box (parse-pat p))]
    [(list 'cons p1 p2)
     (Cons (parse-pat p1) (parse-pat p2))]
    [(list 'and p1 p2)
     (Conj (parse-pat p1) (parse-pat p2))]))


{:> D0} ;; Any -> Boolean
{:> D0}
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      {:> D1}
      (char? x)
      {:> H1}
      (string? x)))

{:> E0} ;; Any -> Boolean
{:> E0}
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

{:> B}
(define (op1? x)
  (memq x '(add1 sub1 {:> D0} zero? {:> D1} char? {:> D1} integer->char {:> D1} char->integer
                 {:> E0} write-byte {:> E0} eof-object?
                 {:> H0} box {:> H0} unbox {:> H0} empty? {:> H0} cons? {:> H0} box? {:> H0} car {:> H0} cdr
                 {:> H1} vector? {:> H1} vector-length {:> H1} string? {:> H1} string-length)))

{:> F}
(define (op2? x)
  (memq x '(+ - < = {:> H0} eq? {:> H0} cons
              {:> H1} make-vector {:> H1} vector-ref {:> H1} make-string {:> H1} string-ref)))

{:> H1}
(define (op3? x)
  (memq x '(vector-set!)))
