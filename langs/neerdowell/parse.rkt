#lang racket
(provide parse parse-define parse-e parse-struct)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match s
    [(cons (and (cons 'struct _) d) s)
     (match (parse s)
       [(Prog ds e)
        (Prog (append (parse-struct d) ds) e)])]
    [(cons (and (cons 'define _) d) s)
     (match (parse s)
       [(Prog ds e)
        (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

;; S-Expr -> [Listof Defn]
(define (parse-struct s)
  (match s
    [(list 'struct (? symbol? n) flds)
     (if (andmap symbol? flds)
         (list* (make-struct-defn-construct n flds)
                (make-struct-defn-predicate n)
                (make-struct-defn-accessors n (reverse flds)))
         (error "parse struct definition error"))]
    [_ (error "parse struct definition error")]))

;; Id [Listof Id] -> [Listof Defn]
(define (make-struct-defn-construct n flds)
  (Defn n flds
    (Prim 'make-struct (cons (Quote n) (map Var flds)))))

;; Id -> [Listof Defn]
(define (make-struct-defn-predicate n)
  (Defn (symbol-append n '?) (list 'x)
    (Prim 'struct? (list (Quote n) (Var 'x)))))

;; Id [Listof Id] -> [Listof Defn]
(define (make-struct-defn-accessors n flds)
  (match flds
    ['() '()]
    [(cons f flds)
     (cons (Defn (symbol-append n '- f) (list 'x)
             (Prim 'struct-ref
                   (list (Quote n)
                         (Quote (length flds))
                         (Var 'x))))
           (make-struct-defn-accessors n flds))]))

;; Symbol ... -> Symbol
(define (symbol-append . ss)
  (string->symbol
   (apply string-append (map symbol->string ss))))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? self-quoting?)             (Quote s)]
    [(list 'quote d)               (Quote d)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list (? (op? op0) p0))       (Prim p0 '())]
    [(list (? (op? op1) p1) e)     (Prim p1 (list (parse-e e)))]
    [(list (? (op? op2) p2) e1 e2) (Prim p2 (list (parse-e e1) (parse-e e2)))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim p3 (list (parse-e e1) (parse-e e2) (parse-e e3)))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    [(list (or 'lambda 'λ) xs e)
     (if (and (list? xs)
              (andmap symbol? xs))
         (Lam (gensym 'lambda) xs (parse-e e))
         (error "parse lambda error"))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]
    [_ (error "Parse error" s)]))

(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]))

(define (parse-pat p)
  (match p
    [(? boolean?) (PLit p)]
    [(? exact-integer?) (PLit p)]
    [(? char?)    (PLit p)]
    ['_           (PWild)]
    [(? symbol?)  (PVar p)]
    [(? string?)  (PStr p)]
    [(list 'quote (? symbol? s))
     (PSymb s)]
    [(list 'quote (list))
     (PLit '())]
    [(list 'box p)
     (PBox (parse-pat p))]
    [(list 'cons p1 p2)
     (PCons (parse-pat p1) (parse-pat p2))]
    [(list 'and p1 p2)
     (PAnd (parse-pat p1) (parse-pat p2))]
    [(cons 'list '())
     (PLit '())]
    [(cons 'list (cons p1 ps))
     (PCons (parse-pat p1)
            (parse-pat (cons 'list ps)))]
    [(cons (? symbol? n) ps)
     (PStruct n (map parse-pat ps))]))

(define (self-quoting? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)
      (string? x)
      (box? x)
      (vector? x)))

(define op0
  '(read-byte peek-byte void))
(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length
         symbol? symbol->string string->symbol string->uninterned-symbol))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
