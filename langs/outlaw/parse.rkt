#lang racket
(provide parse parse-define parse-e parse-library)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match s
    [(cons (and (cons (or 'define 'struct) _) d) s)
     (match (parse s)
       [(Prog ds e)
        (Prog (append (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

;; [Listof S-Expr] -> Lib
(define (parse-library s)
  (match s
    [(cons (cons 'provide ids)
           (cons (cons 'require _) ds))
     (Lib ids (parse-ds ds))]))
   
;; [Listof S-Expr] -> [Listof Defn]
(define (parse-ds s)
  (match s
    ['() '()]
    [(cons d ds)
     (append (parse-define d)
             (parse-ds ds))]))

;; S-Expr -> [Listof Defn]
(define (parse-define s)
  (match s
    [(list 'define (cons f xs) e)
     (match (parse-param-list xs e)
       [(Lam l xs e)
        (list (Defn f (Lam l xs e)))]
       [(LamRest l xs x e)
        (list (Defn f (LamRest l xs x e)))])]
    [(list 'define f (cons 'case-lambda cs))
     (list (Defn f (LamCase (gensym 'lamcase)
                            (parse-case-lambda-clauses cs))))]
    [(list 'define (? symbol? x) e)
     (match (parse-e e)
       [e (list (Defn x e))])]
    [(cons 'struct _)
     (parse-struct s)]     
    [_ (error "Parse defn error" s)]))

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
  (Defn n
    (Lam (gensym 'lam)
         flds
         (Prim 'make-struct (cons (Quote n) (map Var flds))))))

;; Id -> [Listof Defn]
(define (make-struct-defn-predicate n)
  (Defn (symbol-append n '?)
    (Lam (gensym 'lam)
         (list 'x)
         (Prim 'struct? (list (Quote n) (Var 'x))))))

;; Id [Listof Id] -> [Listof Defn]
(define (make-struct-defn-accessors n flds)
  (match flds
    ['() '()]
    [(cons f flds)
     (cons (Defn (symbol-append n '- f)
             (Lam (gensym 'lam)
                  (list 'x)
                  (Prim 'struct-ref
                        (list (Quote n)
                              (Quote (length flds))
                              (Var 'x)))))
           (make-struct-defn-accessors n flds))]))

;; Symbol ... -> Symbol
(define (symbol-append . ss)
  (string->symbol
   (apply string-append (map symbol->string ss))))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? self-quoting?)             (Quote (parse-datum s))]
    [(list 'quote d)               (Quote (parse-datum d))]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list (? (op% op0) p0))       (Prim (drop-% p0) '())]
    [(list (? (op% op1) p1) e)     (Prim (drop-% p1) (list (parse-e e)))]
    [(list (? (op% op2) p2) e1 e2) (Prim (drop-% p2) (list (parse-e e1) (parse-e e2)))]
    [(list (? (op% op3) p3) e1 e2 e3)
     (Prim (drop-% p3) (list (parse-e e1) (parse-e e2) (parse-e e3)))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]    
    [(list (or 'lambda 'λ) xs e)
     (parse-param-list xs e)]
    [(cons 'case-lambda cs)
     (LamCase (gensym 'lamcase)
              (parse-case-lambda-clauses cs))]
    [(cons 'apply (cons e es))
     (parse-apply (parse-e e) es)]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]    
    [_ (error "Parse error" s)]))

;; Expr S-Expr -> Expr
(define (parse-apply e es)
  (match es
    [(list el) (Apply e '() (parse-e el))]
    [(cons e0 es)
     (match (parse-apply e es)
       [(Apply e es el)
        (Apply e (cons (parse-e e0) es) el)])]
    [_ (error "parse apply error")]))

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
    [(? integer?) (PLit p)]
    [(? char?)    (PLit p)]
    ['_           (PWild)]
    [(? symbol?)  (PVar p)]
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

;; S-Expr -> [Listof LamCaseClause]
(define (parse-case-lambda-clauses cs)
  (match cs
    ['() '()]
    [(cons c cs)
     (cons (parse-case-lambda-clause c)
           (parse-case-lambda-clauses cs))]
     [_
      (error "parse case-lambda error")]))

;; S-Expr -> LamCaseClause
(define (parse-case-lambda-clause c)
  (match c
    [(list xs e)
     (parse-param-list xs e)]))

;; S-Expr S-Expr -> Lam or LamRest
(define (parse-param-list xs e)
  (match xs
    ['() (Lam (gensym 'lam) '() (parse-e e))]
    [(cons x xs)
     (match (parse-param-list xs e)
       [(Lam f xs e) (Lam f (cons x xs) e)]
       [(LamRest f xs y e) (LamRest f (cons x xs) y e)])]
    [(? symbol? xs)
     (LamRest (gensym 'lamrest) '() xs (parse-e e))]
    [_
     (error "parse parameter list error")]))

;; Datum -> Datum
(define (parse-datum d)
  (match d
    [(box d)
     (box (parse-datum d))]    
    [(cons d1 d2)
     (cons (parse-datum d1) (parse-datum d2))]
    ['() '()]
    [(? symbol? s) s]
    [(? integer? i) i]
    [(? boolean? b) b]
    [(? string? s) s]
    [(? char? c) c]
    [(? vector? v)
     (apply vector (map parse-datum (vector->list v)))]
    [_ (error "parse datum error")]))

(define (self-quoting? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (string? x)
      (box? x)
      (vector? x)))

(define op0
  '(read-byte peek-byte void read-char peek-char))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length
         symbol->string string->symbol symbol?
         number->string string->uninterned-symbol
         open-input-file
         read-byte-port
         write-char))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref
      string-append set-box! quotient remainder
      bitwise-and arithmetic-shift
      peek-byte-port))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))


(define (op% ops)
  (λ (x)
    (and (symbol? x)
         (eq? #\% (string-ref (symbol->string x) 0))
         (let ((x* (drop-% x)))
           (and (memq x* ops)
                x*)))))

(define (drop-% x)
  (string->symbol  (substring (symbol->string x) 1)))

#|
(define op% op?)
(define (drop-% x) x)
|#

