#lang racket
(provide parse parse-define parse-e parse-library)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match s
    ['() (Prog '())]
    [(cons (and (cons (? def-keyword?) _) d) '())
     (Prog (append (parse-define d)
                   (list (Defn (gensym) (parse-e '(void))))))]
    [(cons (and (cons (? def-keyword?) _) d) s)
     (match (parse s)
       [(Prog ds)
        (Prog (append (parse-define d) ds))])]
    [(cons (cons 'provide _) s) ; ignore provides for now
     (parse s)]
    [(cons (cons 'require _) s) ; ignore requires for now
     (parse s)]
    [(cons (cons 'module+ _) s) ; ignore submodules for now
     (parse s)]
    ;; Doesn't quite work and will make parse depend on read
    #;
    [(cons (cons 'require fs) s)
     (match (parse s)
       [(Prog ds)
        (Prog (append (load-files loaded fs) ds))])]
    [(cons e s)
     (match (parse s)
       [(Prog ds)
        (Prog (cons (Defn (gensym) (parse-e e)) ds))])]
    [_ (error "program parse error" s)]))

(define (def-keyword? x)
  (or (eq? x 'define)
      (eq? x 'struct)))

;; [Listof S-Expr] -> Lib
(define (parse-library s)
  (match s
    [(cons (cons 'provide ids)
           (cons (cons 'require _) ds))
     (match (parse ds)
       [(Prog ds)
        (Lib ids ds)])]))

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
    [(cons 'begin es)
     (Begin (parse-es es))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(cons 'let s)   (parse-let s)]
    [(cons 'match s) (parse-match s)]
    [(list 'λ xs e)
     (parse-param-list xs e)]
    [(list 'lambda xs e)
     (parse-param-list xs e)]
    [(cons 'case-lambda cs)
     (LamCase (gensym 'lamcase)
              (parse-case-lambda-clauses cs))]
    [(cons 'apply (cons e es))
     (parse-apply (parse-e e) es)]
    [(list 'cond (list 'else e)) (parse-e e)]
    [(cons 'cond (cons (list e1 e2) r))
     (If (parse-e e1)
         (parse-e e2)
         (parse-e (cons 'cond r)))]
    [(cons 'or '())
     (Quote #f)]
    [(cons 'or (cons e es))
     (let ((x (gensym 'or)))
       (Let (list x) (list (parse-e e))
            (If (Var x) (Var x) (parse-e (cons 'or es)))))]
    [(cons 'and '())
     (Quote #t)]
    [(cons 'and (cons e '()))
     (parse-e e)]
    [(cons 'and (cons e es))
     (If (parse-e e)
         (parse-e (cons 'and es))
         (Quote #f))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]
    [_ (error "Parse error" s)]))

(define (parse-es es)
  (match es
    ['() '()]
    [(cons e es)
     (cons (parse-e e) (parse-es es))]
    [_ (error "parse es")]))

;; S-Expr -> Expr
(define (parse-let s)
  (match s
    ['() (error "parse error (let)")]
    [(cons s1 s2)
     (parse-let-bindings s1 s2 '() '())]
    [_ (error "parse error let" s)]))

;; S-Expr S-Expr [Listof Id] [Listof Expr] -> Expr
(define (parse-let-bindings s1 s2 xs es)
  (match s1
    ['() (parse-let-body s2 (reverse xs) (reverse es))]
    [(cons (list (? symbol? x) e) s1)
     (parse-let-bindings s1 s2 (cons x xs) (cons (parse-e e) es))]))

;; S-Expr [Listof Id] [Listof Expr] -> Expr
(define (parse-let-body s xs es)
  (match s
    ['() (error "parse error let-body")]
    [(cons e '())
     (Let xs es (parse-e e))]
    [_
     (Let xs es (Begin (parse-es s)))]))

;; Expr S-Expr -> Expr
(define (parse-apply e es)
  (match es
    [(list el) (Apply e '() (parse-e el))]
    [(cons e0 es)
     (match (parse-apply e es)
       [(Apply e es el)
        (Apply e (cons (parse-e e0) es) el)])]
    [_ (error "parse apply error")]))


;; S-Expr -> Expr
(define (parse-match s)
  (match s
    ['() (error "parse error match")]
    [(cons e s)
     (parse-match-clauses s (parse-e e) '() '())]))

;; S-Expr Expr [Listof Pat] [Listof Expr] -> Expr
(define (parse-match-clauses s e ps es)
  (match s
    ['() (Match e (reverse ps) (reverse es))]
    [(cons c s)
     (parse-match-clause c s e ps es)]
    [_ (error "parse error match clause")]))

(define (parse-match-clause c s e ps es)
  (match c
    [(list p e1)
     (parse-match-clauses s e (cons (parse-pat p) ps) (cons (parse-e e1) es))]
    [(list* p es1)
     (parse-match-clauses s e (cons (parse-pat p) ps) (cons (Begin (parse-es es1)) es))]
    [_
     (error "parse error clause")]))

(define (parse-pat p)
  (match p
    [(? boolean?) (PLit p)]
    [(? integer?) (PLit p)]
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
    [(list 'and) (PWild)]
    [(list 'and p) (parse-pat p)]
    [(cons 'and (cons p ps))
     (PAnd (parse-pat p) (parse-pat (cons 'and ps)))]
    [(cons 'list '())
     (PLit '())]
    [(cons 'list (cons p1 ps))
     (PCons (parse-pat p1)
            (parse-pat (cons 'list ps)))]
    [(list '? e)
     (PPred (parse-e e))]
    [(cons '? (cons e ps))
     (PAnd (parse-pat (list '? e))
           (parse-pat (cons 'and ps)))]
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
     (eprintf "xs: ~a e: ~a\n" xs e)
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
  '(read-byte void read-char peek-char
              current-input-port ; hack, doesn't actually exist
              system-type
              ))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length
         symbol->string string->symbol symbol?
         number->string string->uninterned-symbol
         open-input-file
         read-byte-port
         write-char
         error integer?
         eq-hash-code
         char-alphabetic?))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref
      string-append set-box! quotient remainder
      bitwise-and bitwise-ior bitwise-xor arithmetic-shift
      peek-byte))
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
