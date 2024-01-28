#lang racket
(provide parse parse-e parse-define parse-module parse-module-file)
(require "ast.rkt")

;; Need to pass around the module's file name in order to resolve
;; paths in requires.

;; String -> Module
(define (parse-module-file fn)
  (let ((p (open-input-file fn)))
    (begin (read-line p) ; ignore #lang racket line
           (begin0 (parse-module (read-all p) fn)
             (close-input-port p)))))

;; Port -> SExpr
(define (read-all p)
  (let ((r (read p)))
    (if (eof-object? r)
        '()
        (cons r (read-all p)))))

;; S-Expr Path -> Module
(define (parse-module m p)
  (match (parse-module* m p)
    [(list ps rs ds #f)
     (Module ps rs ds)]
    [(list ps rs ds e)
     (Module (cons 'main ps) rs (cons (Defn 'main '() e) ds))]))

(define parse (lambda (m) (parse-module m "<STDIN>")))

(define (parse-module* m p)
  (match m
    ['() (list '() '() '() #f)]
    [(cons x m)
     (match (parse-module* m p)
       [(list ps rs ds e)
        (match x
          [(cons 'provide _)
           (list (append (parse-provide x) ps) rs ds e)]
          [(cons 'require _)
           (list ps (append (parse-require x p) rs) ds e)]
          [(cons 'define _)
           (list ps rs (cons (parse-define x) ds) e)]
          [_
           (list ps rs ds
                 (if e
                     (Begin (parse-e x) e)
                     (parse-e x)))])])]))                     

(define (parse-provide x)
  (match x
    [(cons 'provide xs)
     (if (andmap symbol? xs)
         xs
         (error "invalid provide clause"))]))

(define (parse-require x)
  (match x
    [(cons 'require xs)
     (if (andmap string? xs)
         (map (lambda (x)
                (list x (Module-ps (parse-module-file x))))
              xs)
         (error "invalid require clause"))]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list (? symbol? f) (? symbol? xs) ...) e)
     (Defn f xs (parse-e e))]
    [_ (error "Parse defn error" s)]))

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
