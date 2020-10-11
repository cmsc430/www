#lang racket
(provide (all-defined-out))

;; type Expr =
;; | Integer
;; | Boolean
;; | Variable
;; | Prim1 Expr
;; | Prim2 Expr Expr
;; | If Expr Expr Expr
;; | Let (Binding list) Expr

;; type Prim1 = 'add1 | 'sub1 | 'zero?
;; type Prim2 = '+ | '-

;; type Binding = Variable Expr

;; type Variable = Symbol (except 'add1 'sub1 'if, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The AST data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The AST can be viewed as having 'kinds' of nodes.
;;
;; * The nodes that represnt an expression themselves
;;
;; * The nodes that are part of an expression, but no an expression themselves

;; The below are the former:

(struct int-e  (i)        #:transparent)
(struct bool-e (b)        #:transparent)
(struct var-e  (v)        #:transparent)
(struct prim-e (p es)     #:transparent)
(struct if-e   (e t f)    #:transparent)
(struct let-e  (bs b)     #:transparent)

;; The next is the latter:

;; A binding holds a symbol representing the bound variable and
;; Expr that represents the value that will be bound to that variable
(struct binding (v e) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (predicates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unops '(add1 sub1 zero?))
(define biops '(+ -))

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x (append unops biops))))

;; Any -> Boolean
(define (biop? x)
  (and (symbol? x)
       (memq x biops)))

;; Any -> Boolean
(define (unop? x)
  (and (symbol? x)
       (memq x unops)))

(define (value? v)
  (or (int-e? v)
      (bool-e? v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (getters)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It will sometimes be useful to get the list of all the variables that are
;; introduced by a `let`
;; [Binding] -> [Symbol]
(define (get-vars bs)
  (match bs
    ['() '()]
    [(cons (binding v _) bs) (cons v (get-vars bs))]))

;; Get all of the _definitions_ from a list of bindings
;; [Binding] -> [Expr]
(define (get-defs bs)
  (match bs
    ['() '()]
    [(cons (binding _ def) bs) (cons def (get-defs bs))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (printers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have switched to using `#:transparent` above, so this should only be
;; necessary if you're desperate when debugging :'(

;; Given an AST, construct an sexpr that has the same shape
(define (ast->sexpr a)
  (match a
    [(int-e i)     `(int-e ,i)]
    [(bool-e b)    `(bool-e ,b)]
    [(var-e v)     `(var-e ,v)]
    [(prim-e p es) `(prim-e ,p ,@(map ast->sexpr es))]
    [(if-e e t f)  `(if-e ,(ast->sexpr e)
                          ,(ast->sexpr t)
                          ,(ast->sexpr f))]
    [(let-e bs b)  `(let-e ,(binding->sexpr bs) ,(ast->sexpr b))]))

(define (binding->sexpr bnds)
  (match bnds
    ['() '()]
    [(cons (binding v e) bnds) `((,v ,(ast->sexpr e)) ,@(binding->sexpr bnds))]))
