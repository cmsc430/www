#lang racket
(provide (all-defined-out))

;; type Prog = [FunDef] Expr

;; type FunDef = Variable [Variable] Expr

;; type Expr =
;; | Integer
;; | Boolean
;; | Character
;; | Variable
;; | Prim1 Expr
;; | Prim2 Expr Expr
;; | Lam Name [Variable] Expr   <--- New for Loot
;; | App Expr [Expr]            <--- Changed for Loot
;; | If Expr Expr Expr
;; | Let (Binding list) Expr
;; | LetRec (Binding list) Expr <--- New for Loot (See the lecture notes!)
;; | Nil

;; Note: Fun and Call, from Knock, are gone!
;; They have been made redundant by the combination
;; of Lam (which is new) and App (which has been modified)

;; type Prim1 = 'add1 | 'sub1 | 'zero? | box | unbox | car | cdr
;; type Prim2 = '+ | '- | cons

;; type Binding = Variable Expr

;; type Variable = Symbol (except 'add1 'sub1 'if, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The represenation of top-level programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct prog (ds e) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The represenation of a function definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A FunDef has a symbol for the function's name,
;; a list of symbols representing the names of the function's
;; arguments, and one expression that forms the body of the function.
(struct fundef (name args body) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The Expr data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Expr can be viewed as having 'kinds' of nodes.
;;
;; * The nodes that represnt an expression themselves
;;
;; * The nodes that are part of an expression, but no an expression themselves

;; The below are the former:

(struct int-e  (i)     #:transparent)
(struct bool-e (b)     #:transparent)
(struct char-e (c)     #:transparent)
(struct var-e  (v)     #:transparent)
(struct prim-e (p es)  #:transparent)
(struct lam-e  (vs es) #:transparent) ; <- new for Loot
(struct lam-t  (n vs es) #:transparent) ; <- new for Loot (for when we tag the lambdas)
(struct app-e  (f es)  #:transparent) ; <- changed for Loot
(struct if-e   (e t f) #:transparent)
(struct let-e  (bs b)  #:transparent)
(struct letr-e (bs b)  #:transparent) ; <- new for Loot
(struct nil-e  ()      #:transparent)

;; The next is the latter:

;; A binding holds a symbol representing the bound variable and
;; Expr that represents the value that will be bound to that variable
(struct binding (v e) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (predicates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unops '(add1 sub1 zero? box unbox empty? car cdr))
(define biops '(+ - cons))

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
;;;;;; AST utility functions (maps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bindings-map-def f bs)
  (match bs
    ['() '()]
    [(cons (binding n def) bs)
      (cons (binding n (f def)) (bindings-map-def f bs))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (printers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have switched to using `#:transparent` above, so this should only be
;; necessary if you're desperate when debugging :'(

;; Given a Program, construct an sexpr that has the same shape
(define (prog-debug p)
  (match p
    [(prog ds e) `(prog ,(map fundef-debug ds) ,(ast-debug e))]))

;; Given a FunDef, construct an sexpr that has the same shape
(define (fundef-debug def)
  (match def
    [(fundef name args body) `(fundef ,name ,args ,(ast-debug body))]))

;; Given an AST, construct an sexpr that has the same shape
(define (ast-debug a)
  (match a
    [(int-e i)     `(int-e ,i)]
    [(bool-e b)    `(bool-e ,b)]
    [(char-e c)    `(char-e ,c)]
    [(var-e v)     `(var-e ,v)]
    [(nil-e)       ''()]
    [(prim-e p es) `(prim-e ,p ,@(map ast-debug es))]
    [(lam-t n vs e)`(lam-t ,n ,vs ,(ast-debug e))]
    [(lam-e vs e)  `(lam-e ,vs ,(ast-debug e))]
    [(app-e f es)  `(app-e ,(ast-debug f) ,@(map ast-debug es))]
    [(if-e e t f)  `(if-e ,(ast-debug e)
                          ,(ast-debug t)
                          ,(ast-debug f))]
    [(let-e bs b)  `(let-e ,(binding-debug bs) ,(ast-debug b))]
    [(letr-e bs b) `(letr-e ,(binding-debug bs) ,(ast-debug b))]))

(define (binding-debug bnds)
  (match bnds
    ['() '()]
    [(cons (binding v e) bnds) `((,v ,(ast-debug e)) ,@(binding-debug bnds))]))
