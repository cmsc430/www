#lang racket
(provide (all-defined-out))

(require "pretty-printer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; top-level graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A graph is its name along with its list of contents
; graph : String [Content] -> Graph
(struct graph (name conts) #:transparent)

; A subgraph is its name along with its list of contents
; subgraph : String [Content] -> Graph
(struct subgraph (name conts) #:transparent)

; type Content =
;   | Node
;   | Edge
;   | ToMany    ; Edge from one shared parent to many targets
;   | Attribute
;   | Subgraph

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Graph internal structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Nodes

; type Node =
;   | String                <- Just a node
;   | Symbol                <- Just a node
;   | (String, [Property])  <- A node and some property
;   | SubG                  <- Subgraph (i.e. a set of nodes)

; An edge defines the origin node and the target node by name
; edge : Node Node [Property] -> Content
(struct edge (orig targ props) #:transparent)

; When one origin has many targets
; edge : Node [Node] [Property] -> Content
(struct to-many (orig targs props) #:transparent)

; An attribute described whether it applies to the edges or
; nodes and also has a list of properties
; attr : Symbol [Property] -> Content
(struct attr (type props) #:transparent)

; A property is a parameter and its value
(struct prop (par val) #:transparent)

; Pair
(struct pair (a b) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Make an edge between two nodes with no properties
; e : Node Node -> Content
(define (e n1 n2)
  (edge n1 n2 '()))

; Make edges between a list of nodes
; es [Node] -> [Content]
(define (es ns)
  (match ns
    [(cons n1 (cons n2 '())) (cons (edge n1 n2 '()) '())]
    [(cons n1 (cons n2 ns))  (cons (edge n1 n2 '()) (es (cons n2 ns)))]
    [_  (error "There must be at least 2 nodes given to es")]))

(define (tm n1 ns)
  (to-many n1 ns '()))

; Set a node to a specific shape
; There are many, but we'll start with:
;  * box
;  * oval
;  * circle
;  * diamond
;  * rect
;
; shape : Symbol Node -> Node
(define (shape sh node)
  (match node
    [(? string? n)  (pair n (list (prop 'shape sh)))]
    [(? symbol? n)  (pair (symbol->string n) (list (prop 'shape sh)))]
    [(pair n props) (pair n (cons (prop 'shape sh) props))]))

; Attach meta-data to content, the parameter must be passed
; as a label
; attach : Symbol Symbol Content -> Content
(define (attach param st cont)
  (match cont
   ; Nodes
    [(? string? n)  (pair n (list (prop param st)))]
    [(? symbol? n)  (pair (symbol->string n) (list (prop param st)))]
    [(pair n props) (pair n (cons (prop param st) props))]
   ; Edges
    [(edge o t props)    (edge o t (cons (prop param st) props))]
    [(to-many o t props) (to-many o t (cons (prop param st) props))]
   ; Attribute
    [(attr t props) (attr t (cons (prop param st) props))]))

; Color some Content
; Known colors:
;  * red
;  * blue
;  * black
;  * darkgreen
;  * brown
;  * deeppink2
(define (color val cont)
  (attach 'color (string->symbol val) cont))

; Set the style of some contents
; There are many, but we'll start with:
;  * dashed
;  * dotted
;  * bold
;
; style : Symbol Content -> Content
(define (style st cont)
  (attach 'style st cont))

; Attach a label to Content
(define (label lab cont)
  (attach 'label (string->symbol lab) cont))

; Attach a label to Content
(define (label-str lab str)
  (attach 'label (string->symbol lab) (string->symbol str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Getters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-direct-name cont)
  (match cont
   ; Nodes
    [(? string? n)    (list n)]
    [(? symbol? n)    (list n)]
    [(pair n props)   (list n)]
   ; Edges
    [(edge o t props) '()]
    [(to-many o ts props) '()]
   ; Subgraph
    [(subgraph name cs) '()]
   ; Attribute
    [(attr t props)   '()]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pretty-Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ppr-graph : Graph -> Seq
(define (ppr-graph g)
  (match g
    [(graph n cs)
      (let ((header (+++ (str "digraph") (str n)))
            (body   (vert (map ppr-cont cs))))
           (+++ header (curl-ind body)))]))

; ppr-cont : Content -> Seq
(define (ppr-cont c)
  (match c
   ; Nodes
    [(? string? n)    (str n)]
    [(? symbol? n)    (sym n)]
    [(pair n props)   (end-semi (+++ (str n) (ppr-props props)))]
   ; Edges
    [(edge o t props) (end-semi (ppr-to-target o t props))]
    [(to-many o ts props)
      (vert (map (lambda (x) (end-semi (ppr-to-target o x props))) ts))]
   ; Subgraph
    [(subgraph name cs)
      (let ((header (+++ (str "subgraph") (str name)))
            (body   (vert (map (compose end-semi ppr-cont) cs))))
           (+++ header (curl-ind body)))]
   ; Attribute
    [(attr t props)   (end-semi (+++ (str t) (ppr-props props)))]))

; Helper function for above
(define (ppr-to-target o t props)
  (+++ (+-> (ppr-cont o) (ppr-cont t))
            (ppr-props props)))

; ppr-props : [Property] -> Seq
(define (ppr-props ps)
  (match ps
    ['() (nil)]
    [ps (sqr (comma-sep (map ppr-prop ps)))]))

; ppr-prop : Property -> Seq
(define (ppr-prop p)
  (match p
    [(prop 'label val) (+= (sym 'label) (qt (sym val)))]
    [(prop par val) (+= (sym par) (sym val))]))
