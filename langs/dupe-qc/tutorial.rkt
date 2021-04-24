#lang racket
(provide (all-defined-out))
(require quickcheck quickcheck/generator
         quickcheck/private/random)

; Consider the following short function [list-delete] that takes a
;    natural number [x] and a list of nats [l] and proceeds to remove
;    [x] from the list. While one might be tempted to pose the question
;    "Is there a bug in this definition?", such a question has little
;    meaning without an explicit specification. Here, [delete-removes-every-x]
;    requires that after removing [x] from [l], the resulting list does
;    not contain any occurences of [x]. 

(define (list-delete x l)
  (match l
    ['() '()]
    [(cons h t) (if (= x h) t (cons h (list-delete x t)))]))

(define delete-removes-every-x
    (property ([x arbitrary-integer]
               [l (arbitrary-list arbitrary-integer)])
              (not (member x (list-delete x l)))))

; For this simple example, it is not hard to "spot" the bug by
;    inspection. We will use quickcheck to find out what is wrong.
; 
;    quickcheck provides a toplevel command [quickcheck] that receives
;    as input an executable property and attempts to falsify it. 

(displayln "Checking delete-removes-every-x...")
(quickcheck delete-removes-every-x)
;; Falsifiable, after 1 tests:
;; x = 1 l = (1 1)

; Property Based Random Testing Ingredients 
; 
;    There are four basic ingredients in property based random testing:
; 
;    - An executable property, as discussed above
;    - A printer, to report counterexamples found
;    - A generator, to produce random inputs 
;    - A shrinker, to reduce counterexamples.
; 
;    We will now review the latter three in order. 

; Printing

; We can leverage racket's #:transparent/#:prefab to print our own structures, so not much to say here.

(struct Red   () #:prefab)
(struct Green () #:prefab)
(struct Blue  () #:prefab)

(displayln (Red))
;; #s(Red)
(displayln (Blue))
;; #s(Blue)

; Generators

; In racket's quickcheck, a generator is just a wrapper
; around a function that takes...
; - An integer controlling the size of generated things
; - A pseudo-random number generator
; ... and returns a racket value.

; (struct generator (proc)
;  #:extra-constructor-name make-generator)
;  proc : (-> integer? random-generator? any/c)

; The simplest generator is one for integers, which
; picks a value between two extremes.

; (choose-integer lower upper) → generator?
;  lower : integer?
;  upper : integer?

; We can use [generate] to produce random values from
; a generator.

(generate 10
          (make-random-generator 1)
          (choose-integer 0 10))
;; 0

; We can create a shorthand to try out different-values
; using a generator seeded by the current time.

(define (gen sz g)
  (generate sz
            (make-random-generator (current-milliseconds))
            g))

(gen 10 (choose-integer 0 10))


; Other primitives exist: 

; (choose-char lower upper) → generator?
;  lower : char?
;  upper : char?

; (choose-list elem-gen size) → generator?
;  elem-gen : generator?
;  size : integer?

; (choose-string char-gen size) → generator?
;  char-gen : generator?
;  size : integer?



; What about custom structs?

; (generator-unit val) → generator?
;  val : any/c

; (choose-one-of opts) → generator?
;  opts : (listof any/c)

(define (gen-green) (generator-unit (Green)))
(gen 10 (gen-green))

(define (gen-color)
  (choose-one-of '((Red) (Green) (Blue))))
(gen 10 (gen-color))

; What about non-enumerations?

; Consider binary trees:

(struct Leaf () #:prefab)
(struct Node (x l r) #:prefab)

(displayln (Node 42 (Leaf) (Leaf)))

; (choose-with-frequencies freqs) → generator?
;  freqs : (listof (cons/c integer? generator?))
;
; (bind-generators ([id val-expr] ...) body)

(define (gen-tree-int)
  (choose-with-frequencies
   (list (cons 1 (generator-unit (Leaf)))
         (cons 1 (bind-generators
                  ([x (choose-integer 0 10)]
                   [l (gen-tree-int)]
                   [r (gen-tree-int)])
                  (Node x l r))))))

(gen 10 (gen-tree-int))

; Why does this terminate?
; What is the purpose of this '10'?

; (sized f) → generator?
;  f : (-> integer? generator?)

(define (gen-tree-int-sized-aux sz)
  (if (= sz 0) (generator-unit (Leaf))
      (choose-with-frequencies
       (list (cons 1 (generator-unit (Leaf)))
             (cons sz (bind-generators
                       ([x (choose-integer 0 10)]
                        [l (gen-tree-int-sized-aux (sub1 sz))]
                        [r (gen-tree-int-sized-aux (sub1 sz))])
                       (Node x l r)))))))
(define (gen-tree-int-sized)
  (sized gen-tree-int-sized-aux))

(gen 10 (gen-tree-int-sized))

; Let's try it out!

(define (mirror t)
  (match t
    [(Leaf) (Leaf)]
    [(Node x l r) (Node x (mirror r) (mirror l))]))

(define (tree-equal? t1 t2)
  (match* (t1 t2)
    [((Leaf) (Leaf)) #t]
    [((Node x1 l1 r1) (Node x2 l2 r2))
     (and (= x1 x2)
          (tree-equal? l1 l2)
          (tree-equal? r1 r2))]
    [(_ _) #f]))

(define mirror-prop
  (property ([t (gen-tree-int-sized)])
            (tree-equal? t (mirror t))))

(quickcheck mirror-prop)

;; Counterexample too large? Shrinking to the rescue!
; 
;(define (tree-shrink v gen) (variant (* 2 v) gen))
; 
;(define tree-arb (arbitrary (gen-tree-int-sized) tree-shrink))
; 
;(define mirror-prop-arb
;  (property ([t tree-arb])
;            (tree-equal? t (mirror t))))
; 
;(quickcheck mirror-prop-arb)

