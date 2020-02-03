#lang racket
(struct leaf ())
(struct node (i left right))

(define (get-val n)
  (match n
    [(node i _ _) i]))

; Make a node with no children
(define (make-node i)
  (node i leaf leaf))

(define (take-two ns)
  (match ns
    ['()                   '()]
    [(cons a (cons b nss)) (cons
                             (node (+ (get-val a) (get-val b)) a b)
                             nss)]))

(define (make-inner-layer new-nodes old-nodes)
  (if (empty? old-nodes)
    new-nodes
    (let* ([taken (take-two old-nodes)]
           [ns (append new-nodes (list (car taken)))]
           [os (cdr taken)])
      (make-inner-layer ns os))))

(define (make-tree depth)
  (if (<= depth 0)
    leaf
    (letrec
      ; The values that will exist at the bottom of the tree
      ([final-nodes (map make-node (range 1 (+ 1 (expt 2 depth))))]
       [go (λ (prev-layer d)
              (if (<= d 1)
                prev-layer
                (let ([new-layer (make-inner-layer '() prev-layer)])
                  (go new-layer (- d 1)))))])
      (car (go final-nodes depth)))))


(provide (all-defined-out))
