#lang racket

;; This is a sketch of a copying collector written in Racket

;; RVal ::=
;; | integer
;; | boolean
;; | char
;; | (list 'cons a)
;; | (list 'box a)
;; | (list 'str a)

;; SVal ::=
;; | rval
;; | integer   ; notice the overlap
;; | char

;; a ::= (list h i)

(define heap-size 10)
(define to   (make-vector heap-size))
(define from (make-vector heap-size))

(define *to-next*    0)
(define *curr*       0)
(define *type-queue* '())


;; [Listof RVal] -> [Listof RVal]
(define (collect roots)
  (set! *curr* 0)
  (set! *to-next* 0)
  (begin0 (move-roots roots)
          (move-all)
          (let ((tmp to))
            (set! to from)
            (set! from tmp))
          (set! *from-next* *to-next*)))

;; [Listof RVal] -> [Listof RVal]
;; EFFECT: shallowly moves data pointed to by roots to 'to' space,
;;         leaving fowarding address in 'from' space
(define (move-roots rs)
  (map move-root rs))

;; [Listof RVal] -> [Listof RVal]
;; EFFECT: shallowly moves data pointed to by root to 'to' space,
;;         leaving fowarding address in 'from' space
(define (move-root r)
  (match r
    [(list τ (list h i))
     (match (vector-ref h i)
       [(list _ (list (? to?) j)) ; fwd reference
        (list τ (list to j))]
       [_
        (begin0 (list τ (list to *to-next*))
                (move-obj τ i))])]
    ;; not a pointer
    [_ r]))

;; Type Index -> Void
;; EFFECT: Moves object of type τ at from-i to *to-next*
;; Pushes the type on the type queue so the moved object
;; can be interpreted appropriately later
;; If any objects were not word-aligned records,
;; this would need to be adapted
(define (move-obj τ from-i)
  (for ((i (size-of (list τ (list from from-i)))))
    (vector-set! to *to-next* (vector-ref from (+ from-i i)))
    (when (zero? i) ; fwd pointer
      (vector-set! from from-i (list τ (list to *to-next*))))     
    (set! *to-next* (add1 *to-next*)))
  (push! τ))

(define to? (λ (h) (eq? h to)))

;; -> Void
;; EFFECT: Move all objects starting from *curr*,
;; interpreting bits according to the type queue.
(define (move-all)
  (let loop ()
    (unless (= *curr* *to-next*)
      (move-curr)
      (loop))))

;; -> Void
;; EFFECT: Move object at *curr*, interpreting bits according
;; to front of type queue.
(define (move-curr)
  (let ((τ (pop!)))
    (match τ
      ['box  (scan-word)]
      ['cons (scan-word) (scan-word)]
      ['str  (set! *curr* (+ *curr* (add1 (vector-ref to *curr*))))])))

;; -> Void
;; *curr* is at the start of a value (i.e. a single word)
(define (scan-word)
  (match (vector-ref to *curr*)
    [(list τ (list from i))            
     (match (vector-ref from i)
       [(list _ (list (? to?) j)) ; fwd reference
        (vector-set! to *curr* (list τ (list to j)))]               
       [_
        (vector-set! to *curr* (list τ (list to *to-next*)))
        (move-obj τ i)])]
    [_ (void)])
  
  (set! *curr* (add1 *curr*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (size-of r)
  (match r
    [(list 'box  _) 1]
    [(list 'cons _) 2]
    [(list 'str  (list h i))
     (add1 (vector-ref h i))]))

(define (push! τ)
  (printf "pushing ~a\n" τ)
  (set! *type-queue*
        (append *type-queue* (list τ))))

(define (pop!)
  (let ((τ (car *type-queue*)))
    (printf "popping ~a\n" τ)
    (begin0 τ
            (set! *type-queue* (cdr *type-queue*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an example

(define roots
  (list (list 'cons (list from 0))
        (list 'cons (list from 0))))

(vector-set! from 0 (list 'str (list from 3)))
(vector-set! from 1 (list 'box (list from 6))) ;(list 'str (list from 3)))  ; sharing a string 
(vector-set! from 2 3) ; dead
(vector-set! from 3 2)
(vector-set! from 4 #\a)
(vector-set! from 5 #\b)
(vector-set! from 6 #\c)

(define *from-next* 7)
