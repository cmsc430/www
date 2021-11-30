#lang racket
(provide #;%provides
         list map length append memq append-map vector->list
         ;; Op0
         read-byte peek-byte void gensym
         ;; Op1
         add1; sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length
         symbol->string string->symbol symbol?         
         ;; Op2
         + - < = cons eq? make-vector vector-ref
         make-string string-ref string-append
         ;; Op3
         vector-set!)

(require (prefix-in % racket))
       

#;
(define (%provides)
  '(;; lists
    list map length append memq append-map vector->list
    ;; Op0
    read-byte peek-byte void gensym
    ;; Op1
    add1 sub1 zero? char? write-byte eof-object?
    integer->char char->integer
    box unbox empty? cons? box? car cdr
    vector? vector-length string? string-length
    symbol->string string->symbol symbol?         
    ;; Op2
    + - < = cons eq? make-vector vector-ref
    make-string string-ref string-append
    ;; Op3
    vector-set!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Op0
(define (read-byte) (%read-byte))
(define (peek-byte) (%peek-byte))
(define (void) (%void))
(define (gensym) (%gensym)) ; IMPROVE: add symbol/string argument

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Op1
(define (add1 n) (%add1 n))
(define (sub1 n) (%sub1 n))
(define (zero? n) (%zero? n))
(define (char? n) (%char? n))
(define (write-byte b) (%write-byte b)) ; IMPROVE: add port
(define (eof-object? x) (%eof-object? x))
(define (integer->char i) (%integer->char i))
(define (char->integer c) (%char->integer c))
(define (box x) (%box x))
(define (unbox x) (%unbox x))
(define (empty? x) (%empty? x))
(define (cons? x) (%cons? x))
(define (car x) (%car x))
(define (cdr x) (%cdr x))
(define (vector? x) (%vector? x))
(define (vector-length x) (%vector-length x))
(define (string? x) (%string? x))
(define (string-length x) (%string-length x))
(define (symbol->string x) (%symbol->string x))
(define (string->symbol x) (%string->symbol x))
(define (symbol? x) (%symbol? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Op2
(define (+ . xs)
  (match xs
    ['() 0]
    [(cons x xs)
     (%+ x (apply + xs))]))

(define -
  (case-lambda
    [(z) (%- 0 z)]
    [(x y) (%- x y)]
    [(x y z . zs)
     (apply - (%- x y) z zs)]))

(define <
  (case-lambda
    [(z) #t]
    [(x y . zs)
     (if (%< x y)
         (apply < y zs)
         #f)]))

(define =
  (case-lambda
    [(z) #t]
    [(x y . zs)
     (if (%= x y)
         (apply = y zs)
         #f)]))

(define (cons x y) (%cons x y))

(define (eq? x y) (%eq? x y))

(define make-vector
  (case-lambda
    [(size) (make-vector size 0)]
    [(size v) (%make-vector size v)]))

(define (vector-ref v i) (%vector-ref v i))

(define make-string
  (case-lambda
    [(k) (make-string k #\nul)]
    [(k c) (%make-string k c)]))

(define (string-ref s i)
  (%string-ref s i))

(define string-append
  (case-lambda
    [(x y) (%string-append x y)]
    [(x) (%string-append x "")]
    [(x . ys) (%string-append x (apply string-append ys))]
    [() ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Op3
(define (vector-set! v i x)
  (%vector-set! v i x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
(define (list . xs) xs)

(define (length xs)
  (match xs
    ['() 0]
    [(cons _ xs) (add1 (length xs))]))

(define map
  (case-lambda
    [(f xs) (map1 f xs)]
    [(f . xss) (mapn f xss)]))

(define (mapn f xss)
  (if (empty? (car xss))
      '()
      (cons (apply f (map1 (lambda (x) (car x)) xss))
            (mapn f (map1 (lambda (x) (cdr x)) xss)))))

(define (map1 f xs)
  (match xs
    ['() '()]
    [(cons x xs)
     (cons (f x) (map1 f xs))]))

(define (append . xss)
  (match xss
    ['() '()]
    [(cons '() xss)
     (apply append xss)]
    [(cons (cons x xs) xss)
     (cons x
           (apply append xs xss))]))

(define (memq v lst)
  (match lst
    ['() #f]
    [(cons l lst1)
     (if (eq? v l)
         lst
         (memq v lst1))]))

(define append-map
  (case-lambda
    [(f xs) (append-map1 f xs)]
    [(f . xss) (append-mapn f xss)]))

(define (append-map1 f xs)
  (match xs
    ['() '()]
    [(cons x xs)
     (append (f x) (append-map1 f xs))]))

(define (append-mapn f xss)
  (if (empty? (car xss))
      '()
      (append (apply f (map1 (lambda (x) (car x)) xss))
              (append-mapn f (map1 (lambda (x) (cdr x)) xss)))))
  
(define (vector->list v)
  (vector->list/a v (vector-length v) '()))

(define (vector->list/a v i a)
  (if (zero? i)
      a
      (vector->list/a v
                      (sub1 i)
                      (cons (vector-ref v (sub1 i)) a))))

