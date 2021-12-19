#lang racket
(provide list list* make-list list? map foldr filter length append append*
         memq member append-map vector->list
         number->string gensym read read-char
         > <= >= void?
         char<=? char=?
         list->string string->list
         reverse
         remove-duplicates remq* remove* remove
         andmap vector list->vector boolean? substring
         odd?
         system-type
         not
         findf
         read-line
         char-alphabetic? char-whitespace?
         displayln ; only works for strings
         ; unimplemented
         exact->inexact / expt string->keyword
         ;; Op0
         read-byte peek-byte void
         ;; Op1
         add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length
         symbol->string string->symbol symbol?
         string->uninterned-symbol open-input-file
         write-char error integer?
         eq-hash-code
         ;; Op2
         + - < = cons eq? make-vector vector-ref
         make-string string-ref string-append
         quotient remainder set-box!
         bitwise-and bitwise-ior bitwise-xor arithmetic-shift
         ;; Op3
         vector-set!)

(require (prefix-in % racket)
         (rename-in racket [read-byte %read-byte-port]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Op0
(define read-byte
  (case-lambda
    [() (%read-byte)]
    [(p) (%read-byte-port p)]))  ;; not a racket function!

;(define (peek-byte) (%peek-byte))
(define peek-byte
  (case-lambda
    [()
     (%peek-byte (%current-input-port) 0)]
    [(p off)
     (%peek-byte p off)]))

(define (void . xs) (%void))

(define (current-input-port) (%current-input-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Op1
(define (add1 n) (%add1 n))
(define (sub1 n) (%sub1 n))
(define (zero? n) (%zero? n))
(define (char? n) (%char? n))
(define (write-byte b) (%write-byte b)) ; IMPROVE: add port
(define (write-char c) (%write-char c))
(define (eof-object? x) (%eof-object? x))
(define (integer->char i) (%integer->char i))
(define (char->integer c) (%char->integer c))
(define (box x) (%box x))
(define (box? x) (%box? x))
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
(define (string->uninterned-symbol x) (%string->uninterned-symbol x))
(define (open-input-file x) (%open-input-file x))
(define (error . x) (%error (car x))) ;; drops other args
(define (integer? x) (%integer? x))
(define (eq-hash-code x) (%eq-hash-code x))

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

(define <=
  (case-lambda
    [(z) #t]
    [(x y . zs)
     (if (%< y x)
         #f
         (apply <= y zs))]))

(define >
  (case-lambda
    [(z) #t]
    [(x y . zs)
     (if (%< y x)
         (apply > y zs)
         #f)]))

(define >=
  (case-lambda
    [(z) #t]
    [(x y . zs)
     (if (%< x y)
         #f
         (apply >= y zs))]))

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

(define (quotient x y) (%quotient x y))
(define (remainder x y) (%remainder x y))
(define (set-box! x y) (%set-box! x y))
(define (bitwise-and x y) (%bitwise-and x y)) ;; should be n-ary
(define (bitwise-ior x y) (%bitwise-ior x y)) ;; should be n-ary
(define (bitwise-xor x y) (%bitwise-xor x y)) ;; should be n-ary
(define (arithmetic-shift x y) (%arithmetic-shift x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Op3
(define (vector-set! v i x)
  (%vector-set! v i x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (length xs)
  (match xs
    ['() 0]
    [(cons _ xs) (add1 (length xs))]))

(define (reverse xs)
  (reverse/a xs '()))

(define (reverse/a xs ys)
  (match xs
    ['() ys]
    [(cons x xs)
     (reverse/a xs (cons x ys))]))

(define (equal? x y)
  (error "equal? is not defined"))

(define member
  (case-lambda
    [(v lst) (member v lst equal?)]
    [(v lst is-equal?)
     (match lst
       ['() #f]
       [(cons l lst1)
        (if (is-equal? v l)
            lst
            (member v lst1 is-equal?))])]))

(define remove-duplicates
  (case-lambda
    [(xs) (remove-duplicates xs equal?)]
    [(xs eq)
     (remove-duplicates/a xs eq '())]))

(define (remove-duplicates/a xs eq seen)
  (match xs
    ['() (reverse seen)]
    [(cons x xs)
     (if (member x seen eq)
         (remove-duplicates/a xs eq seen)
         (remove-duplicates/a xs eq (cons x seen)))]))

(define (remq* v-list lst)
  (match v-list
    ['() lst]
    [(cons v v-list)
     (remq* v-list (remove* v lst eq?))]))

(define remove*
  (case-lambda
    [(x xs) (remove* x xs equal?)]
    [(x xs eq)
     (match xs
       ['() '()]
       [(cons y xs)
        (if (eq x y)
            (remove* x xs eq)
            (cons y (remove* x xs eq)))])]))

(define (remove x xs eq)
  (match xs
    ['() '()]
    [(cons y xs)
     (if (eq x y)
         xs
         (cons y (remove x xs eq)))]))

(define (andmap f xs)
  (match xs
    ['() #t]
    [(cons x xs)
     (and (f x)
          (andmap f xs))]))

(define (list->vector xs)
  (list->vector/a (make-vector (length xs) 0) 0 xs))

(define (list->vector/a v i xs)
  (match xs
    ['() v]
    [(cons x xs)
     (begin
       (vector-set! v i x)
       (list->vector/a v (add1 i) xs))]))

(define (vector . xs)
  (list->vector xs))

(define (boolean? x)
  (or (eq? x #t)
      (eq? x #f)))

(define (list->string xs)
  (match xs
    ['() ""]
    [(cons c cs)
     (string-append (make-string 1 c)
                    (list->string cs))]))

(define substring
  (case-lambda
    [(str start) (substring str start (string-length str))]
    [(str start end)
     (substring/a str start end '())]))

(define (substring/a str start end cs)
  (if (= start end)
      (list->string cs)
      (substring/a str start (sub1 end)
                   (cons (string-ref str (sub1 end)) cs))))

(define (odd? x)
  (= (remainder x 2) 1))

(define (system-type)
  ;; the primitive system type returns 1 for mac, 0 otherwise;
  ;; the fall through case is for when %system-type is implemented in Racket
  (match (%system-type)
    [1 'macosx]
    [0 'unix]
    [x x]))

(define (not x)
  (if x #f #t))

(define (findf proc xs)
  (match xs
    ['() #f]
    [(cons x xs)
     (if (proc x)
         x
         (findf proc xs))]))

(define (char<=? c . cs)
  (char-compare <= (char->integer c) cs))

(define (char=? c . cs)
  (char-compare = (char->integer c) cs))

(define (char-compare cmp d cs)
  (match cs
    ['() #t]
    [(cons c cs)
     (let ((d1 (char->integer c)))
       (if (cmp d d1)
           (char-compare cmp d1 cs)
           #f))]))


(define (string->list s)
  (string->list/a s (string-length s) '()))

(define (string->list/a s n xs)
  (if (zero? n)
      xs
      (string->list/a s (sub1 n)
                      (cons (string-ref s (sub1 n)) xs))))

(define (void? x)
  (eq? x (void)))

(define (list . xs) xs)

(define (list* x . xs)
  (dot-last x xs))

(define (dot-last x xs)
  (match xs
    ['() x]
    [(cons y xs)
     (cons x (dot-last y xs))]))

(define (make-list n x)
  (if (zero? n)
      '()
      (cons x (make-list (sub1 n) x))))

(define (list? xs)
  (match xs
    ['() #t]
    [(cons x xs)
     (list? xs)]
    [_ #f]))

;; should really take any number of xss
(define (foldr f b xs)
  (match xs
    ['() b]
    [(cons x xs)
     (f x (foldr f b xs))]))

(define (filter p xs)
  (match xs
    ['() '()]
    [(cons x xs)
     (if (p x)
         (cons x (filter p xs))
         (filter p xs))]))

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
    [(cons x '()) x]
    [(cons '() xss)
     (apply append xss)]
    [(cons (cons x xs) xss)
     (cons x
           (apply append xs xss))]))

(define (append* xs xss) ; only binary case
  (apply append xs xss))

(define (memq v lst)
  (member v lst eq?))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gensym-counter (box 0))

(define gensym
  (case-lambda
    [() (gensym "g")]
    [(s)
     (let ((i (unbox gensym-counter)))
       (begin (set-box! gensym-counter (add1 i))
              (string->uninterned-symbol
               (string-append (if (string? s)
                                  s
                                  (symbol->string s))
                              (number->string i)))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Integer -> String
;; Only works for integers

(define number->string
  (case-lambda
    [(n) (number->string n 10)]
    [(n radix)
     (if (< n 0)
         (string-append "-" (nat->string (- n) "" radix))
         (nat->string n "" radix))]))

(define (nat->string n m radix)
  (if (< n radix)
      (string-append (digit->string n radix) m)
      (nat->string (quotient n radix)
                   (string-append (digit->string (remainder n radix) radix) m)
                   radix)))

(define (digit->string n radix)
  (if (= radix 16)
      (hex-digit->string n)
      (make-string 1 (integer->char (+ (char->integer #\0) n)))))

(define (hex-digit->string n)
  (match n
    [10 "a"]
    [11 "b"]
    [12 "c"]
    [13 "d"]
    [14 "e"]
    [15 "f"]
    [_ (digit->string n 10)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-char)
   (let ((b (read-byte)))
     (if (eof-object? b)
         b
         (integer->char
          (if (< b 128)
              b
              (if (>= b 240)
                  (+ (arithmetic-shift (bitwise-and b #b111) 18)
                     (arithmetic-shift (bitwise-and (read-byte) #b111111) 12)
                     (arithmetic-shift (bitwise-and (read-byte) #b111111) 6)
                     (bitwise-and (read-byte) #b111111))
                  (if (>= b 224)
                      (+ (arithmetic-shift (bitwise-and b #b1111) 12)
                         (arithmetic-shift (bitwise-and (read-byte) #b111111) 6)
                         (bitwise-and (read-byte) #b111111))
                      (if (>= b 192)
                          (+ (arithmetic-shift (bitwise-and b #b11111) 6)
                             (bitwise-and (read-byte) #b111111))
                          (error "bad bytes")))))))))

(define (peek-char)
   (let ((b (peek-byte)))
     (if (eof-object? b)
         b
         (integer->char
          (if (< b 128)
              b
              (if (>= b 240)
                  (+ (arithmetic-shift (bitwise-and b #b111) 18)
                     (arithmetic-shift (bitwise-and (peek-byte (%current-input-port) 1) #b111111) 12)
                     (arithmetic-shift (bitwise-and (peek-byte (%current-input-port) 2) #b111111) 6)
                     (bitwise-and (peek-byte (%current-input-port) 3) #b111111))
                  (if (>= b 224)
                      (+ (arithmetic-shift (bitwise-and b #b1111) 12)
                         (arithmetic-shift (bitwise-and (peek-byte (%current-input-port) 1) #b111111) 6)
                         (bitwise-and (peek-byte (%current-input-port) 2) #b111111))
                      (if (>= b 192)
                          (+ (arithmetic-shift (bitwise-and b #b11111) 6)
                             (bitwise-and (peek-byte (%current-input-port) 1) #b111111))
                          (error "bad bytes")))))))))

(define (read-line)
  (read-line/a '()))

(define (read-line/a cs)
  (let ((c (read-char)))
    (if (or (eof-object? c) (eq? c #\newline))
        (list->string (reverse cs))
        (read-line/a (cons c cs)))))

(define (char-alphabetic? x) (%char-alphabetic? x))
(define (char-whitespace? x) (%char-whitespace? x))

(define (displayln s)
  (if (string? s)
      (begin (map write-char (string->list s))
             (write-char #\newline))
      (error "unimplemented displayln for non-strings")))

(define (exact->inexact x)
  (error "exact->inexact not implemented"))

(define (/ x y)
  (error "/ not implemented"))

(define (expt n m)
  (error "expt not implemented"))

(define (string->keyword s)
  (error "string->keyword not implemented"))
