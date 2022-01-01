#lang racket
(provide list list* make-list list? map foldr filter length append append*
         memq member append-map vector->list
         number->string gensym read read-char peek-char
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
         * ; limited
         char-alphabetic? char-whitespace?
         displayln ; only works for strings
         write-string
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
         write-char error integer? procedure?
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
(define (procedure? x) (%procedure? x))
(define (eq-hash-code x) (%eq-hash-code x))

(define (* x y)
  (match x
    [0 0]
    [1 y]
    [2  (arithmetic-shift y 1)]
    [4  (arithmetic-shift y 2)]
    [8  (arithmetic-shift y 3)]
    [10 ; 10a=2^3a+2a
     (+ (arithmetic-shift y 1)
        (arithmetic-shift y 3))]
    [16 (arithmetic-shift y 4)]
    [64 (arithmetic-shift y 6)]
    [_ (error "unimplemented multiplication")]))


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
    ;; the use of string->symbol here is to avoid subtle issues about symbol interning
    ;; in separately compiled libraries
    [1 (string->symbol "macosx")]
    [0 (string->symbol "unix")]
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
      (begin (write-string s)
             (write-char #\newline))
      (error "unimplemented displayln for non-strings")))

(define (write-string s)
  (begin (map write-char (string->list s))
         (string-length s)))

(define (exact->inexact x)
  (error "exact->inexact not implemented"))

(define (/ x y)
  (error "/ not implemented"))

(define (expt n m)
  (error "expt not implemented"))

(define (string->keyword s)
  (error "string->keyword not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read.rkt

;; -> Any
;; Read an s-expression from given port
(define (read)
  (let ((r (<start>)))
    (if (err? r)
        (error (err-msg r))
        r)))

(struct err (msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start

(define (<start>)
  (match (peek-char)
    [(? eof-object?) (read-char)]
    [(? char-whitespace?) (begin (read-char) (<start>))]
    [#\; (begin (<line-comment>) (<start>))]
    [#\# (begin (read-char)
                (match (peek-char)
                  [#\|
                   (begin (read-char)
                          (let ((r (<block-comment>)))
                            (if (err? r) r (<start>))))]
                  [#\; (read-char)
                   (let ((r (<elem>)))
                     (if (err? r) r (<start>)))]
                  [_ (<octo>)]))]
    [_   (<elem>)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elem

(define (<elem>)
  (match (read-char)
    [(? eof-object?) (err "eof")]
    [(? char-whitespace?) (<elem>)]
    [#\| (<symbol-escape>)]
    [#\" (<string-start-chars> '())]
    [#\# (<octo-elem>)]
    [(? open-paren? c) (<list-or-pair> c)]
    [#\; (<line-comment>) (<elem>)]
    [#\' (<quote> (string->symbol "quote"))]
    [#\` (<quote> (string->symbol "quasiquote"))]
    [#\, (match (peek-char)
           [#\@ (read-char) (<quote> (string->symbol "unquote-splicing"))]
           [_ (<quote> (string->symbol "unquote"))])]
    [c   (<number-or-symbol> c)]))

(define (<quote> q)
  (let ((r (<elem>)))
    (if (err? r)
        r
        (list q r))))

(define (<octo-elem>)
  (match (peek-char)
    [#\| (read-char)
         (let ((r (<block-comment>)))
           (if (err? r) r (<elem>)))]
    [#\; (read-char)
         (let ((r (<elem>)))
           (if (err? r) r (<elem>)))]
    [_ (<octo>)]))

(define (<octo>)
  (match (read-char)
    [(? eof-object?) (err "bad syntax `#`")]
    [#\T (committed-delim '() #t)]
    [#\F (committed-delim '() #f)]  ; could also be #Fl
    [#\t (if (delim?) #t (committed-delim '(#\r #\u #\e) #t))]
    ;; could also be #fl
    [#\f (if (delim?) #f (committed-delim '(#\a #\l #\s #\e) #f))]
    [#\( (<vector> #\()]
    [#\[ (<vector> #\[)]
    [#\{ (<vector> #\{)]
    [#\s (unimplemented "structure")]
    [#\\ (<char-start>)]
    [#\: (<keyword>)]
    [#\& (unimplemented "boxes")] ; FIXME
    [#\' (<quote> (string->symbol "syntax"))]
    [#\! (unimplemented "shebang comment")]
    [#\` (<quote> (string->symbol "quasisyntax"))]
    [#\, (match (peek-char)
           [#\@ (read-char) (<quote> (string->symbol "unsyntax-splicing"))]
           [_ (<quote> (string->symbol "unsyntax"))])]
    [#\~ (unimplemented "compiled code")]
    [#\i (unimplemented "inexact number")]
    [#\I (unimplemented "inexact number")]
    [#\e (unimplemented "exact number")]
    [#\E (unimplemented "exact number")]
    [#\b (<general-numbern> char-digit2? char-digit2s->number)]
    [#\B (<general-numbern> char-digit2? char-digit2s->number)]
    [#\o (<general-numbern> char-digit8? char-digit8s->number)]
    [#\O (<general-numbern> char-digit8? char-digit8s->number)]
    [#\d (<general-numbern> char-digit10? char-digit10s->number)]
    [#\D (<general-numbern> char-digit10? char-digit10s->number)]
    [#\x (<general-numbern> char-digit16? char-digit16s->number)]
    [#\X (<general-numbern> char-digit16? char-digit16s->number)]
    [#\< (<here-string>)]
    [#\r (unimplemented "regexp or reader")]
    [#\p (unimplemented "pregexp")]
    [#\c (unimplemented "case switch")]
    [#\C (unimplemented "case switch")]
    [#\h (unimplemented "hash")]
    [(? char-digit10?) (unimplemented "vector or graph")]
    [_ (err "bad syntax")]))


(define (<here-string>)
  (unimplemented "here string"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers

;; have seen '#b', '#o', etc.
;; simplified to just digits
(define (<general-numbern> char-digitn? char-digitsn->number)
  (match (read-char)
    [#\#
     (match (read-char)
       [#\e (<digitn>+ char-digitn?)]
       [#\i (unimplemented "inexact")]
       [_ (err "error")])]
    [#\+ (read-char) (<digitn>+)]
    [#\- (read-char) (- (<digitn>+ char-digitn?))]
    [(? char-digitn? c) (<digitn>* (list c) char-digitn? char-digitsn->number)]
    [_ (err "error")]))

(define (<digitn>+ char-digitn?)
  (match (read-char)
    [(? char-digitn? c)  (<digitn>* (list c))]
    [_ (err "error")]))

(define (<digitn>* ds char-digitn? char-digitsn->number)
  (if (delim?)
      (char-digitsn->number ds)
      (match (read-char)
        [(? char-digitn? c) (<digitn>* (cons c ds) char-digitn? char-digitsn->number)]
        [_ (err "error")])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers or Symbols

;; Numbers are simplified significantly:

;; <number>   ::= ['+' | '-] <unsigned>
;; <unsigned> ::= '.' <digit>+
;;             |  <digit> <digit>* ['.' <digit>*]

;; whenver something else is encounter, parse as a symbol

(define (<number-or-symbol> c)
  (match c
    [#\+ (if (delim?) (string->symbol "+") (<unsigned-or-symbol> #\+ '()))]
    [#\- (if (delim?) (string->symbol "-") (<unsigned-or-symbol> #\- '()))]
    [#\. (if (delim?) (err ".") (<frac> #f '() '()))]
    [(? char-digit10?) (<unsigned-or-symbol> #f (list c))]
    [_   (<symbol> (list c))]))

(define (<unsigned-or-symbol> signed? whole)
  (match (peek-char)
    [(? eof-object?) (make-whole signed? whole)]
    [(? char-delim?) (make-whole signed? whole)]
    [#\. (read-char) (<frac> signed? whole '())]
    [(? char-digit10? d)
     (read-char)
     (<unsigned-or-symbol> signed? (cons d whole))]
    [_ (<symbol> (cons (read-char)
                       (append whole (if signed? (list signed?) '())))
                )]))

(define (<frac> signed? whole frac)
  (match (peek-char)
    [(? eof-object?) (make-frac signed? whole frac)]
    [(? char-delim?) (make-frac signed? whole frac)]
    [(? char-digit10?) (<frac> signed? whole (cons (read-char) frac))]
    [_ (<symbol> (cons (read-char)
                       (append frac
                               (list #\.)
                               whole
                               (if signed? (list signed?) '())))
                )]))

(define (make-frac signed? whole frac)
  (match (cons whole frac)
    [(cons '() '()) (chars->symbol (list #\. signed?))]
    [(cons _ _)
     (exact->inexact
      (match signed?
        [#\- (- (frac->number whole frac))]
        [_ (frac->number whole frac)]))]))


(define (frac->number whole frac)
  (+ (char-digit10s->number whole)
     (/ (char-digit10s->number frac)
        (expt 10 (length frac)))))

(define (make-whole signed? ds)
  (match signed?
    [#\- (- (char-digit10s->number ds))]
    [_      (char-digit10s->number ds)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line comment

(define (<line-comment>)
  (let ((c (read-char)))
    (or (eof-object? c)
        (and (memq c '(#\newline #\return #\u0085 #\u2028 #\u2029)) #t)
        (<line-comment>))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block comment

(define (<block-comment>)
  (match (read-char)
    [(? eof-object?) (err (string-append "unbalanced |" "#"))]
    [#\# (match (peek-char)
           [#\| (let ((r (<block-comment>)))
                  (if (err? r) r (<block-comment>)))]
           [_ (<block-comment>)])]
    [#\| (match (peek-char)
           [#\# (read-char) #t]
           [_ (<block-comment>)])]
    [_ (<block-comment>)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors

(define (<vector> paren)
  (let ((r (<list-or-pair> paren)))
    (if (err? r)
        r
        (if (list? r)
            (list->vector r)
            (err "dotted list in vector")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists or pairs

(define (<list-or-pair> paren)
  (match (peek-char)
    [(? eof-object?) (err "missing! )")]
    [(? char-whitespace?) (read-char) (<list-or-pair> paren)]
    [#\; (<line-comment>) (<list-or-pair> paren)]
    [(? close-paren? c)
     (read-char)
     (if (opposite? paren c) '() (err "mismatched paren"))]
    [#\# (read-char)
         (match (peek-char)
           [#\| (read-char)
                (let ((r (<block-comment>)))
                  (if (err? r) r (<list-or-pair> paren)))]
           [#\; (read-char)
                (let ((r (<elem>)))
                  (if (err? r) r (<list-or-pair> paren)))]
           [_ (let ((r (<octo>)))
                (if (err? r) r (<elem><list-or-pair> paren (list r))))])]
    [_  (let ((r (<elem>)))
          (if (err? r)
              r
              (<elem><list-or-pair> paren (list r))))]))

(define (<elem><list-or-pair> paren xs)
  (match (peek-char)
    [(? eof-object?) (err "missing!! )")]
    [(? char-whitespace?) (read-char) (<elem><list-or-pair> paren xs)]
    [#\; (<line-comment>) (<elem><list-or-pair> paren xs)]
    [(? close-paren? c)
     (read-char)
     (if (opposite? paren c) (reverse xs) (err "mismatched paren"))]
    [#\# (read-char)
         (match (peek-char)
           [#\| (read-char)
                (let ((r (<block-comment>)))
                  (if (err? r) r (<elem><list-or-pair> paren xs)))]
           [#\; (read-char)
                (let ((r (<elem>)))
                  (if (err? r) r (<elem><list-or-pair> paren xs)))]
           [_ (let ((r (<octo>)))
                (if (err? r) r (<elem><list-or-pair> paren (cons r xs))))])]
    [#\. (read-char)
         (if (delim?)
             (<dotted-list> paren xs)
             (<elem><list-or-pair> paren (cons (<frac> #f '() '()) xs)))]
    [_  (let ((r (<elem>)))
          (if (err? r)
              r
              (<elem><list-or-pair> paren (cons r xs))))]))

(define (<dotted-list> paren xs)
  (let ((r (<elem>)))
    (if (err? r)
        r
        (<dotted-list-close> paren (append* (reverse xs) (list r))))))

(define (<dotted-list-close> paren xs)
  (match (read-char)
    [(? char-whitespace?) (<dotted-list-close> paren xs)]
    [#\; (<line-comment>) (<dotted-list-close> paren xs)]
    [#\# (match (peek-char)
           [#\| (read-char)
                (let ((r (<block-comment>)))
                  (if (err? r) r (<dotted-list-close> paren xs)))]
           [#\; (read-char)
                (let ((r (<elem>)))
                  (if (err? r) r (<dotted-list-close> paren xs)))]
           [_ (err "unexpected")])]
    [(? close-paren? c)
     (if (opposite? paren c) xs (err "mismatched paren"))]
    [_ (err "uneasdfasdxpected")]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols and Keywords

(define (<symbol> cs)
  (let ((r (<symbol-chars> cs)))
    (if (err? r)
        r
        (chars->symbol r))))

(define (<keyword>)
  (let ((r (<symbol-chars> '())))
    (if (err? r)
        r
        (chars->keyword r))))

(define (<symbol-escape>)
  (let ((r (<symbol-escape-chars> '())))
    (if (err? r)
        r
        (chars->symbol r))))

;; Assume: what we've seen tells us this is a symbol, cs are the chars of the
;; symbol seen so far in reverse order
(define (<symbol-chars> cs)
  (if (delim?)
      cs
      (match (peek-char)
        [#\\ (read-char) (<symbol-single-escape-chars> cs)]
        [#\| (read-char) (<symbol-escape-chars> cs)]
        [_ (<symbol-chars> (cons (read-char) cs))])))

(define (<symbol-single-escape-chars> cs)
  (match (read-char)
    [(? eof-object?) (err "read: end-of-file following `\\` in symbol")]
    [c (<symbol-chars> (cons c cs))]))

(define (<symbol-escape-chars> cs)
  (match (read-char)
    [(? eof-object?) (err "read: end-of-file following `|` in symbol")]
    [#\| (<symbol-chars> cs)]
    [c (<symbol-escape-chars> (cons c cs))]))

(define (chars->symbol cs)
  (string->symbol (list->string (reverse cs))))

(define (chars->keyword cs)
  (string->keyword (list->string (reverse cs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters

;; Assume: have already read '#\'
(define (<char-start>)
  (let ((c (read-char)))
    (cond
      [(eof-object? c) (err "error")]
      [(eof-object? (peek-char)) c]
      [(char-digit8? c) (<char-start><digit8> c)]
      [(not-char-alphabetic? c) c]
      [else
       (match c
         [#\b (<char-start>-special-seq #\b #\a '(#\c #\k #\s #\p #\a #\c #\e) #\backspace)]
         [#\l (<char-start>-special-seq #\l #\i '(#\n #\e #\f #\e #\e #\d) #\linefeed)]
         [#\p (<char-start>-special-seq #\p #\a '(#\g #\e) #\page)]
         [#\s (<char-start>-special-seq #\s #\p '(#\a #\c #\e) #\space)]
         [#\t (<char-start>-special-seq #\t #\a '(#\b) #\tab)]
         [#\v (<char-start>-special-seq #\v #\t '(#\a #\b) #\vtab)]
         [#\r (<char-start>-special-seq-alt #\r
                                                 #\e '(#\t #\u #\r #\n) #\return
                                                 #\u '(#\b #\o #\u #\t) #\rubout)]
         ;; Move this into <char-start>-nu and rename to -n.
         [#\n (let ((next (peek-char)))
                (cond [(char=? next #\e)
                       (begin (read-char)
                              (committed '(#\w #\l #\i #\n #\e) #\newline))]
                      [(char=? next #\u)
                       (begin (read-char) (<char-start>nu))]
                      [(eof-object? next) #\n]
                      [(not-char-alphabetic? next) #\n]
                      [else (err "error")]))]

         [#\u
          (cond [(char-digit16? (peek-char))
                 (<char-start><digit16>+ (list (read-char)) 3)]
                [(not-char-alphabetic? (peek-char))
                 #\u]
                [else (err "error")])]
         [#\U
          (cond [(char-digit16? (peek-char))
                 (<char-start><digit16>+ (list (read-char)) 7)]
                [(not-char-alphabetic? (peek-char))
                 #\U]
                [else (err "error")])]
         [_
          (if (and (char-alphabetic? c)
                   (not-char-alphabetic? (peek-char)))
              c
              (err "error"))])])))

;; Assume: seen '#\', c0, which may be the start of special sequence for char if c1 comes next
(define (<char-start>-special-seq c0 c1 seq char)
  (let ((next (peek-char)))
    (cond [(char=? next c1)
           (begin (read-char)
                  (committed seq char))]
          [(eof-object? next) c0]
          [(not-char-alphabetic? next) c0]
          [else (err "error")])))

;; Assume: seen '#\', c0, which may be the start of special sequence;
;; for char1 if c1 comes next or for char2 if c2 comes next
(define (<char-start>-special-seq-alt c0 c1 seq1 char1 c2 seq2 char2)
  (let ((next (peek-char)))
    (cond [(char=? next c1)
           (begin (read-char)
                  (committed seq1 char1))]
          [(char=? next c2)
           (begin (read-char)
                  (committed seq2 char2))]
          [(eof-object? next) c0]
          [(not-char-alphabetic? next) c0]
          [else (err "error")])))

;; committed to see #\nul or #\null, error otherwise
(define (<char-start>nu)
  (match (read-char)
    [#\l (match (peek-char)
           [(? not-char-alphabetic?) #\nul]
           [#\l (read-char)
                (match (peek-char)
                  [(? not-char-alphabetic?) #\nul]
                  [_ (err "error")])]
           [_ (err "error")])]
    [_ (err "error")]))

(define (<char-start><digit16>+ cs n)
  (if (zero? n)
      (char-digit16s->char cs)
      (match (peek-char)
        [(? eof-object?) (char-digit16s->char cs)]
        [(? char-digit16?) (<char-start><digit16>+ (cons (read-char) cs) (sub1 n))]
        [_ (char-digit16s->char cs)])))

(define (<char-start><digit8> c)
  (match (peek-char)
    ;; this is the same behavior Racket has: it commits after two digits
    ;; have to use peek-bytes to behave differently
    [(? char-digit8?) (<char-start><digit8><digit8> c (read-char))]
    [_ c]))

(define (<char-start><digit8><digit8> c1 c2)
  (match (read-char)
    [(? eof-object?) (err "error")]
    [(? char-digit8? c3) (octal-char c1 c2 c3)]
    [_ (err "error")]))

(define (committed chars c)
  (match chars
    ['() (if (not-char-alphabetic? (peek-char))
             c
             (err "error"))]
    [(cons c0 cs)
     (let ((c1 (read-char)))
       (if (and (char? c1) (char=? c1 c0))
           (committed cs c)
           (err "error")))]))

(define (char-digit16s->char ds)
  (let ((x (char-digit16s->number ds)))
    (if (or (<= 0 x 55295)
            (<= 57344 x 1114111))
        (integer->char x)
        (err "error"))))

(define (char-digit2s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit->number d)
        (* 2 (char-digit2s->number ds)))]))

(define (char-digit8s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit->number d)
        (* 8 (char-digit8s->number ds)))]))

(define (char-digit10s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit->number d)
        (* 10 (char-digit10s->number ds)))]))

(define (char-digit16s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit16->number d)
        (* 16 (char-digit16s->number ds)))]))

(define (char-digit->number d)
  (- (char->integer d)
     (char->integer #\0)))

(define (char-digit2? d)
  (and (char? d)
       (<= 48 (char->integer d) 49)))

(define (char-digit8? d)
  (and (char? d)
       (<= 48 (char->integer d) 55)))

(define (char-digit10? d)
  (and (char? d)
       (<= 48 (char->integer d) 57)))

(define (char-digit16? d)
  (and (char? d)
       (let ((x (char->integer d)))
         (or (<= 48 x 57)
             (<= 65 x 70)
             (<= 97 x 102)))))

(define (char-digit8->number c)
  (- (char->integer c) 48))

(define (char-digit16->number c)
  (let ((x (char->integer c)))
    (cond [(<= 48 x 57)  (- x 48)]
          [(<= 65 x 70)  (- x 55)]
          [(<= 97 x 102) (- x 87)]
          [else (error "bad char-digit16")])))

(define (octal-char d1 d2 d3)
  (let ((x (+ (* 64 (char-digit8->number d1))
              (* 8  (char-digit8->number d2))
              (char-digit8->number d3))))
    (if (<= 0 x 255)
        (integer->char x)
        (err "ERROR"))))

(define (not-char-alphabetic? c)
  (or (eof-object? c)
      (not (char-alphabetic? c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings

;; Assume: have already read '"'
(define (<string-start-chars> cs)
  (match (read-char)
    [(? eof-object?) (err "error")]
    [#\" (list->string (reverse cs))]
    [#\\ (<escape> cs)]
    [c   (<string-start-chars> (cons c cs))]))

(define (<escape> cs)
  (match (read-char)
    [(? eof-object?) (err "error")]
    [#\a (<string-start-chars> (cons #\007 cs))]
    [#\b (<string-start-chars> (cons #\010 cs))]
    [#\t (<string-start-chars> (cons #\011 cs))]
    [#\n (<string-start-chars> (cons #\012 cs))]
    [#\v (<string-start-chars> (cons #\013 cs))]
    [#\f (<string-start-chars> (cons #\014 cs))]
    [#\r (<string-start-chars> (cons #\015 cs))]
    [#\e (<string-start-chars> (cons #\033 cs))]
    [#\" (<string-start-chars> (cons #\"   cs))]
    [#\' (<string-start-chars> (cons #\'   cs))]
    [#\\ (<string-start-chars> (cons #\\   cs))]
    [#\x (<hex>* cs 2)]
    [#\u (<hex>* cs 4)] ; FIXME: will need a different function to handle \u...\u... form
    [#\U (<hex>* cs 8)]
    [(? char-digit8? d) (<octal>+ cs (list d) 2)]
    [#\newline (<string-start-chars> cs)]
    [_ (err "error")]))

(define (<octal>+ cs ds n)
  (if (zero? n)
      (<string-start-chars> (cons (char-digit8s->char ds) cs))
      (match (peek-char)
        [(? eof-object?) (err "error")]
        [(? char-digit8?) (<octal>+ cs (cons (read-char) ds) (sub1 n))]
        [_ (<string-start-chars> (cons (char-digit8s->char ds) cs))])))

(define (<hex>* cs n)
  (match (peek-char)
    [(? eof-object?) (err "error")]
    [(? char-digit16?) (<hex>+ cs (list (read-char)) (sub1 n))]
    [_ (err "error")]))

(define (<hex>+ cs ds n)
  (if (zero? n)
      (return-<hex>+ cs ds)
      (match (peek-char)
        [(? eof-object?) (err "error")]
        [(? char-digit16?) (<hex>+ cs (cons (read-char) ds) (sub1 n))]
        [_ (return-<hex>+ cs ds)])))

(define (return-<hex>+ cs ds)
  (let ((r (char-digit16s->char ds)))
    (if (err? r)
        r
        (<string-start-chars> (cons r cs)))))

(define (char-digit8s->char ds)
  (integer->char (char-digit8s->number ds)))

(define (delim?)
  (let ((c (peek-char)))
    (or (eof-object? c)
        (char-delim? c))))

(define (char-delim? x)
  (or (char-whitespace? x)
      (memq x '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\;))))

(define (opposite? p1 p2)
  (match p1
    [#\( (char=? p2 #\))]
    [#\[ (char=? p2 #\])]
    [#\{ (char=? p2 #\})]))

(define (open-paren? c)
  (memq c '(#\( #\[ #\{)))

(define (close-paren? c)
  (memq c '(#\) #\] #\})))

;; committed to seeing chars followed by a delimiter, producing x
(define (committed-delim chars x)
  (match chars
    ['() (if (delim?) x (err "unexpected sequence"))]
    [(cons c0 cs)
     (let ((c1 (read-char)))
       (if (and (char? c1) (char=? c1 c0))
           (committed-delim cs x)
           (err "unexpected sequence")))]))

(define (unimplemented x)
  (err (string-append "unimplemented: " x)))
