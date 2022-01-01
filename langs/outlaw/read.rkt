#lang racket
(provide read)
(require "stdlib.rkt" "utils.rkt")
;(require (only-in "stdlib.rkt" read-char))

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
    [#\' (<quote> 'quote)]
    [#\` (<quote> 'quasiquote)]
    [#\, (match (peek-char)
           [#\@ (read-char) (<quote> 'unquote-splicing)]
           [_ (<quote> 'unquote)])]
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
    [#\' (<quote> 'syntax)]
    [#\! (unimplemented "shebang comment")]
    [#\` (<quote> 'quasisyntax)]
    [#\, (match (peek-char)
           [#\@ (read-char) (<quote> 'unsyntax-splicing)]
           [_ (<quote> 'unsyntax)])]
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
    [#\+ (if (delim?) '+ (<unsigned-or-symbol> #\+ '()))]
    [#\- (if (delim?) '- (<unsigned-or-symbol> #\- '()))]
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
