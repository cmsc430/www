#lang racket
(provide read)

;; Port -> Any
;; Read an s-expression from given port
(define (read p)
  (let ((r (<start> p)))
    (if (err? r)
        (error (err-msg r))
        r)))

(struct err (port msg))


(define (error str)
  (add1 #f))

(define (<start> p)
  100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start

(module+ test
  (define (p s)
    (<start> (open-input-string s)))

  (check-equal? (p "") eof)
  (check-equal? (p "  ") eof)
  (check-equal? (p ";123") eof)
  (check-equal? (p "#;123 ") eof)
  (check-equal? (p "#;123") eof)
  (check-equal? (p "#|123|# ") eof)
  (check-equal? (p "#;#|123|#1 ") eof)
  (check-equal? (p "#;#;1 2") eof)
  (check-equal? (p "123") 123)
  (check-equal? (p "#t") #t)
  (check-equal? (p "#f") #f)
  (check-equal? (p "#T") #t)
  (check-equal? (p "#F") #f)
  (check-equal? (p "#b0") 0)
  (check-equal? (p "#b1") 1)
  (check-equal? (p "#b101") #b101)
  (check-equal? (p "#B101") #b101)
  (check-equal? (p "#o0") 0)
  (check-equal? (p "#o1") 1)
  (check-equal? (p "#o701") #o701)
  (check-equal? (p "#O701") #o701)
  (check-equal? (p "#d0") 0)
  (check-equal? (p "#d1") 1)
  (check-equal? (p "#d901") 901)
  (check-equal? (p "#D901") 901)
  (check-equal? (p "#x0") 0)
  (check-equal? (p "#x1") 1)
  (check-equal? (p "#xF01") #xF01)
  (check-equal? (p "#XF01") #xF01)
  (check-equal? (p ";123\n1") 1)
  (check-equal? (p "()") '())
  (check-equal? (p "[]") '())
  (check-equal? (p "{}") '())
  (check-equal? (p "(#t)") '(#t))
  (check-equal? (p "[#t]") '(#t))
  (check-equal? (p "{#t}") '(#t))
  (check-equal? (p "((#t))") '((#t)))
  (check-pred err? (p "#|"))
  (check-pred err? (p "#;")) 
  (check-pred err? (p "(}"))
  (check-pred err? (p "(]"))
  (check-pred err? (p "[)"))
  (check-pred err? (p "(x}"))
  (check-pred err? (p "(x]"))
  (check-pred err? (p "[x)"))
  (check-pred err? (p "(x . y}"))
  (check-pred err? (p "(x . y]"))
  (check-pred err? (p "[x . y)")))

(define (<start> p)
  (match (peek-char p)
    [(? eof-object?) (read-char p)]
    [(? char-whitespace?) (read-char p) (<start> p)]
    [#\; (<line-comment> p) (<start> p)]
    [#\# (read-char p)
         (match (peek-char p)
           [#\| (read-char p)
                (let ((r (<block-comment> p)))
                  (if (err? r) r (<start> p)))]
           [#\; (read-char p)
                (let ((r (<elem> p)))
                  (if (err? r) r (<start> p)))]
           [_ (<octo> p)])]
    [_   (<elem> p)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elem

(module+ test
  (define (pe s)
    (<elem> (open-input-string s)))

  (check-equal? (pe "1") 1)
  (check-equal? (pe "x") 'x)
  (check-equal? (pe "|x|") 'x)
  (check-equal? (pe "'x") ''x)
  (check-equal? (pe "`x") '`x)
  (check-equal? (pe ",x") ',x)
  (check-equal? (pe ",@x") ',@x)
  (check-equal? (pe "\"x\"") "x")
  (check-equal? (pe "(x)") '(x))
  (check-equal? (pe "('x)") '('x))
  (check-equal? (pe ";f\n1") 1)
  (check-equal? (pe "#'x") '#'x)
  (check-equal? (pe "#`x") '#`x)
  (check-equal? (pe "#,x") '#,x)
  (check-equal? (pe "#,@x") '#,@x)
  (check-equal? (pe "#true") #t)
  (check-equal? (pe "#false") #f)
  (check-equal? (pe "#\\a") #\a)
  (check-equal? (pe "#:a") '#:a)
 
  (check-pred err? (pe "'(."))
  (check-pred err? (pe "#"))
  (check-pred err? (pe "#z"))
  (check-pred err? (pe "#|"))
  (check-pred err? (pe "|"))
  (check-pred err? (pe "#;")))

(define (<elem> p)
  (match (read-char p)
    [(? eof-object?) (err p "eof")]
    [(? char-whitespace?) (<elem> p)]
    [#\| (<symbol-escape> p)]
    [#\" (<string-start-chars> '() p)]
    [#\# (<octo-elem> p)]
    [(? open-paren? c) (<list-or-pair> c p)]
    [#\; (<line-comment> p) (<elem> p)]
    [#\' (<quote> 'quote p)]
    [#\` (<quote> 'quasiquote p)]
    [#\, (match (peek-char p)
           [#\@ (read-char p) (<quote> 'unquote-splicing p)]
           [_ (<quote> 'unquote p)])]
    [c   (<number-or-symbol> c p)]))

(define (<quote> q p)
  (let ((r (<elem> p)))
    (if (err? r)
        r
        (list q r))))

(define (<octo-elem> p)
  (match (peek-char p)
    [#\| (read-char p)
         (let ((r (<block-comment> p)))
           (if (err? r) r (<elem> p)))]
    [#\; (read-char p)
         (let ((r (<elem> p)))
           (if (err? r) r (<elem> p)))]
    [_ (<octo> p)]))

(define (<octo> p)
  (match (read-char p)
    [(? eof-object?) (err p "bad syntax `#`")]
    [#\T (committed-delim '() #t p)]
    [#\F (committed-delim '() #f p)]  ; could also be #Fl  
    [#\t (if (delim? p) #t (committed-delim '(#\r #\u #\e) #t p))]
    ;; could also be #fl
    [#\f (if (delim? p) #f (committed-delim '(#\a #\l #\s #\e) #f p))] 
    [#\( (<vector> #\( p)]
    [#\[ (<vector> #\[ p)]
    [#\{ (<vector> #\{ p)]
    [#\s (unimplemented "structure")]
    [#\\ (<char-start> p)]
    [#\: (<keyword> p)]
    [#\& (unimplemented "boxes")] ; FIXME
    [#\' (<quote> 'syntax p)]
    [#\! (unimplemented "shebang comment")]
    [#\` (<quote> 'quasisyntax p)]
    [#\, (match (peek-char p)
           [#\@ (read-char p) (<quote> 'unsyntax-splicing p)]
           [_ (<quote> 'unsyntax p)])]
    [#\~ (unimplemented "compiled code")]
    [#\i (unimplemented "inexact number")]
    [#\I (unimplemented "inexact number")]
    [#\e (unimplemented "exact number")]
    [#\E (unimplemented "exact number")]
    [#\b (<general-numbern> char-digit2? char-digit2s->number p)]
    [#\B (<general-numbern> char-digit2? char-digit2s->number p)]
    [#\o (<general-numbern> char-digit8? char-digit8s->number p)]
    [#\O (<general-numbern> char-digit8? char-digit8s->number p)]
    [#\d (<general-numbern> char-digit10? char-digit10s->number p)]
    [#\D (<general-numbern> char-digit10? char-digit10s->number p)]
    [#\x (<general-numbern> char-digit16? char-digit16s->number p)]
    [#\X (<general-numbern> char-digit16? char-digit16s->number p)]
    [#\< (<here-string> p)]
    [#\r (unimplemented "regexp or reader")]
    [#\p (unimplemented "pregexp")]
    [#\c (unimplemented "case switch")]
    [#\C (unimplemented "case switch")]
    [#\h (unimplemented "hash")]
    [(? char-digit10?) (unimplemented "vector or graph")]
    [_ (err p "bad syntax")]))
    

(define (<here-string> p)
  (unimplemented "here string"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers

;; have seen '#b', '#o', etc.
;; simplified to just digits
(define (<general-numbern> char-digitn? char-digitsn->number p)
  (local [(define (<digitn>+ p)
            (match (read-char p)
              [(? char-digitn? c)  (<digitn>* p (list c))]
              [_ (err p "error")]))
          (define (<digitn>* p ds)
            (if (delim? p)
                (char-digitsn->number ds)
                (match (read-char p)
                  [(? char-digitn? c) (<digitn>* p (cons c ds))]
                  [_ (err p "error")])))]
    (match (read-char p)
      [#\#
       (match (read-char p)
         [#\e (<digitn>+ p)]
         [#\i (unimplemented "inexact")]
         [_ (err p "error")])]
      [#\+ (read-char p) (<digitn>+ p)]
      [#\- (read-char p) (- (<digitn>+ p))]
      [(? char-digitn? c) (<digitn>* p (list c))]
      [_ (err p "error")])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers or Symbols

;; Numbers are simplified significantly:

;; <number>   ::= ['+' | '-] <unsigned>
;; <unsigned> ::= '.' <digit>+
;;             |  <digit> <digit>* ['.' <digit>*]

;; whenver something else is encounter, parse as a symbol

(module+ test
  (define (pn s)
    (<number-or-symbol> (string-ref s 0) (open-input-string (substring s 1))))

  (check-equal? (pn "+") '+)
  (check-equal? (pn "-") '-)
  (check-equal? (pn "5") 5)
  (check-equal? (pn "123") 123)
  (check-equal? (pn "123 ") 123)
  (check-equal? (pn "0123") 123)
  (check-equal? (pn "-123") -123)
  (check-equal? (pn "+123") 123)
  ; removed frac
  (check-equal? (pn "5.") 5.0)
  (check-equal? (pn ".5") 0.5)
  (check-equal? (pn ".5 ") 0.5)
  (check-equal? (pn "+.5") 0.5)
  (check-equal? (pn "-.5") -0.5)
  (check-equal? (pn "+1.5") 1.5)
  (check-equal? (pn "-1.5") -1.5)
  (check-equal? (pn "+.5") 0.5)
  (check-equal? (pn "-.") '-.)
  (check-equal? (pn "+.") '+.)
  (check-equal? (pn "+.x") '+.x)
  (check-equal? (pn "+x") '+x)
  (check-equal? (pn "-x") '-x)
  ; removed frac
  (check-equal? (pn ".x") '.x)
  (check-equal? (pn "1..") '1..)
  (check-equal? (pn "1.1.") '1.1.)  
  (check-pred err? (pn ".")))

(define (<number-or-symbol> c p)
  (match c
    [#\+ (if (delim? p) '+ (<unsigned-or-symbol> #\+ '() p))]
    [#\- (if (delim? p) '- (<unsigned-or-symbol> #\- '() p))]
    [#\. (if (delim? p) (err p ".") (<frac> #f '() '() p))]
    [(? char-digit10?) (<unsigned-or-symbol> #f (list c) p)]
    [_   (<symbol> (list c) p)]))

(define (<unsigned-or-symbol> signed? whole p)
  (match (peek-char p)
    [(? eof-object?) (make-whole signed? whole)]
    [(? char-delim?) (make-whole signed? whole)]
    [#\. (read-char p) (<frac> signed? whole '() p)]
    [(? char-digit10? d)
     (read-char p)
     (<unsigned-or-symbol> signed? (cons d whole) p)]    
    [_ (<symbol> (cons (read-char p)
                       (append whole (if signed? (list signed?) '())))
                 p)]))

(define (<frac> signed? whole frac p)
  (match (peek-char p)
    [(? eof-object?) (make-frac signed? whole frac)]
    [(? char-delim?) (make-frac signed? whole frac)]
    [(? char-digit10?) (<frac> signed? whole (cons (read-char p) frac) p)]
    [_ (<symbol> (cons (read-char p)
                       (append frac
                               (list #\.)
                               whole
                               (if signed? (list signed?) '())))
                 p)]))

(define (make-frac signed? whole frac)
  (match* (whole frac)
    [('() '()) (chars->symbol (list #\. signed?))]
    [(_ _)
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

(module+ test
  (define (pl s)
    (let ((p (open-input-string s))
          (o (open-output-string)))
      (<line-comment> p)
      (copy-port p o)
      (get-output-string o)))

  (check-equal? (pl "foo") "")
  (check-equal? (pl "foo\n") "")
  (check-equal? (pl "foo\u20291") "1")
  (check-equal? (pl "foo\u20281") "1")
  (check-equal? (pl "foo\r1") "1")
  (check-equal? (pl "foo\n1") "1"))

(define (<line-comment> p)
  (let ((c (read-char p)))
    (or (eof-object? c)
        (and (memv c '(#\newline #\return #\u0085 #\u2028 #\u2029)) #t)
        (<line-comment> p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block comment

(module+ test
  (define (pb s)
    (<block-comment> (open-input-string s)))

  (check-equal? (pb "|#") #t)
  (check-equal? (pb "aadsfa|#") #t)
  (check-equal? (pb "aadsfa|#adsf") #t)
  (check-equal? (pb "aad# |#") #t)
  (check-equal? (pb "aadsfa|#|#") #t)
  (check-equal? (pb "aads#|fa|#adsf|#") #t)

  (check-pred err? (pb ""))
  (check-pred err? (pb "#|"))
  (check-pred err? (pb "#||#")))

(define (<block-comment> p)
  (match (read-char p)
    [(? eof-object?) (err p "unbalanced |#")]
    [#\# (match (peek-char p)
           [#\| (let ((r (<block-comment> p)))
                  (if (err? r) r (<block-comment> p)))]
           [_ (<block-comment> p)])]
    [#\| (match (peek-char p)
           [#\# (read-char p) #t]
           [_ (<block-comment> p)])]
    [_ (<block-comment> p)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors

(module+ test
  (define (pv s)
    (<vector> #\( (open-input-string s)))

  (check-equal? (pv ")") #())
  (check-equal? (pv "x)") #(x))
  (check-equal? (pv "x y z)") #(x y z))
  (check-equal? (pv "#;() x y z)") #(x y z))
  (check-equal? (pv "x #;() y z)") #(x y z))
  (check-equal? (pv "x y z #;())") #(x y z))
  (check-equal? (pv ";f\nx y z #;())") #(x y z))
  (check-equal? (pv ";f\nx y z #;();\n)") #(x y z))
  (check-pred err? (pv "x . y)"))
  (check-pred err? (pv "x y . z)"))
  (check-pred err? (pv "#;() x y . z)"))
  (check-pred err? (pv "x #;() y . z)"))
  (check-pred err? (pv "x y #;() . z)"))
  (check-pred err? (pv "x y . #;() z)"))
  (check-pred err? (pv "x y . z #;())"))
  (check-pred err? (pv "#||# x y . z)"))
  (check-pred err? (pv "x #||# y . z)"))
  (check-pred err? (pv "x y #||# . z)"))
  (check-pred err? (pv "x y . #||# z)"))
  (check-pred err? (pv "x y . z #||#)"))
  (check-pred err? (pv "x y . z ;f\n)")))

(define (<vector> paren p)
  (let ((r (<list-or-pair> paren p)))
    (if (err? r)
        r
        (if (list? r)
            (list->vector r)
            (err p "dotted list in vector")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists or pairs

(module+ test
  (define (pd s)
    (<list-or-pair> #\( (open-input-string s)))

  (check-equal? (pd ")") '())
  (check-equal? (pd "x)") '(x))
  (check-equal? (pd "x y z)") '(x y z))
  (check-equal? (pd "#;() x y z)") '(x y z))
  (check-equal? (pd "x #;() y z)") '(x y z))
  (check-equal? (pd "x y z #;())") '(x y z))
  (check-equal? (pd ";f\nx y z #;())") '(x y z))
  (check-equal? (pd ";f\nx y z #;();\n)") '(x y z))
  (check-equal? (pd "x . y)") '(x . y))
  (check-equal? (pd "x y . z)") '(x y . z))
  (check-equal? (pd "#;() x y . z)") '(x y . z))
  (check-equal? (pd "x #;() y . z)") '(x y . z))
  (check-equal? (pd "x y #;() . z)") '(x y . z))
  (check-equal? (pd "x y . #;() z)") '(x y . z))
  (check-equal? (pd "x y . z #;())") '(x y . z))
  (check-equal? (pd "#||# x y . z)") '(x y . z))
  (check-equal? (pd "x #||# y . z)") '(x y . z))
  (check-equal? (pd "x y #||# . z)") '(x y . z))
  (check-equal? (pd "x y . #||# z)") '(x y . z))
  (check-equal? (pd "x y . z #||#)") '(x y . z))
  (check-equal? (pd "x y . z ;f\n)") '(x y . z))  
  (check-equal? (pd "x #t)") '(x #t))
  (check-equal? (pd "x .y)") '(x .y))
  ; removed frac
  ;(check-equal? (pd "x .0)") '(x 0.0))
  (check-equal? (pd "x .y)") '(x .y))
  (check-equal? (pd "[] 0)") '(() 0))

  (check-pred err? (pd ""))
  (check-pred err? (pd "#|"))
  (check-pred err? (pd "#;"))
  (check-pred err? (pd ";"))
  (check-pred err? (pd "#z"))
  (check-pred err? (pd "1"))
  (check-pred err? (pd "1 ."))
  (check-pred err? (pd "1 #|"))
  (check-pred err? (pd "1 #;"))
  (check-pred err? (pd "1 #z"))
  (check-pred err? (pd "1 ("))
  (check-pred err? (pd "x . y #t"))
  (check-pred err? (pd "x . y #|"))
  (check-pred err? (pd "x . y #;"))
  (check-pred err? (pd "x . y 1")))


(define (<list-or-pair> paren p)
  (match (peek-char p)
    [(? eof-object?) (err p "missing! )")]
    [(? char-whitespace?) (read-char p) (<list-or-pair> paren p)]    
    [#\; (<line-comment> p) (<list-or-pair> paren p)]
    [(? close-paren? c)
     (read-char p)
     (if (opposite? paren c) '() (err p "mismatched paren"))]
    [#\# (read-char p)
         (match (peek-char p)
           [#\| (read-char p)
                (let ((r (<block-comment> p)))
                  (if (err? r) r (<list-or-pair> paren p)))]
           [#\; (read-char p)
                (let ((r (<elem> p)))
                  (if (err? r) r (<list-or-pair> paren p)))]           
           [_ (let ((r (<octo> p)))
                (if (err? r) r (<elem><list-or-pair> paren (list r) p)))])]    
    [_  (let ((r (<elem> p)))
          (if (err? r)
              r
              (<elem><list-or-pair> paren (list r) p)))]))

(define (<elem><list-or-pair> paren xs p)
  (match (peek-char p)
    [(? eof-object?) (err p "missing!! )")]
    [(? char-whitespace?) (read-char p) (<elem><list-or-pair> paren xs p)]    
    [#\; (<line-comment> p) (<elem><list-or-pair> paren xs p)]
    [(? close-paren? c)
     (read-char p)
     (if (opposite? paren c) (reverse xs) (err p "mismatched paren"))]
    [#\# (read-char p)
         (match (peek-char p)
           [#\| (read-char p)
                (let ((r (<block-comment> p)))
                  (if (err? r) r (<elem><list-or-pair> paren xs p)))]
           [#\; (read-char p)
                (let ((r (<elem> p)))
                  (if (err? r) r (<elem><list-or-pair> paren xs p)))]           
           [_ (let ((r (<octo> p)))
                (if (err? r) r (<elem><list-or-pair> paren (cons r xs) p)))])]
    [#\. (read-char p)
         (if (delim? p)
             (<dotted-list> paren xs p)
             (<elem><list-or-pair> paren (cons (<frac> #f '() '() p) xs) p))]
    [_  (let ((r (<elem> p)))
          (if (err? r)
              r
              (<elem><list-or-pair> paren (cons r xs) p)))]))

(define (<dotted-list> paren xs p)
  (let ((r (<elem> p)))
    (if (err? r)
        r
        (<dotted-list-close> paren (append* (reverse xs) (list r)) p))))

(define (<dotted-list-close> paren xs p)
  (match (read-char p)
    [(? char-whitespace?) (<dotted-list-close> paren xs p)]
    [#\; (<line-comment> p) (<dotted-list-close> paren xs p)]
    [#\# (match (peek-char p)
           [#\| (read-char p)
                (let ((r (<block-comment> p)))
                  (if (err? r) r (<dotted-list-close> paren xs p)))]
           [#\; (read-char p)
                (let ((r (<elem> p)))
                  (if (err? r) r (<dotted-list-close> paren xs p)))]
           [_ (err p "unexpected")])]
    [(? close-paren? c)
     (if (opposite? paren c) xs (err p "mismatched paren"))]
    [_ (err p "uneasdfasdxpected")]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols and Keywords

(module+ test
  (define (py s)
    (<symbol> '() (open-input-string s)))

  (check-equal? (py "") '||)
  (check-equal? (py "x") 'x)
  (check-equal? (py "xyz") 'xyz)
  (check-equal? (py "x(") 'x)
  (check-equal? (py "|x|") 'x)
  (check-equal? (py "| |") '| |)
  (check-equal? (py "\\x") 'x)
  (check-equal? (py "\\ ") '| |)
  (check-equal? (py "|\\|") '|\|)
  (check-pred err? (py "|"))
  (check-pred err? (py "\\"))

  (define (pk s)
    (<keyword> (open-input-string s)))

  (check-equal? (pk "") '#:||)
  (check-equal? (pk "x") '#:x)
  (check-equal? (pk "xyz") '#:xyz)
  (check-equal? (pk "x(") '#:x)
  (check-equal? (pk "|x|") '#:x)
  (check-equal? (pk "| |") '#:| |)
  (check-equal? (pk "\\x") '#:x)
  (check-equal? (pk "\\ ") '#:| |)
  (check-equal? (pk "|\\|") '#:|\|)
  (check-pred err? (pk "|"))
  (check-pred err? (pk "\\")))

(define (<symbol> cs p)
  (let ((r (<symbol-chars> cs p)))
    (if (err? r)
        r
        (chars->symbol r))))

(define (<keyword> p)
  (let ((r (<symbol-chars> '() p)))
    (if (err? r)
        r
        (chars->keyword r))))

(define (<symbol-escape> p)
  (let ((r (<symbol-escape-chars> '() p)))
    (if (err? r)
        r
        (chars->symbol r))))

;; Assume: what we've seen tells us this is a symbol, cs are the chars of the
;; symbol seen so far in reverse order
(define (<symbol-chars> cs p)
  (if (delim? p)
      cs
      (match (peek-char p)        
        [#\\ (read-char p) (<symbol-single-escape-chars> cs p)]
        [#\| (read-char p) (<symbol-escape-chars> cs p)]        
        [_ (<symbol-chars> (cons (read-char p) cs) p)])))

(define (<symbol-single-escape-chars> cs p)
  (match (read-char p)
    [(? eof-object?) (err p "read: end-of-file following `\\` in symbol")]
    [c (<symbol-chars> (cons c cs) p)]))

(define (<symbol-escape-chars> cs p)
  (match (read-char p)
    [(? eof-object?) (err p "read: end-of-file following `|` in symbol")]
    [#\| (<symbol-chars> cs p)]
    [c (<symbol-escape-chars> (cons c cs) p)]))

(define (chars->symbol cs)
  (string->symbol (list->string (reverse cs))))

(define (chars->keyword cs)
  (string->keyword (list->string (reverse cs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters

(module+ test
  (require rackunit)
  (define (pc s)
    (<char-start> (open-input-string s)))

  (check-equal? (pc "nul") #\nul)
  (check-equal? (pc "null") #\nul)
  (check-equal? (pc "backspace") #\backspace)
  (check-equal? (pc "tab") #\tab)
  (check-equal? (pc "newline") #\newline)
  (check-equal? (pc "linefeed") #\linefeed)
  (check-equal? (pc "vtab") #\vtab)
  (check-equal? (pc "page") #\page)
  (check-equal? (pc "return") #\return)
  (check-equal? (pc "space") #\space)
  (check-equal? (pc "rubout") #\rubout)
  (check-equal? (pc "000") #\000)
  (check-equal? (pc "123") #\123)
  (check-equal? (pc "uABCD") #\uABCD)
  (check-equal? (pc "uABC") #\uABC)
  (check-equal? (pc "uAB") #\uAB)
  (check-equal? (pc "uA") #\uA)
  (check-equal? (pc "uabcd") #\uabcd)
  (check-equal? (pc "uabcd7") #\uabcd)
  (check-equal? (pc "uabc") #\uabc)
  (check-equal? (pc "uab") #\uab)
  (check-equal? (pc "ua") #\ua)
  (check-equal? (pc "uag") #\ua)
  (check-equal? (pc "UABCD") #\uABCD)
  (check-equal? (pc "UABC") #\uABC)
  (check-equal? (pc "UAB") #\uAB)
  (check-equal? (pc "UA") #\uA)
  (check-equal? (pc "Uabcd") #\uabcd)
  (check-equal? (pc "Uabc") #\uabc)
  (check-equal? (pc "Uab") #\uab)
  (check-equal? (pc "Ua") #\ua)  
  (check-equal? (pc "UABCDE") #\UABCDE)
  (check-equal? (pc "U000DEF") #\U000DEF)  
  (check-equal? (pc "u") #\u)
  (check-equal? (pc "7") #\7)
  (check-equal? (pc "78") #\7)  
  (check-equal? (pc "a7") #\a)
  (check-equal? (pc " 8") #\space)
  (check-pred err? (pc ""))
  (check-pred err? (pc "aa"))
  (check-pred err? (pc "newlines"))
  (check-pred err? (pc "777"))
  (check-pred err? (pc "UABCDEF"))
  (check-pred err? (pc "spo"))
  (check-pred err? (pc "nub"))
  (check-pred err? (pc "nula"))
  (check-pred err? (pc "nulla"))    
  ;; controversial
  (check-pred err? (pc "77"))
  (check-pred err? (pc "779"))
  (check-equal? (pc "U0000000A") #\newline))
  

;; Assume: have already read '#\'
(define (<char-start> p)
  (let ((c (read-char p)))
    (cond
      [(eof-object? c) (err p "error")]
      [(eof-object? (peek-char p)) c]
      [(char-digit8? c) (<char-start><digit8> c p)]
      [(not-char-alphabetic? c) c]
      [else
       (match c
         [#\b (<char-start>-special-seq #\b #\a '(#\c #\k #\s #\p #\a #\c #\e) #\backspace p)]
         [#\l (<char-start>-special-seq #\l #\i '(#\n #\e #\f #\e #\e #\d) #\linefeed p)]
         [#\p (<char-start>-special-seq #\p #\a '(#\g #\e) #\page p)]
         [#\s (<char-start>-special-seq #\s #\p '(#\a #\c #\e) #\space p)]
         [#\t (<char-start>-special-seq #\t #\a '(#\b) #\tab p)]
         [#\v (<char-start>-special-seq #\v #\t '(#\a #\b) #\vtab p)]
         [#\r (<char-start>-special-seq-alt #\r
                                                 #\e '(#\t #\u #\r #\n) #\return
                                                 #\u '(#\b #\o #\u #\t) #\rubout p)]
         ;; Move this into <char-start>-nu and rename to -n.
         [#\n (let ((next (peek-char p)))
                (cond [(char=? next #\e)
                       (begin (read-char p)
                              (committed '(#\w #\l #\i #\n #\e) #\newline p))]
                      [(char=? next #\u)
                       (begin (read-char p) (<char-start>nu p))]
                      [(eof-object? next) #\n]
                      [(not-char-alphabetic? next) #\n]
                      [else (error p "error")]))]

         [#\u
          (cond [(char-digit16? (peek-char p))
                 (<char-start><digit16>+ (list (read-char p)) 3 p)]
                [(not-char-alphabetic? (peek-char p))
                 (read-char p)]
                [else (err p "error")])]
         [#\U
          (cond [(char-digit16? (peek-char p))
                 (<char-start><digit16>+ (list (read-char p)) 7 p)]
                [(not-char-alphabetic? (peek-char p))
                 (read-char p)]
                [else (err p "error")])]
         [_
          (if (and (char-alphabetic? c)
                   (not-char-alphabetic? (peek-char p)))
              c
              (err p "error"))])])))

;; Assume: seen '#\', c0, which may be the start of special sequence for char if c1 comes next
(define (<char-start>-special-seq c0 c1 seq char p)
  (let ((next (peek-char p)))
    (cond [(char=? next c1)
           (begin (read-char p)
                  (committed seq char p))]
          [(eof-object? next) c0]
          [(not-char-alphabetic? next) c0]
          [else (error p "error")])))

;; Assume: seen '#\', c0, which may be the start of special sequence;
;; for char1 if c1 comes next or for char2 if c2 comes next
(define (<char-start>-special-seq-alt c0 c1 seq1 char1 c2 seq2 char2 p)
  (let ((next (peek-char p)))
    (cond [(char=? next c1)
           (begin (read-char p)
                  (committed seq1 char1 p))]
          [(char=? next c2)
           (begin (read-char p)
                  (committed seq2 char2 p))]
          [(eof-object? next) c0]
          [(not-char-alphabetic? next) c0]
          [else (error p "error")])))

;; committed to see #\nul or #\null, error otherwise
(define (<char-start>nu p)
  (match (read-char p)
    [#\l (match (peek-char p)
           [(? not-char-alphabetic?) #\nul]
           [#\l (read-char p)
                (match (peek-char p)
                  [(? not-char-alphabetic?) #\nul]
                  [_ (err p "error")])]
           [_ (err p "error")])]
    [_ (err p "error")]))

(define (<char-start><digit16>+ cs n p)
  (if (zero? n)
      (char-digit16s->char cs)
      (match (peek-char p)
        [(? eof-object?) (char-digit16s->char cs)]
        [(? char-digit16?) (<char-start><digit16>+ (cons (read-char p) cs) (sub1 n) p)]
        [_ (char-digit16s->char cs)])))

(define (<char-start><digit8> c p)
  (match (peek-char p)
    ;; this is the same behavior Racket has: it commits after two digits
    ;; have to use peek-bytes to behave differently
    [(? char-digit8?) (<char-start><digit8><digit8> c (read-char p) p)] 
    [_ c]))

(define (<char-start><digit8><digit8> c1 c2 p)
  (match (read-char p)
    [(? eof-object?) (err p "error")]
    [(? char-digit8? c3) (octal-char c1 c2 c3)]
    [_ (err p "error")]))

(define (committed chars c p)
  (match chars
    ['() (if (not-char-alphabetic? (peek-char p))
             c
             (err p "error"))]
    [(cons c0 cs)
     (let ((c1 (read-char p)))
       (if (and (char? c1) (char=? c1 c0))
           (committed cs c p)
           (err 'p "error")))]))

(define (char-digit16s->char ds)
  (let ((x (char-digit16s->number ds)))
    (if (or (<= 0 x 55295)
            (<= 57344 x 1114111))
        (integer->char x)
        (err 'p "error"))))

(define (char-digit2s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit->number d)
        (*2 (char-digit2s->number ds)))]))

(define (char-digit8s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit->number d)
        (*8 (char-digit8s->number ds)))]))

(define (char-digit10s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit->number d)        
        (*10 (char-digit10s->number ds)))]))

(define (char-digit16s->number ds)
  (match ds
    ['() 0]
    [(cons d ds)
     (+ (char-digit16->number d)
        (*16 (char-digit16s->number ds)))]))

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
          [(<= 97 x 102) (- x 87)])))

(define (octal-char d1 d2 d3)
  (let ((x (+ (*64 (char-digit8->number d1))
              (*8  (char-digit8->number d2))
              (char-digit8->number d3))))
    (if (<= 0 x 255)
        (integer->char x)
        (err 'p "ERROR"))))

(define (not-char-alphabetic? c)
  (or (eof-object? c)
      (not (char-alphabetic? c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings

(module+ test
  (require rackunit)
  (define (ps s)
    (<string-start-chars> '() (open-input-string s)))

  (check-equal? (ps "\"") "")
  (check-equal? (ps "a\"") "a")
  (check-equal? (ps "\\a\"") "\a")
  (check-equal? (ps "\\b\"") "\b")
  (check-equal? (ps "\\t\"") "\t")
  (check-equal? (ps "\\n\"") "\n")
  (check-equal? (ps "\\v\"") "\v")
  (check-equal? (ps "\\f\"") "\f")
  (check-equal? (ps "\\r\"") "\r")
  (check-equal? (ps "\\e\"") "\e")
  (check-equal? (ps "\\\"\"") "\"")
  (check-equal? (ps "\\\'\"") "'")
  (check-equal? (ps "\\\\\"") "\\")
  (check-equal? (ps "\\xa\"") "\xa")
  (check-equal? (ps "\\xab\"") "\xab")
  (check-equal? (ps "\\xabcd\"") "\xabcd")
  (check-equal? (ps "\\xabc\"") "\xabc")
  (check-equal? (ps "\\xaq\"") "\xaq")
  (check-equal? (ps "\\\nabc\"") "abc")
  (check-equal? (ps "\\uab\"") "\xab")
  (check-equal? (ps "\\uabcd\"") "\uabcd")
  (check-equal? (ps "\\Uabcd\"") "\uabcd")
  (check-equal? (ps "\\000\"") "\u000")
  (check-equal? (ps "\\0g\"") "\u0g")
  (check-equal? (ps "\\x0\"") "\x0")
  (check-equal? (ps "\\xA\"") "\xa")
  (check-equal? (ps "\\xa\"") "\xa")
  (check-equal? (ps "\\UAAAAA\"") "\UAAAAA")

  (check-pred err? (ps ""))
  (check-pred err? (ps "\\"))
  (check-pred err? (ps "\\x"))
  (check-pred err? (ps "\\0"))
  (check-pred err? (ps "\\x0"))  
  (check-pred err? (ps "\\xg\""))
  
  (check-pred err? (ps "\\q\""))
  (check-pred err? (ps "a\\q\""))
  (check-pred err? (ps "\\UFFFFFFFF\""))
  #;(check-pred err? (ps "\\Uag")))

;; Assume: have already read '"'
(define (<string-start-chars> cs p)
  (match (read-char p)
    [(? eof-object?) (err p "error")]
    [#\" (list->string (reverse cs))]
    [#\\ (<escape> cs p)]
    [c   (<string-start-chars> (cons c cs) p)]))

(define (<escape> cs p)
  (match (read-char p)
    [(? eof-object?) (err p "error")]
    [#\a (<string-start-chars> (cons #\007 cs) p)]
    [#\b (<string-start-chars> (cons #\010 cs) p)]
    [#\t (<string-start-chars> (cons #\011 cs) p)]
    [#\n (<string-start-chars> (cons #\012 cs) p)]
    [#\v (<string-start-chars> (cons #\013 cs) p)]
    [#\f (<string-start-chars> (cons #\014 cs) p)]
    [#\r (<string-start-chars> (cons #\015 cs) p)]
    [#\e (<string-start-chars> (cons #\033 cs) p)]
    [#\" (<string-start-chars> (cons #\"   cs) p)]
    [#\' (<string-start-chars> (cons #\'   cs) p)]
    [#\\ (<string-start-chars> (cons #\\   cs) p)]
    [#\x (<hex>* cs 2 p)]
    [#\u (<hex>* cs 4 p)] ; FIXME: will need a different function to handle \u...\u... form
    [#\U (<hex>* cs 8 p)]
    [(? char-digit8? d) (<octal>+ cs (list d) 2 p)]
    [#\newline (<string-start-chars> cs p)]
    [_ (err p "error")]))

(define (<octal>+ cs ds n p)
  (if (zero? n)
      (<string-start-chars> (cons (char-digit8s->char ds) cs) p)
      (match (peek-char p)
        [(? eof-object?) (err p "error")]
        [(? char-digit8?) (<octal>+ cs (cons (read-char p) ds) (sub1 n) p)]
        [_ (<string-start-chars> (cons (char-digit8s->char ds) cs) p)])))

(define (<hex>* cs n p)
  (match (peek-char p)
    [(? eof-object?) (err p "error")]
    [(? char-digit16?) (<hex>+ cs (list (read-char p)) (sub1 n) p)]
    [_ (err p "error")]))

(define (<hex>+ cs ds n p)
  (if (zero? n)
      (return-<hex>+ cs ds p)
      (match (peek-char p)
        [(? eof-object?) (err p "error")]
        [(? char-digit16?) (<hex>+ cs (cons (read-char p) ds) (sub1 n) p)]
        [_ (return-<hex>+ cs ds p)])))

(define (return-<hex>+ cs ds p)
  (let ((r (char-digit16s->char ds)))
    (if (err? r)
        r
        (<string-start-chars> (cons r cs) p))))

(define (char-digit8s->char ds)
  (integer->char (char-digit8s->number ds)))

(define (delim? p)
  (let ((c (peek-char p)))
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
(define (committed-delim chars x p)
  (match chars
    ['() (if (delim? p) x (err p "unexpected sequence"))]
    [(cons c0 cs)
     (let ((c1 (read-char p)))
       (if (and (char? c1) (char=? c1 c0))
           (committed-delim cs x p)
           (err p "unexpected sequence")))]))

(define (unimplemented x)
  (err #f (string-append "unimplemented: " x)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multipliers

(define (*2 a)
   (arithmetic-shift a 1))

(define (*8 a)
   (arithmetic-shift a 3))

(define (*16 a)
   (arithmetic-shift a 4))

(define (*10 a) ; 10a=2^3a+2a
  (+ (arithmetic-shift a 1)
     (arithmetic-shift a 3)))

(define (*64 a)
  (arithmetic-shift a 6))
