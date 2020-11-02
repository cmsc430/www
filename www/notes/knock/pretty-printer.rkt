#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Principal data structure for describing pretty-printed things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; type Seq
; | Str String
; | Nil
; | Newline
; | Indent Seq
; | Append Seq Seq

(struct str    (s)     #:transparent)
(struct nil    ()      #:transparent)
(struct nl     ()      #:transparent)
(struct indent (s)     #:transparent)
(struct ++     (s1 s2) #:transparent)

;(list (cons (list (++ (++ (str "main") 
;                      (++ (str " ") (++ (str "->")
;                      (++ (str " ") (str "print")))))
;                      (++ (str " ") (nil)))) 
;             2)
;      (cons (++ (nl) (str "}")) 0))
; Efficiently converting that data structure into a string, avoiding the
; n^2 append when dealing with left-nested appends
(define (flatten-seq col ss)
  (match ss
    ['() ""]
    [(cons (cons (indent s) i) rest) (flatten-seq col `((,s . ,col) ,@rest))]
    [(cons (cons (nil) i)      rest) (flatten-seq col rest)]
    [(cons (cons (str s) i)    rest) (string-append s
                                       (flatten-seq (+ (string-length s) i) rest))]
    [(cons (cons (++ s1 s2) i) rest) (flatten-seq col `(,(cons s1 i)
                                                    ,(cons s2 i)
                                                    ,@rest))]
    [(cons (cons (nl) i)       rest) (string-append
                                       "\n"
                                       (make-string i #\ )
                                       (flatten-seq i rest))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pretty-printing API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Top-level pretty-print, 
(define (seq->string seq)
  (flatten-seq 0 (list (cons seq 0))))

;;;;; Appending Things

; Append two things with a space in between
(define (+++ s1 s2)
  (++ s1 (++ ss s2)))

; Append two things with an arrow in between
(define (+-> s1 s2)
  (+++ s1 (+++ (str "->") s2)))

; Append two things with an equals in between (no space)
(define (+= s1 s2)
  (++ s1 (++ (str "=") s2)))

; Append two things with an equals in between (space)
(define (+=+ s1 s2)
  (+++ s1 (+++ (str "=") s2)))

;;;;; List of sequences
;;;;;   * no separation
;;;;;   * space separated
;;;;;   * Comma separated
;;;;;   * semi-colon separated

(define (lst seqs sep)
  (match seqs
    ['() (nil)]
    [(cons s '()) s]
    [(cons s ss)  (++ s (++ sep (lst ss sep)))]))

(define (no-sep seqs)
  (lst seqs (nil)))

(define (sp-sep seqs)
  (lst seqs ss))

(define (comma-sep seqs)
  (lst seqs com))

(define (semi-sep seqs)
  (lst seqs sem))

(define (vert seqs)
  (lst seqs (nl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Enclosing things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Enclose a sequence with the given open and close sequences
(define (enc open close seq)
  (+++ open (+++ seq close)))

; Enclose the given sequence with curly braces
(define (curly seq)
  (enc (str "{") (str "}") seq))

; Enclose the given sequence with square braces
(define (sqr seq)
  (enc (str "[") (str "]") seq))

; Enclose the given sequence with parentheses
(define (par seq)
  (enc (str "(") (str ")") seq))

; Enclose the given sequence with double-quotes
(define (qt seq)
  (enc (str "\"") (str "\"") seq))

;; Same as above but with an indented sequence

; This is ugly: TODO: Think of a better way
(define (enc-ind open close seq)
  (++ open (++ (nl)
               (++ (space 2)
                   (++ (indent seq)
                        (++ (nl) close))))))

; Enclose and indent with curly braces
(define (curl-ind seq)
  (enc-ind (str "{") (str "}") seq))

; Enclose and indent with square braces
(define (sqr-ind seq)
  (enc-ind (str "[") (str "]") seq))

; Enclose and indent with parantheses
(define (par-ind seq)
  (enc-ind (str "(") (str ")") seq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; End line with various things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (end-semi seq)
  (++ seq (++ sem (nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Miscelanious Characters or Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; When you have a symbol
(define (sym s)
  (str (symbol->string s)))

; A set number of spaces
(define (space i)
  (str (make-string i #\ )))

; A single space
(define ss (str " "))  ; single space

;;; Comma, semi-colons, and colons
(define com (str ","))
(define sem (str ";"))
(define col (str ":"))
