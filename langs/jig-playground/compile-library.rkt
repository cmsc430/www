#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "ast.rkt" a86/printer a86/ast)

;; String -> Void
;; Compile contents of given library file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (read-line p) ; ignore provide line      
      (displayln (asm-string (compile-library (read-defns p))))
      (close-input-port p))))

;; Port -> [Listof Defn]
(define (read-defns p)
  (let ((d (read p)))
    (if (eof-object? d)
        '()
        (cons (parse-d d)
              (read-defns p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [Listof Defn] -> Asm
(define (compile-library ds)
  (prog (externs)
        (library-globals ds)
        (compile-defines ds)
        (Label 'raise_error_align)
        (Sub rsp 8)
        (Jmp 'raise_error)))

;; [Listof Defn] -> [Listof Global]
(define (library-globals ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (match d
       [(Defn f xs e)
        (seq (Global (symbol->label f))
             (library-globals ds))])]))
