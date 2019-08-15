#lang racket
(provide (all-defined-out))
(require (only-in xml cdata)
         scribble/core
         scribble/base
         scribble/html-properties)

(define PANDOC
  "pandoc --syntax-definition fish.xml --syntax-definition nasm.xml --syntax-definition ocaml.xml -f markdown -t html")

(define (fancy-c s)
  (fancyverbatim "c" s))

(define (fancy-nasm s)
  (fancyverbatim "nasm" s))

(define (fancy-make s)
  (fancyverbatim "makefile" s))

(define (fancyverbatim l . d)
  (define in (apply string-append (append (list "```" l "\n") d '("\n```"))))
  (with-input-from-string in
    (lambda ()
      (elem #:style (style #f (list 
                               (xexpr-property
                                (cdata #f #f
                                       (with-output-to-string
                                         (lambda ()
                                           (system PANDOC))))
                                (cdata #f #f ""))))))))
