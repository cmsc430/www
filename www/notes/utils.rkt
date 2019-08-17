#lang racket
(provide (all-defined-out))
(require (for-syntax racket/base racket/file))
(require scribble/manual)

(define-syntax (filebox-include stx)
  (syntax-case stx ()
    [(_ form fn)
    (parameterize ([current-directory (build-path (current-directory) "notes")])
      (let ((s (file->string (syntax->datum #'fn))))
        #`(filebox (link (string-append "code/" fn) (tt fn)) (form #,s))))]))

(define-syntax (filebox-include-fake stx)
  (syntax-case stx ()
    [(_ form fn . s)
     #`(filebox (link (string-append "code/" fn) (tt fn)) (form . s))]))
