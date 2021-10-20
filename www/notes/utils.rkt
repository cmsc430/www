#lang racket
(provide (all-defined-out))
(require (for-syntax racket/runtime-path racket/base racket/file))
(require scribble/manual racket/runtime-path)
(require (for-label (except-in racket compile) a86))

(begin-for-syntax
  (define-runtime-path notes "../../langs/"))

(define-runtime-path notes "../../langs")

(define-syntax (filebox-include stx)
  (syntax-case stx ()
    [(_ form fn)
    (parameterize ()
      (let ((s (file->string (build-path notes (syntax->datum #'fn)))))
        #`(filebox (link (string-append "code/" fn) (tt fn)) (form #,(datum->syntax #'form s)))))]))

(define ((make-codeblock-include ctxt) fn)
   (filebox (link (string-append "code/" fn) (tt fn))
            (typeset-code #:context #'h (file->string (build-path notes fn)))))

(define-syntax (filebox-include-fake stx)
  (syntax-case stx ()
    [(_ form fn . s)
     #`(filebox (link (string-append "code/" fn) (tt fn)) (form . #,(map syntax-e (syntax->list #'s))))]))

(define (save-file f s)
  (with-output-to-file f (λ () (display s)) #:exists 'replace))

(define (binary i [len 0])
  (typeset-code #:block? #f #:indent 0
                (string-append "#b"
                               (~a (number->string i 2)
                                   #:left-pad-string "0"
                                   #:align 'right
                                   #:min-width len))))
