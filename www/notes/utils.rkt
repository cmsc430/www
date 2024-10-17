#lang racket
(provide (all-defined-out))
(require (for-syntax racket/runtime-path racket/base racket/file pkg/lib)
         (for-meta 2 racket/base pkg/lib))
(require scribble/manual racket/runtime-path pkg/lib)
(require (for-label (except-in racket compile) a86))
(require images/icons/file)


(begin-for-syntax
  (define-runtime-path langs (pkg-directory "langs"))
  (define-runtime-path a86 (build-path (pkg-directory "a86") "a86")))

(define-runtime-path langs (pkg-directory "langs"))
(define-runtime-path a86 (build-path (pkg-directory "a86") "a86"))

(define-syntax (filebox-include stx)
  (syntax-case stx (a86)
    [(_ form lang fn)
     (parameterize ()
       (let ((s (file->string (build-path (syntax-case #'lang (a86)
                                            [a86 a86]
                                            [_ (build-path langs (symbol->string (syntax->datum #'lang)))])
                                          (syntax->datum #'fn)))))
         #`(filebox (link (string-append "code/" fn) (tt fn)) (form #,(datum->syntax #'form s)))))]))

(define ((make-codeblock-include ctxt) fn) ;; Should h be ctxt!?  Seems like a bug
   (filebox (link (string-append "code/" fn) (tt fn))
            (typeset-code #:context #'h (file->string (build-path langs fn)))))

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

(define (src-code lang)
  (margin-note (small-save-icon) " "
               (link (string-append "code/" (string-downcase lang) ".zip") "Source code")
	       "."))
