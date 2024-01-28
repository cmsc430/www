#lang racket
(provide check-nasm-available)
(require racket/gui/dynamic)

(define nasm-msg
  #<<HERE
nasm not found: either you have not installed nasm
or it cannot be found in your PATH: ~a.~a
HERE
  )

(define macosx-msg
  #<<HERE


It appears you launched DrRacket using Finder, which
does not properly set up the PATH environment variable.
Try launching DrRacket from the command line using the
'drracket' command.
HERE
  )

(define (macos?)
  (eq? 'macosx (system-type 'os)))

;; This will lie if you happen to run drracket from /, but that's OK.
(define (launched-with-finder?)
  (equal? (string->path "/") (find-system-path 'orig-dir)))

(define (drracket?)
  (gui-available?))

(define (check-nasm-available)
  (unless (parameterize ([current-output-port (open-output-string)]
                         [current-error-port (open-output-string)])
            (system "nasm -v"))
    (error (format nasm-msg
                   (getenv "PATH")
                   (if (and (drracket?) (macos?) (launched-with-finder?))  macosx-msg "")))))