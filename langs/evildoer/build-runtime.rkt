#lang racket
(provide runtime-path)

(require racket/runtime-path)
(define-runtime-path here ".")

(unless (system (string-append "make -C '"
                               (path->string (normalize-path here))
                               "' -s runtime.o"))
  (error 'build-runtime "could not build runtime"))

(define runtime-path
  (normalize-path (build-path here "runtime.o")))

