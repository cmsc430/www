#lang racket
(provide runtime-path)

(require racket/runtime-path)
(define-runtime-path here ".")

(system (string-append "make -C "
                       (path->string (normalize-path here))
		       " runtime.o"))

(define runtime-path
  (normalize-path (build-path here "runtime.o")))