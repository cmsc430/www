#lang racket
(require racket/runtime-path)
(provide runtime-path)

(define-runtime-path here ".")

(void
 (system (string-append "make -C '"
                        (path->string (normalize-path here))
                        "' runtime.o")))

(define runtime-path
  (path->string
   (normalize-path (build-path here "runtime.o"))))
