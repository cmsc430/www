#lang crook
{:= E0 E1 F H0 H1 I J K L}
(provide runtime-path)

(require racket/runtime-path)
(define-runtime-path here ".")

(unless (system (string-append "make -C '"
                               (path->string (normalize-path here))
                               "' runtime.o"))
  (error 'build-runtime "could not build runtime"))

(define runtime-path
  (normalize-path (build-path here "runtime.o")))
