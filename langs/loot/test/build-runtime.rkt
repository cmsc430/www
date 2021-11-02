#lang racket
(require a86/interp)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))
