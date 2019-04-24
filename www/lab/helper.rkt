#lang racket
(require scribble/manual scribble/core)
(provide (all-defined-out))

(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))
