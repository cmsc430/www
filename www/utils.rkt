#lang racket
(provide exercise float-right panopto-vid)
(require scribble/base scribble/core scribble/html-properties)

(define exercise-body-style
  (make-style "ExerciseBody" null))

(define exercise-style
  (make-style "Exercise" null))

(define *count* 0)

(define (exercise title . t) 
  (set! *count* (add1 *count*))
  (nested #:style exercise-body-style 
          (para #:style exercise-style (format "Exercise ~a: " *count*) title)
          t))


(define float-right
  (style #f (list (attributes '((style . "float: right"))))))

;; Embed a public panopto video into page
(define (panopto-vid src)
  (elem #:style 
        (style #f (list (alt-tag "iframe") 
                        (attributes 
                          `((src . ,src)
                            (width . "688")
                            (height . "387")
                            (gesture . "media")
                            (allowfullscreen . "")
                            (style . "padding: 0px; border: 1px solid #464646;")))))))

