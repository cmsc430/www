#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

(define prof (link "https://www.cs.umd.edu/~dvanhorn/" "David Van Horn"))
(define prof-pronouns "he/him")
(define prof-email "dvanhorn")
(define prof-initials "DVH")

(define semester "summer")
(define year "2023")
(define courseno "CMSC 430")

(define lecture-dates "May 30 -- July 7, 2023")

(define IRB "IRB") 
(define AVW "AVW")
(define KEY "KEY")


(define m1-date "June 14")
(define m2-date "June 28")
(define midterm-hours "24") ; for summer
(define final-date "July 7")
(define elms-url "https://umd.instructure.com/courses/1345891/")


(define racket-version "8.7")

(define staff
  (list (list "William Wegand"  "wwegand@terpmail.umd.edu" "TBD")
        ))


(define lecture-schedule "Weekdays, 12:30pm - 1:50pm")
(define classroom (link "https://umd.zoom.us/j/99831545139?pwd=NWMzbG5mcWVtS0FBWlBnVXU4WVFqQT09" "Zoom"))

(define discord "https://discord.gg/Me7XFYC8")
(define gradescope "https://www.gradescope.com/courses/533338")
