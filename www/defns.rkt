#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

;(define prof1 (link "https://jmct.cc" "José Manuel Calderón Trilla"))
;(define prof1-pronouns "he/him")
;(define prof1-email "jmct@cs.umd.edu")
;(define prof1-initials "JMCT")

(define prof1 (link "https://www.cs.umd.edu/~dvanhorn/" "David Van Horn"))
(define prof1-pronouns "he/him")
(define prof1-email "dvanhorn@cs.umd.edu")
(define prof1-initials "DVH")

(define semester "fall")
(define year "2024")
(define courseno "CMSC 430")

(define lecture-dates "" #;"May 30 -- July 7, 2023")

(define IRB "IRB") 
(define AVW "AVW")
(define KEY "KEY")


(define m1-date "TBD")
(define m2-date "TBD")
(define midterm-hours "24")
(define final-date "TBD")
(define elms-url "https://umd.instructure.com/courses/1368381")


(define racket-version "8.13")

(define staff
  (list (list "Pierce Darragh" "pdarragh@umd.edu")
        (list "Kalyan Bhetwal" "kbhetwal@umd.edu")
        (list "Justin Frank" "jpfrank@umd.edu")
	(list "Deena Postol" "dpostol@umd.edu")
        (list "Caspar Popova" "caspar@umd.edu")
        (list "Emma Shroyer" "eshroyer@umd.edu")
	(list "Kazi Tasnim Zinat" "kzintas@umd.edu")
	#;(list "Fuxiao Liu" "fl3es@umd.edu")
	#;(list "Vivian Chen" "vchen8@terpmail.umd.edu")
	#;(list "Ian Morrill" "imorrill@terpmail.umd.edu")
	#;(list "Matthew Schneider" "mgl@umd.edu")
	#;(list "Rhea Jajodia" "rjajodia@terpmail.umd.edu")
	#;(list "Syed Zaidi" "szaidi@umd.edu")
	#;(list "William Wegand" "wfweg@verizon.net")
	#;(list "Wilson Smith" "smith@umd.edu")
	#;(list "Yuhwan Lee" "ylee9251@terpmail.umd.edu")
        ))


(define lecture-schedule1 "TTh, 2:00-3:15pm")

(define classroom1 "LEF 2205")

;(define discord "TBD")
(define piazza "https://piazza.com/umd/fall2024/cmsc430/home")
(define gradescope "https://www.gradescope.com/courses/818295")

(define feedback "https://forms.gle/A6U3CCR2KyA86UTh6")
