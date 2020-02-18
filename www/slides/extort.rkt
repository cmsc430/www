#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require slideshow/repl)


;; Title
(slide
  #:title "CMSC 430, Feb 18th 2020"
  (with-size 64 (tt "Extort")))

(slide
  #:title "First things first"
  'next
  (item "Assignment #2")
  'next
  (subitem "Thanks to those of you that turned it in!")
  'next
  (subitem "Hoping to get grading done by the end of the week.")
  'next
  (item "Two issues:")
  'next
  (subitem "One of my TAs is going to disambiguate github ID <-> UID. If you have concerns about that, contact me ASAP")
  'next
  (subitem "Without ELMS/Canvas, how can I best communicate grades?"))


(slide
  #:title "We've been Duped"
  'next
  (item "On Thursday we saw that even though we have two types and a semantics for our programs, there's all sorts of undefined behaviour.")
  'next
  (item "One strange consequence of this was that our interpreter and compiler behaved differently!")
  'next
  (subitem "Why?"))

(slide
  #:title "Addressing the error of our ways"
  'next
  (item "Recap from last time:")
  'alts
  (list (list 'next (code (add1 #f)))
        (list 'next (code (zero? #f)))
        (list 'next (code (if (zero? #f) 1 2))))
  'next
  (item "Previously, these were undefined")
  'next
  (subitem "In our interpreter we would get a failure because of the errors from the underlying Racket execution")
  'next
  (subitem "In our compiler we'd get junk"))

(slide
  #:title "Extort"
  'next
  (item "Our language" (tt "extort") "the same as" (tt "dupe") (it "except") "we address errors explicitely"))

(slide
  #:title "Extort's AST"
  'next
  (item "No changes:")
  'next
  (subitem (tt "e ::= ... | if e e e | zero? e"))
  'next
  (subitem "Why don't we need to change the AST?"))

(slide
  #:title "C'est man-tick"
  'next
  (item "Type mismatches in" (tt "dupe") "were undefined behavior")
  'next
  (subitem "Do we have to make them defined?")
  'next
  (subitem "What are the pros/cons?"))

(slide
  #:title "Errors Rule"
  'next
  (item "Let's add some, knowing that it's not strictly necessary")
  'next
  (item "Our semantics now relate programs to" (it "answers") "instead of values")
  'next
  (subitem (it "answers") "are either" (it "values") "(as before), or" (it "errors"))
  'next
  (item "We'll just show the new rules, none of the others have changed."))

(slide
  #:title "C'est man-tick"
  'next
  (item "Where can errors occur (currently)?")
  'next
  (bitmap "extort/leaf1.png")
  'next
  (bitmap "extort/leaf2.png")
  'next
  (bitmap "extort/leaf3.png"))

(slide
  #:title "C'est man-tick"
  'next
  (item "Is that it?")
  'next
  (code (if (zero? #f) 1 2)))

(slide
  #:title "C'est man-tick"
  'next
  (item "We also need to propagate errors 'upward'")
  'next
  (bitmap "extort/up1.png")
  'next
  (bitmap "extort/up2.png")
  'next
  (bitmap "extort/up3.png")
  'next
  (bitmap "extort/up4.png"))

(slide
  #:title "Rules are the easy part"
  'next
  (item "How can our implementations match these rules?"))

(slide
  #:title "Let's look at the interpreter"
  (text "We'll do that in the terminal, as it's starting to get a bit too cumbersome"))

(slide
  #:title "Let's experiment"
  (repl-area
    #:prompt "extort> "
    #:height (* client-h 8/10)
    #:width (* client-w 9/10)
    "(require \"extort_interp.rkt\")"))

(slide
  #:title "Now the compiler."
  'next
  (item "What needs to change, if anything?")
  'next
  (item "What should the error message be?"))

(slide
  #:title "Runtime errors"
  'next
  (item "Things need to happen in the RTS and compiler.")
  'next
  (subitem "Runtime system?")
  'next
  (subitem "Compiler?"))

(slide
  #:title "Let's take a look at the RTS and compiler")

(slide
  #:title "Assignment 3"
  (item "Is live")
  'next
  (item "Due next Tuesday.")
  'next
  (subitem "Please tell your fellow students to check the webpage periodically")
  (subitem "If there are any issues that might make you unable to do the assignment on time," (it "talk to me")))
