#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require slideshow/repl)


;; Title
(slide
  #:title "CMSC 430, Feb 13th 2020"
  (with-size 64 (tt "Dupe")))

(slide
  #:title "First things first"
  'next
  (item "Consider the following Racket code")
  ;(item "Scope isn't just an alternative to Listerine"))
  'next
  'alts
  (list (list
          (code
            x)
          'next
          (item "Is" (tt "x") "`free'?"))
        (list
          (code
            (cons x y))
          'next
          (item "Is" (tt "x") "`free'?")
          'next
          (item "Is" (tt "y") "`free'?"))
        (list
          (code
            (lambda (x) (cons x y)))
          'next
          (item "Is" (tt "x") "`free'?")
          'next
          (item "Is" (tt "y") "`free'?"))
        (list
          (code
            (let ((y 5))
                 (lambda (x) (cons x y))))
          'next
          (item "Is" (tt "x") "`free'?")
          'next
          (item "Is" (tt "y") "`free'?"))))


(slide
  #:title "Our languages so far:"
  'next
  (item "We can branch based on computed values, but it's a bit clunky")
  (subitem "We'd like to a) understand the clunkiness, and b) fix it"))

(slide
  #:title "Language du jour"
  'next
  (item "Today, we will double the number of types we can deal with!")
  'next
  (subitem "Right now, we've only got integers")
  'next
  (subitem "By the end of today we'll have integers" (it "and") "booleans"))

(slide
  #:title "Dupe"
  'next
  (item "Our language" (tt "Dupe") "is going to modify" (it "and") "extend" (tt "con")))

(slide
  #:title "Con's AST"
  'next
  (item "Let's review")
  'next
  (subitem (tt "e ::= i | add1 e | sub1 e | if (zero? e) e e"))
  'next
  (item "This is clunky")
  'next
  (subitem (tt "if") "is `hard coded' to dispatch based on" (tt "zero?") "and can do nothing else")
  'next
  (item "Let's make" (tt "if") "be more like what we experience in other languages")
  'next
  (subitem "It should dispatch on arbitrary boolean expressions!"))

(slide
  #:title "Dupe's AST"
  'next
  (item "Some changes:")
  'next
  (subitem (tt "e ::= ... | if e e e | zero? e"))
  'next
  (item "This is less clunky")
  'next
  (subitem (tt "if") "is no longer `hard coded' to dispatch based on" (tt "zero?"))
  'next
  (item (tt "if") "is now more like what we experience in other languages")
  'next
  (subitem "Thing to think about:")
  'next
  (subitem "Why do we still need" (tt "zero?") "(if at all)"))

(slide
  #:title "Valley Date"
  (item "Syntax validation for" (tt "Dupe") "is just what you might expect")
  'next
  (code
    (define (expr? x)
      (match x
        [(? integer?) #t]
        [(? boolean?) #t]
        [`(add1 ,x) (expr? x)]
        [`(sub1 ,x) (expr? x)]
        [`(zero? ,x) (expr? x)]
        [`(if ,x ,y ,z)
         (and (expr? x)
              (expr? y)
              (expr? z))]
        [_ #f]))))

(slide
  #:title "Some Ant, ick!"
  'next
  (item "The meaning of integers is subsumed by a meaning for" (it "values"))
  'next
  (bitmap "dupe/val.png"))

(slide
  #:title "Some Ant, ick!"
  'next
  (item "The meaning of" (tt "add1/sub1") "is unchanged since" (tt "blackmail"))
  'next
  (bitmap "dupe/add1.png")
  'next
  (bitmap "dupe/sub1.png"))

(slide
  #:title "Some Ant, ick!"
  'next
  (item "The meaning of" (tt "if") "has changed a bit")
  'next
  (bitmap "dupe/ift.png")
  'next
  (bitmap "dupe/iff.png"))

(slide
  #:title "Some Ant, ick!"
  'next
  (item "Now we need a separate meaning for" (tt "zero?"))
  'next
  (bitmap "dupe/zt.png")
  'next
  (bitmap "dupe/zf.png"))

(slide
  #:title "Some Ant, ick!"
  'next
  (item "Let's take a look at" (tt "if") "again, with some helper rules")
  'next
  (vc-append 50
    (hc-append 50 (bitmap "dupe/zt.png")
          (bitmap "dupe/zf.png"))
    (hc-append 50 (bitmap "dupe/itt.png")
          (bitmap "dupe/itf.png")
          (bitmap "dupe/iti.png"))))

(slide
  #:title "Things to consider"
  'next
  (item "All of the following are" (it "syntactically valid") "programs")
  'next
  (item "What do you expect them to do (i.e. what do the semantics say about them)?")
  'next
  (code
    (if 0 1 2))
  'next
  (code
    (if (zero? 1) 1 2))
  (code
    (if #t 1 2))
  'next
  (code
    (if #t (add1 #f) 2)))

(slide
  #:title "Let's look at the interpreter"
  (text "We'll do that in the terminal, as it's starting to get a bit too cumbersome"))

(slide
  #:title "Let's experiment"
  (repl-area
    #:prompt "dupe> "
    #:height (* client-h 8/10)
    #:width (* client-w 9/10)
    "(require \"dupe_interp.rkt\")"))

(slide
  #:title "Things are bit tricky"
  'next
  (item "Now let's think about generating x86 code")
  'next
  (item "Clearly," (tt "#f") "is not the same as" (tt "0"))
  'next
  (subitem "How do we make sure that the values from the different types don't get mixed up?"))

(slide
  #:title "** several people are typing..."
  'next
  (item "This is the crux of a" (it "type system"))
  'next
  (item "Different type systems have different tradeoffs")
  'next
  (item "We are going to implement a" (it "dynamic") "type system")
  'next
  (subitem "What does this imply about how our implementation doesn't get values from different types mixed up?"))

(slide
  #:title "Tag your int"
  'next
  (bitmap "dupe/bitmap-lol.png"))

(slide
  #:title "Tag your int"
  'next
  (item "We have to choose: which type gets" (tt "1") "?")
  'next
  (item "Either can work, but we'll argue that" (tt "booleans") "should get the" (tt "1")))

(slide
  #:title "Tag your int"
  'next
  (item "What does this imply about our")
  'next
  (subitem "Runtime system?")
  'next
  (subitem "Compiler?"))

(slide
  #:title "Let's take a look at the RTS and compiler")

(slide
  #:title "Assignment 3"
  (item "Will go live tomorrow")
  (subitem "Please tell your fellow students to check the webpage periodically")
  (subitem "If there are any issues that might make you unable to do the assignment on time," (it "talk to me")))
