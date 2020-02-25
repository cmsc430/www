#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require slideshow/repl)


;; Title
(slide
  #:title "CMSC 430, Feb 25th 2020"
  (with-size 64 (tt "Grift")))

(slide
  #:title "Update"
  'next
  (item "I have credentials!")
  'next
  (subitem "I got UMD credentials near the end of last week, and am now jumping through all the hoops to get your grades on ELMS/Canvas")
  'next
  (subitem "Hoping to get grading done by the end of the week.")
  'next
  (subitem "I'm scraping the plan of having the TA disambiguate and am going to try and do it through ELMS. You should already see a quiz on ELMS?"))

(slide
  #:title "Word to the wise"
  'next
  (item "I've been seein some of the assignments that have been submitted")
  'next
  (subitem "I am consistently seeing a very serious mistake!")
  'next
  (subitem "Unless you defined" (tt "interp") "using macros, " (it "you must quote your input expression!"))
  'next
  (subitem "Why is the following wrong?")
  'next
  (code (check-equal? (interp (add1 1)) 2)))

(slide
  #:title "Word to the wise"
  'next
  (item "This is partly my fault (and is why I stopped using macros in class)")
  'next
  (subitem "Some of you wrote tests (yay!)")
  'next
  (subitem "But those tests are just testing racket, not your interpreter.")
  'next
  (subitem "This is not a rare mistake. You should _all_ double-check your code."))

(slide
  #:title "Appreciating what we have:"
  'next
  (item "To recap, we've got:")
  'next
  (subitem (it "unary") "arithmetic primitives")
  'next
  (subitem "Conditionals, for branching")
  'next
  (subitem "Errors that halt our programs")
  'next
  (subitem "let-bound variables"))

(slide
  #:title "Grift"
  'next
  (item "What would be useful to add?"))

(slide
  #:title "Fraud's AST"
  (subitem (tt "e = i | b | if e e e | let ((id e)) e | id | p e"))
  'next
  (subitem (tt "p = add1 | sub1 | zero?"))
  'next
  (subitem (tt "id = variable")))

(slide
  #:title "Grift's AST"
  (item "We go")
  'alts
  (list (list (item "from:") 'next (subitem (tt "e = ... | p e")))
        (list (item "to:") 'next (subitem (tt "e = ... | p1 e | p2 e e"))))
  'next
  (subitem (tt "p1 = add1 | sub1 | zero?"))
  'next
  (subitem (tt "p2 = + | -")))

(slide
  #:title "Binary Operators!"
  'next
  (item "Interpretation is easy (as we'll see)")
  'next
  (item "Compilation is not hard, but requires a non-trivial insight (as we'll see)")
  'next
  (item "Can anyone think of why interpretation might be much easier?"))

(slide
  #:title "Meanings"
  'next
  (item "Grift doesn't add much:")
  'next
  'alts
  (list (list (item "First we factor out a rule for primitives")
              (bitmap "grift/factor.png"))
        (list (item "Then we use that rule")
              (bitmap "grift/prim.png"))))

(slide
  #:title "Interpreter"
  'next
  (item "Switch to the terminal..."))

(slide
  #:title "The Compiler"
  'next
  (item "We can't do it naively, consider:")
  'next
  (code (define (compile-+ e0 e1 c)
          (let ((c0 (compile-e e0 c))
                 (c1 (compile-e e1 c)))
            `(,@c0 ; result in rax
              ,@c1 ; result in rax
              (add rax ???))))))

(slide
  #:title "The Compiler"
  'next
  (item "What are some alternatives?")
  'next
  (item "With those alternatives in mind, consider:")
  'next
  (code (+ (add1 2) (add1 3)))
  'next
  (code (+ (add1 2) 3))
  'next
  (code (+ (add1 2) x)))

(slide
  #:title "The Compiler"
  'next
  (item "Before we dive in, let's review compiling `let` and add comments")
  'next
  (subitem "Reminder to José: in assembly they're called `remarks'"))
