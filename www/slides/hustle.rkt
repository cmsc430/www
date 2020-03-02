#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require slideshow/repl)

;; Title
(slide
  #:title "CMSC 430, March 3rd 2020"
  (with-size 64 (tt "Hustle")))


(slide
  #:title "Stacks"
  'next
  (item "Let's review stacks a little bit")
  'next
  (item "One thing I was trying to get across, but may have failed:")
  'next
  (subitem "There are many ways to use stacks to store temporaries!")
  'next
  (subitem "Only thing that matters: that it works."))

(slide
  #:title "Stacks: Part 1"
  'next
  (item "In AMD64, there are two registers normally used for the stack:")
  'next
  (subitem (tt "rsp") "and" (tt "rbp"))
  'next
  (item "Importantly, these registers are not special!")
  'next
  (subitem "In fact, in the architecture specification they are explicitely called out as" (it "general purpose")))

(slide
  #:title "Stacks: Part 2"
  'next
  (item "The idea behind having two:")
  'next
  (subitem "The stack pointer points to the \"top\" of the stack")
  'next
  (subitem "The base pointer points to the \"bottom\" of the stack")
  'next
  (item "The 'distance' between the determines how many things are currently on the stack."))

(slide
  #:title "Stacks: Part 3"
  'next
  (item "Let's take a look:")
  'next
  (bitmap "stacks/stack1.png")
  'next
  (item "Even with both" (tt "rsp") "and" (tt "rbp") "we have to keep track of things"))

(slide
  #:title "Stacks: Part 4"
  'next
  (item "Since we're keeping track of things, the following are all equivalent:")
  'next
  'alts
  (list (list (bitmap "stacks/stack2.png"))
        (list (bitmap "stacks/stack3.png"))))

(slide
  #:title "Stacks: Part 5"
  'next
  (item "But I also said there was nothing special about" (tt "rsp") "and" (tt "rbp"))
  'next
  'alts
  (list (list (bitmap "stacks/stack4.png"))
        (list (bitmap "stacks/stack5.png"))))

(slide
  #:title "Stacks: Part 6"
  'next
  (item "We went with the last one:")
  'next
  (bitmap "stacks/stack5.png")
  'next
  (item "Why not use" (tt "rbp") "?")
  'next
  (subitem "Because" (tt "rbp") "is special to C")
  (subitem ":("))

(slide
  #:title "Our languages so far:"
  'next
  (item "Each lecture we're seeing the complexities of our language grow")
  'next
  (item "Most of the time these new features change things in our interpreter/compiler but not in our RTS")
  'next
  (item "Today is an RTS day.")
  'next
  (subitem "Which is also a compiler day, to take advantage of our new RTS!"))

(slide
  #:title "Hustle"
  'next
  (item "Hustle is going to introduce a notion of a" (it "heap") "to our RTS")
  'next
  (item "We will use the heap to implement" (it "boxed values")))

(slide
  #:title "What's in the box?"
  'next
  (item "A good short-hand:")
  'next
  (subitem "Box = not on the stack")
  'next
  (item "In general, boxed values are things you need to derefence a pointer to get.")
  'next
  (item "But not all things that you need to dereference a pointer are 'boxed'"))

(slide
  #:title "Boxing Day"
  (repl-area
    #:prompt "racket> "
    #:height (* client-h 8/10)
    #:width (* client-w 9/10)
    "; show box and unbox"))

(slide
  #:title "What's in the box?"
  'next
  (item "Boxes, without a notion of pointer equality, are uninteresting.")
  'next
  (item "In our language, boxes are single-element vectors")
  'next
  (item "For now, we can see boxes as an important stepping stone to something much more important:")
  'next
  (subitem (tt "cons")))

(slide
  #:title "Getting Box/Car on track"
  'next
  (item "Goal for today:")
  'next
  (item "Understand how things like" (tt "box") "and" (tt "cons") "are implemented"))

(slide
  #:title "Hustle's AST"
  'next
  (item "We're only showing the new stuff:")
  'next
  (subitem (tt "e = ..."))
  'next
  (item "Expressions are unchanged!")
  'next
  (subitem (tt "p1 = ... | box | unbox | car | cdr"))
  'next
  (subitem (tt "p2 = ... | cons"))
  'next
  (item "Is this enough?")
  'next
  (subitem "Not if we want programs to have boxed results.")
  'next
  (subitem "v = ... | (box v) | (cons v v) | '()"))

(slide
  #:title "Find value in the hustle"
  'next
  (item "We've got 3 new values, what do we do about representation?")
  'next
  (item "Before: All values were 'flat'")
  'next
  (item "Now: values can be arbitrarily big")
  'next
  (subitem "So they won't all fit in a machine word!")
  'next
  (item "Idea:")
  'next
  (subitem "Make distinction between flat and boxed values")
  'next
  (subitem "Then make distinctions between the flat (immediate) and boxed values"))

(slide
  #:title "From grifters to hustlers"
  'next
  'alts
  (list (list (item "Before we had the following:")
              'next
              (code 
                (define imm-shift        1)
                (define imm-type-mask    (sub1 (shift 1 imm-shift)))
                (define imm-type-int     #b0)
                (define imm-val-true     #b11)
                (define imm-val-false    #b01)))
        (list (item "Which becomes:")
              (code
                (define result-shift     3)
                (define result-type-mask (sub1 (shift 1 result-shift)))
                (define type-imm         #b000)
                (define type-box         #b001)
                (define type-pair        #b010)))))

(slide
  #:title "We need more"
  'next
  (item "However, this only helps us determine the types")
  'next
  (item "We need more in order to disambiguate the values"))

(slide
  #:title "All the bits"
  'next
  (code
    (define result-shift     3)
    (define result-type-mask (sub1 (shift 1 result-shift)))
    (define type-imm         #b000)
    (define type-box         #b001)
    (define type-pair        #b010)
    
    (define imm-shift        (+ 3 result-shift))
    (define imm-type-mask    (sub1 (shift 1 imm-shift)))
    (define imm-type-int     (shift #b000 result-shift))
    (define imm-type-true    (shift #b001 result-shift))
    (define imm-type-false   (shift #b010 result-shift))
    (define imm-type-empty   (shift #b011 result-shift))))

(slide
  #:title "Follow these instructions"
  (item "Here is a quick overview of some useful facts")
  'alts
  (list (list (item (tt "RSP"))
              'next
              (tt "MOV RAX, [RSP]")
              'next
              (tt "MOV RAX, [RSP - 8]")
              'next
              (tt "MOV [RSP - 8], RAX")
              'next
              (tt "MOV RAX, [RSP - 16]")
              'next
              (item "we will call this" (tt "offset")))
        (list (item "run the following at your terminal")
              (subitem (tt "ulimit -a"))
              'next
              (item "If I did my math right (always questionable), we should be able to store ~1 million let-bound variables."))))
        
(slide
  #:title "Let's write it!")
