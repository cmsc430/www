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
  (item ""))


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
  (subitem "Why do we still need" (tt "zero?")))

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

;(slide
;  #:title "Semantics -> Interpreter"
;  'alts
;  (list (list (item "The interpreter can still fit on a single slide")
;              'next
;              (code 
;               (define (interp e)
;                 (match e
;                   [(? integer? i) i]
;                   [`(add1 ,e0)
;                    (+ (interp e0) 1)]
;                   [`(sub1 ,e0)
;                    (- (interp e0) 1)]
;                   [`(if (zero? ,e0) ,e1 ,e2)
;                    (if (zero? (interp e0))
;                        (interp e1)
;                        (interp e2))]))))
;        (list (item "But let's just focus on the new bit:")
;              (code 
;               (define (interp e)
;                 (match e
;                   (...)
;                   [`(if (zero? ,e0) ,e1 ,e2)
;                    (if (zero? (interp e0))
;                        (interp e1)
;                        (interp e2))])))
;              'next
;              (item "the" (tt "zero?") "functions are not the same!")
;              'next
;              (subitem (tt "con") "has no notion of booleans (yet!)"))))
;
;(slide
;  #:title "Let's think through two examples"
;  'alts
;  (list (list (item "Example 1")
;              'next
;              (code (if (zero? 8) 2 3)))
;        (list (item "Example 2")
;              'next
;              (code (if (zero? (add1 -1)) (sub1 2) 3)))))
;
;(slide
;  #:title "Follow these instructions"
;  (item "Here is a quick overview of some useful instructions:")
;  'alts
;  (list (list (item (tt "CMP"))
;              'next
;              (tt "CMP RAX, imm32")
;              'next
;              (item "imm32 sign-extended to 64-bits with RAX.")
;              (subitem "limit of 32 bit immediate not an issue for us (always 0)"))
;        (list (item (tt "JMP"))
;              'next
;              (tt "JMP <label>")
;              'next
;              (item "Jump to an absolute address")
;              (subitem "we are going to let the assembler deal with whether it's direct of indirect"))
;        (list (item (tt "JNE"))
;              'next
;              (tt "JNE <label>")
;              'next
;              (item "IFF" (tt "ZF!=0") "jump to absolute address")
;              (subitem "we are going to let the assembler deal with whether it's direct of indirect"))
;        (list (item (tt "JE"))
;              'next
;              (tt "JE <label>")
;              'next
;              (item "IFF" (tt "ZF==0") "jump to absolute address")
;              (subitem "we are going to let the assembler deal with whether it's direct of indirect"))))
;        
;(slide
;  #:title "Let's write it!")
