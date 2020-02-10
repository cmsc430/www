#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require slideshow/repl)


;; Title
(slide
  #:title "CMSC 430, Feb 11th 2020"
  (with-size 64 (tt "Con")))

(slide
  #:title "First things first"
  'next
  (item "Reflection on what a compiler" (it "is")))

(slide
  #:title "Recap"
  'next
  (item "Compilers translate a" (it "source language") "to some" (it "target language")))

(slide
  #:title "Recap"
  'next
  (item "In this class we will have" (it "many") "source lanagues")
  'next
  (item "We will only have one target language"))

(slide
  #:title "Our languages so far:"
  'next
  (item "The two languages so far are quite limited but still interesting")
  'next
  (subitem "We could extend the runtime system to allow some sort of integer-IO")
  'next
  (subitem "We could imagine `finishing up' a calculator-like language")
  'next
  (item "However there are a few things that, without them, we'd be hamstrung in developing more sophisticated languages")
  'next
  (subitem "We'd like to be able to name things: variables")
  'next
  (subitem "We'd like to be able to" (it "make decisions") ", i.e. perform branching"))

(slide
  #:title "Language du jour"
  'next
  (item "We will look at naming things next week")
  'next
  (item "Today, we will look at branching via conditionals")
  'next
  (subitem "Because we want to focus on the branching aspect, we will not introduce booleans (yet!)")
  'next
  (subitem "Instead we will allow only a single predicate, that we define up-front"))

(slide
  #:title "Con"
  'next
  (item "Our language" (tt "Con") "is going to extend" (tt "blackmail") "with only one new syntactic feature"))

(slide
  #:title "Con's AST"
  'next
  (item "We've got expressions")
  'next
  (subitem (tt "e ::= i | add1 e | sub1 e | if (zero? e) e e"))
  'next
  (item "Everything works, as before...")
  'next
  (subitem "but now we can decide between two programs depending on whether some expression results in" (tt "0"))
  'next
  (item "Important Point:")
  'next
  (subitem "This does not mean we have booleans!"))

(slide
  #:title "What does it mean?"
  'next
  (item "This is a job for semantics"))

(slide
  #:title "Some Antics"
  'next
  (item "The meaning of integers is unchanged since" (tt "abscond"))
  'next
  (bitmap "con/int.png"))

(slide
  #:title "Some Antics"
  'next
  (item "The meaning of" (tt "add1/sub1") "is unchanged since" (tt "blackmail"))
  'next
  (bitmap "con/add1.png")
  'next
  (bitmap "con/sub1.png"))

(slide
  #:title "Some Antics"
  'next
  (item "The new stuff in" (tt "con"))
  'next
  (bitmap "con/ift.png")
  'next
  (bitmap "con/iff.png"))

(slide
  #:title "Semantics -> Interpreter"
  'alts
  (list (list (item "The interpreter can still fit on a single slide")
              'next
              (code 
               (define (interp e)
                 (match e
                   [(? integer? i) i]
                   [`(add1 ,e0)
                    (+ (interp e0) 1)]
                   [`(sub1 ,e0)
                    (- (interp e0) 1)]
                   [`(if (zero? ,e0) ,e1 ,e2)
                    (if (zero? (interp e0))
                        (interp e1)
                        (interp e2))]))))
        (list (item "But let's just focus on the new bit:")
              (code 
               (define (interp e)
                 (match e
                   (...)
                   [`(if (zero? ,e0) ,e1 ,e2)
                    (if (zero? (interp e0))
                        (interp e1)
                        (interp e2))])))
              'next
              (item "the" (tt "zero?") "functions are not the same!")
              'next
              (subitem (tt "con") "has no notion of booleans (yet!)"))))

(slide
  #:title "Let's think through two examples"
  'alts
  (list (list (item "Example 1")
              'next
              (code (if (zero? 8) 2 3)))
        (list (item "Example 2")
              'next
              (code (if (zero? (add1 -1)) (sub1 2) 3)))))

(slide
  #:title "Follow these instructions"
  (item "Here is a quick overview of some useful instructions:")
  'alts
  (list (list (item (tt "CMP"))
              'next
              (tt "CMP RAX, imm32")
              'next
              (item "imm32 sign-extended to 64-bits with RAX.")
              (subitem "limit of 32 bit immediate not an issue for us (always 0)"))
        (list (item (tt "JMP"))
              'next
              (tt "JMP <label>")
              'next
              (item "Jump to an absolute address")
              (subitem "we are going to let the assembler deal with whether it's direct of indirect"))
        (list (item (tt "JNE"))
              'next
              (tt "JNE <label>")
              'next
              (item "IFF" (tt "ZF!=0") "jump to absolute address")
              (subitem "we are going to let the assembler deal with whether it's direct of indirect"))
        (list (item (tt "JE"))
              'next
              (tt "JE <label>")
              'next
              (item "IFF" (tt "ZF==0") "jump to absolute address")
              (subitem "we are going to let the assembler deal with whether it's direct of indirect"))))
        
(slide
  #:title "Let's write it!")
