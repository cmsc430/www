#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require slideshow/repl)

;; Title
(slide
  #:title "CMSC 430, Feb 20th 2020"
  (with-size 64 (tt "Fraud")))

(slide
  #:title "Jobs?")

(slide
  #:title "Jobs."
  'next
  (item "I asked the following question on the bird site:")
  'next
  (subitem "A student in my class is asking about what PL/Compiler career options there are for folks fresh out of a BS program with an interest in that stuff. [...] Where else can I point them to?"))

(slide
  #:title "Jobs."
  'next
  'alts
  (list (list (bitmap "jobs/louis.png.new"))
        (list (bitmap "jobs/mike.png.new"))
        (list (bitmap "jobs/matt.png.new"))
        (list (bitmap "jobs/v.png.new"))
        (list (bitmap "jobs/rich.png.new"))
        (list (bitmap "jobs/tran.png.new"))
        (list (bitmap "jobs/kavon.png.new"))
        (list (bitmap "jobs/amazon.png.new"))
        (list (bitmap "jobs/aws.png.new"))
        (list (bitmap "jobs/cpu.png.new"))
        (list (bitmap "jobs/fb.png.new"))
        (list (bitmap "jobs/fb2.png.new"))
        (list (bitmap "jobs/igalia.png.new"))
        (list (bitmap "jobs/oracle.png.new"))
        (list (bitmap "jobs/reservoir.png.new"))
        (list (bitmap "jobs/sifive.png.new"))
        (list (bitmap "jobs/veracode.png.new"))))

(slide
  #:title "A thing to discuss"
  'next
  'alts
  (list (list (bitmap "jobs/ms2.png"))
        (list (bitmap "jobs/industry1.png"))
        (list (bitmap "jobs/industry2.png"))))

(slide
  #:title "Our languages so far:"
  'next
  (item "We're starting to get a feel for the process")
  'next
  (item "We can add, we can branch, we can falsify, we can error!")
  'next
  (item "However, lots of things still missing that we enjoy in traditional languages")
  'next
  (subitem "Can you name any?")
  'next
  (subitem "We'd like to be able to name at least one thing!"))

(slide
  #:title "Fraud"
  'next
  (item "Our language" (tt "Fraud") "is going to extend" (tt "extort") "with only one new syntactic feature"))

(slide
  #:title "Fraud's AST, Take #1"
  'next
  (item "You know the drill, we had this before:")
  'next
  (subitem (tt "e = i | b | add1 e | sub1 e | if e e e | zero? e"))
  'next
  (item "We just add two things to it:")
  'next
  (subitem (tt "... | let ((id e)) e | id"))
  'next
  (item "What do you expect this to do?")
  'next
  (item "Important Point:")
  'next
  (subitem "This syntax only allows a single binding at a time!"))

(slide
  #:title "Fraud's AST, Take #2"
  'next
  (item "The" (tt "add1 e | sub1 e | zero? e") "are starting to annoy me")
  'next
  (item "Let's factor them out:")
  'next
  (subitem (tt "e = i | b | if e e e | let ((id e)) e | id | p e"))
  'next
  (subitem (tt "p = add1 | sub1 | zero?"))
  'next
  (subitem (tt "id = variable"))
  'next
  (item "Is this equivalent?"))

(slide
  #:title "The sin tax"
  'next
  (item "Extending the syntax validation is a bit more subtle this time")
  'next
  (item "We no longer fit on one slide :( let's ignore the easy stuff")
  'next
  (code

    (define keywords '(if let add1 sub1 zero?))
    (define (expr? x)
      (match x
        [(? symbol?)
            (not (memq x keywords))]

        [`(let ((,x ,y)) ,z)
         (and (symbol? x)
              (expr? y)
              (expr? z))]
        [_ #f]))))

(slide
  #:title "Whisper Words of Wisdom"
  'next
  (item "The semantics of" (tt "let") "are a bit trickier! So,")
  'next
  (item "we aren't going to go through them in class.")
  'next
  (item "read. the. lecture. notes.")
  'next
  (item "Let's just try some stuff at a racket repl"))

(slide
  #:title "Let it be"
  (repl-area
    #:prompt "racket> "
    #:height (* client-h 8/10)
    #:width (* client-w 9/10)
    "(let ((i #t)) \"be\")"))

(slide
  #:title "Key insight"
  'next
  (item "We no longer have the luxury of ignoring the" (it "context") "that our expressions are in")
  'next
  (item "Think of what the interpreter has to be able to keep track of in order to evaluate" (tt "e")":")
  (code
    (let ((x 5)) e))
  'next
  (item "What does our interpreter need to have at its disposal?"))

(slide
  #:title "Key insight"
  'next
  (item "We want some sort of mapping from" (it "names") "to" (it "expressions"))
  'next
  (item "Tradition dictates that we call this an" (it "environment"))
  'next
  (item "In OCaml we could write: " (tt "type env = (name * expr) list"))
  'next
  (item "But we're not in OCaml, so what do we do?"))

(slide
  #:title "Let's think through two examples"
  'alts
  (list (list (item "Example 1")
              'next
              (code (if (zero? 8)
                      (let ((x 5)) (add1 x))
                      3)))
        (list (item "Example 2")
              'next
              (code (let ((x 5))
                      (if (zero? x)
                        (add1 x)
                        (let ((y 5))
                          (add1 x))))))))

(slide
  #:title "In(terp)")

(slide
  #:title "Compile"
  'next
  (item "For the interpreter, we could just pass around the environment.")
  'next
  (item "What are some ways we could achieve the same thing for the compiler?"))

(slide
  #:title "Let's think through two examples (differently)"
  'alts
  (list (list (item "Example 1")
              'next
              (code (if (zero? 8)
                      (let ((x 5)) (add1 x))
                      3)))
        (list (item "Example 2")
              'next
              (code (let ((x 5))
                      (if (zero? x)
                        (add1 x)
                        (let ((y 5))
                          (add1 x))))))))

(slide
  #:title "Compile"
  'next
  (item "Stacks!")
  'next
  (item "In our languages we only care about how" (it "nested") "we are")
  'next
  (item "This means that we can" (it "statically") "know `how far' we are from the binding site"))

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
