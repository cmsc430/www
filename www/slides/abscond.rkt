#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require slideshow/repl)


;; Title
(slide
  #:title "CMSC 430, Feb 6th 2020"
  (with-size 64 (tt "Abscond and Blackmail")))

(slide
  #:title "First things first"
  'next
  'alts
  (list (list (item "I messed up!"))
        (list
          'next
          (code 
            (define (get-elems bt)
               (match bt
                 [(leaf) '()]
                 [(node i left right)
                    (cons i (append (get-elems left)
                                    (get-elems right)))])))
          'next
          (item "Was correct!"))))

(slide
  #:title "First things first"
  'next
  (item "The problem was in how the functio was" (it "called"))
  'next
  (repl-area
    #:prompt "sorry> "
    #:height (* client-h 8/10)
    #:width (* client-w 9/10)
    "(require \"trees.rkt\")
     (get-elems (node 1
                      (leaf)
                      (leaf)))"))

(slide
  #:title "Second things second"
  'next
  (item "One last things about quasiquoting")
  'next
  (item "If the thing we want to" (tt "unquote") "is a list, we can use" (tt "unquote splicing") "to put the elements of the list directly in our structure")
  'next
  (repl-area
    #:prompt "uqs> "
    #:height (* client-h 6/10)
    #:width (* client-w 9/10)
    "(define xs '(1 2 3))
     `(huh ,@xs)"))

(slide
  #:title "Lastly, before we begin"
  'next
  (item "Read the lecture notes!")
  'next
  (subitem "It will be increasingly important as we progress through the course"))

(slide
  #:title "If you see what I mean"
  'next
  (item "There are several ways of defining a language")
  'next
  (subitem "By example")
  'next
  (subitem "By informal description")
  'next
  (subitem "Via reference implementation")
  'next
  (subitem "With a formal (mathematical) semantics"))

(slide
  #:title "How it's made"
  'next
  'alts
  (list
    (list (item "C")
          (subitem "Informal Description"))
    (list (item "OCaml")
          (subitem "Defined by its implementation"))
    (list (item "Standard ML")
          (subitem "Fully formalized"))
    (list (item "Python")
          (subitem "Informal Description")
          (subitem "Examples")
          (subitem "Mostly defined by CPython?"))
    (list (item "Haskell")
          (subitem "Informal Description")
          (subitem "Appeal to some formalism"))))

(slide
  #:title "Abscond"
  'next
  (item "For our first language")
  'next
  (subitem "Formal Definition")
  'next
  (subitem "Via reference implementation")
  'next
  (item "If everything is done right, the two should match*"))

(slide
  #:title "Abscond's AST"
  'next
  (item "We've got expressions")
  'next
  (subitem (tt "e ::= i"))
  'next
  (item "We've got `" (tt "i") "'s")
  'next
  (subitem (tt "i ::= ℤ"))
  'next
  (item "That's it"))

(slide
  #:title "Let's argue semantics"
  'next
  (item "Abscond has an" (it "operational") "semantics:")
  'next
  (subitem "We relate a program to its meaning via a" (it "relation") (tt "A[_,_]"))
  'next
  (item "For Abscon we have only a single instance of this relation because we only have a single kind of expression")
  'next
  (subitem (tt "A[")(it "i")(tt ",")(it "i")(tt "]")))

(slide
  #:title "Let's write an interpreter!"
  (repl-area
    #:prompt "abs> "
    #:height (* client-h 8/10)
    #:width (* client-w 9/10)
    "(define (interp e)"))

(slide
  #:title "What about compilers?"
  'next
  (item "Having an interpreter is useful for a few reasons (non-exhaustive):")
  'next
  (subitem "(tend to be) easier to reason about than compilers")
  'next
  (subitem "Easier to experiment with language features")
  'next
  (subitem "They let us 'borrow' more from the host language")
  'next
  (subitem "We can test our compiler against them! (believe me, this is helpful!)"))

(slide
  #:title "What about compilers?"
  'next
  (item "Testing against a reference interpreter:")
  'next
  (code (check-eqv? (source-interp e)
                    (target-interp (source-compile e)))))

(slide
  #:title "Running on our target (x86)"
  'next
  (item "Assume we had a compiler that could produce" (tt "x86") "code")
  'next
  (item "Executables have to know where to start execution")
  'next
  (subitem "This is different from" (tt "main()") "!")
  'next
  (item "We need a" (it "runtime system")))

(slide
  #:title "A simple runtime system"
  'next
    (para
      #:align 'left
      (tt "#include <stdio.h>"))
    (para
      #:align 'left
      (tt "#include <inttypes.h>"))
    (para
      #:align 'left
      (tt ""))
    (para
      #:align 'left
      (tt "int64_t entry();"))
    (para
      #:align 'left
      (tt ""))
    (para
      #:align 'left
      (tt "int main(int argc, char** argv) {"))
      (para
        #:align 'left
        (tt "    int64_t result = entry();"))
      (para
        #:align 'left
        (tt "    printf(\"%\" PRId64 \"\\n\", result);"))
      (para
        #:align 'left
        (tt "    return 0;"))
    (para
      #:align 'left
      (tt "}")))

(slide
  #:title "The object we desire"
  'next
  (item "Let's run the following to get a linkable RTS")
  (subitem (tt "gcc -m64 -c -o main.o main.c")))

(slide
  #:title "What do we want?"
  'next
  (item "Let's look at an example assembly file."))

(slide
  #:title "Making an AST"
  'next
  (item "In OCaml we'd make a few types:")
  'next
  (subitem (tt "type Reg = RAX"))
  'next
  (subitem (tt "type Arg = Int | Reg"))
  'next
  (subitem (tt "type Lab = Symbol"))
  'next
  (subitem (tt "type Inst = Lab | RET | MOV Arg Arg"))
  'next
  (subitem (tt "type Asm = Inst list"))
  'next
  (item "In Racket we will do none of that")
  'next
  (subitem "Dynamic types!"))

(slide
  #:title "Our first compiler"
  'next
  (repl-area
    #:prompt "abs> "
    #:height (* client-h 7/10)
    #:width (* client-w 9/10)
    "(define (compile e)")
  'next
  (item "lol"))

(slide
  #:title "pretty-print"
  'next
  (item "Good: now we have the structure we want")
  'next
  (item "Bad: Assemblers take flat strings, not racket structures")
  'next
  (item "Solution: Write a pretty-printer"))

(slide
  #:title "Settling an argument"
  'next
  (code
    (define (arg->string a)
      (match a
        [`rax "rax"]
        [n (number->string n)]))))

(slide
  #:title "Settling an argument"
  'next
  (code
    (define (instr->string i)
      (match i
        [`(mov ,a1 ,a2)
         (string-append "\tmov "
                        (arg->string a1) ", "
                        (arg->string a2) "\n")]
        [`ret "\tret\n"]
        [l (string-append (label->string l) ":\n")])))
  'next
  (item "the rest are in the lecture notes online!"))

(slide
  #:title "Take it for a spin")

(slide
  #:title "Our Second Compiler"
  'next
  (item "Let's add a feature to our compiler: incrementing and decrementing.")
  'next
  (item "We'll call it blackmail"))

(slide
  #:title "Blackmail's AST"
  'next
  (item "We've got expressions")
  'next
  (subitem (tt "e ::= i | add1 e | sub1 e"))
  'next
  (item "We've got `" (tt "i") "'s")
  'next
  (subitem (tt "i ::= ℤ"))
  'next
  (item "And we've got two functions:")
  'next
  (subitem (tt "add1 : ℤ → ℤ"))
  'next
  (subitem (tt "sub1 : ℤ → ℤ"))
  'next
  (item "That's it"))

(slide
  #:title "It's dangerous to go alone"
  'next
  (item "In Abscond, it was only integers, parsing was trivial.")
  'next
  (subitem "Now we have to make sure what we have is actually an expression.")
  'next
  (code
  (define (expr? x)
    (match x
      [(? integer? i) #t]
      [`(add1 ,x) (expr? x)]
      [`(sub1 ,x) (expr? x)]
      [_ #f])))
  'next
  (item "As mentioned on Tuesday, since we don't have static types, we can use validation like the above to make sure our values are well formed"))

(slide
  #:title "Blackmail is all about interpretation"
  'next
  (item "In Abscond, interpreter was 'trivial'")
  'next
  (subitem "For blackmail we have to think a bit more")
  'next
  (code
  (define (interp e)
    (match e
      [(? integer? i) i]
      [`(add1 ,e0)
       (match (interp e0)
         [i0 (+ i0 1)])]
      [`(sub1 ,e0)
       (match (interp e0)
         [i0 (- i0 1)])]))))

(slide
  #:title "Seeing how blackmail feels")

(slide
  #:title "What's different about compilation?"
  'next
  (item "Runtime system?")
  'next
  (item "What about entry?")
  'next
  (item "What about return?")
  'next
  (code
  (define (compile e)
    (append '(entry)
            (compile-e e)
            '(ret)))))

(slide
  #:title "compile-e coyote"
  'next
  (item "Take a deep breath")
  'next
  (code
  (define (compile-e e)
    (match e
      [(? integer? i) `((mov rax ,i))]
      [`(add1 ,e0)
       (let ((c0 (compile-e e0)))
         `(,@c0
           (add rax 1)))]
      [`(sub1 ,e0)
       (let ((c0 (compile-e e0)))
         `(,@c0
           (sub rax 1)))]))))

(slide
  #:title "Seeing how compiled blackmail feels")

(slide
  #:title "Assignment 2"
  (item "Details on the website"))
