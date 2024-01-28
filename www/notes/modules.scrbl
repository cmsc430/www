#lang scribble/manual

@(require (for-label (except-in racket compile ...) a86))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../fancyverb.rkt"	  
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path notes "jig")]) ;; FIXME
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))


@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path notes "jig")))) ;; FIXME
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt" "unload-bits-asm.rkt"))

@(define this-lang "Knock")

@title[#:tag this-lang]{@|this-lang|: Libraries and Modules}

@table-of-contents[]

@section[#:tag-prefix "knock"]{Modules, Everywhere!}

With @secref{Iniquity}, we introduced function definitions and
function calls and with @secref{Jig} we even made sure that calls
behave properly and that a call in tail position operates like a
``goto with arguments.''

At this point, we have a pretty rich language: there are some nice
built-in datatypes (although lacking the ability to add user-defined
data types is a real shortcoming), we have a few useful primitives,
and we can write and use functions.

One limitation of our language so far is that programs are assumed to
be monolithic; we're given a single file containing a bunch of
function definitions and an expression to evaluate.  This of course
isn't realistic.  Real programs are composed of different parts and
these parts can be re-used.  Our compiler is a good example: we've
defined several modules for dealing with data type definitions,
parsing, compiling primitives, etc.

Let us now endow our language with the same capability.

We'll call it @bold{@this-lang}!

@section[#:tag-prefix "knock"]{What is a Module?}

A module is something we've been writing throughout this course.
Roughly it's a unit of code that may make use of other modules via
@racket[require], define some things, and provide a subset of those
things via @racket[provide].  For example, the following is a module:

@typeset-code{
#lang racket
(provide rev)

;; [Listof X] -> [Listof X]
(define (rev xs)
  (rev/a xs '()))

;; [Listof X] [Listof X] -> [Listof X]
(define (rev/a xs ys)
  (if (empty? xs)
      ys
      (rev/a (cdr xs) (cons (car xs) ys))))
}

It doesn't require any other modules, but it provides
@racket[rev].  It defines @racket[rev] in terms of a helper
function @racket[rev/a], which is not provided, and therefore won't be
visible to clients of the module.

Another module can use this module:

@typeset-code{
#lang racket
(provide main)
(require "rev.rkt")

(define (main)
  (rev (cons 1 (cons 2 (cons 3 '())))))
}

This module requires the module defined in @tt{rev.rkt}, which makes
the @racket[rev] function available within the module, and defines
a @racket[main] function that uses @racket[rev].

We might think of this as the ``main'' entry point of the program,
and can run it in Racket with the following command:

racket -t p.rkt -m

which says to require the module and call its @racket[main] function.




@section[#:tag-prefix "knock"]{A Standard Library}



But one thing that's missing is a large set of useful functions.  Sure
you can roll your own @racket[length], @racket[reverse], and
@racket[append] functions for operating on lists, but these kinds of
things usually come standard with a programming language.

One option is to add more primitives.  We could add a case to
@racket[compile-op1] to handle @racket['length] and then implement the
length operation on lists in assembly.  That sounds tedious, and
writing assembly is easy to get wrong, hard to debug, and just
generally a little unpleasant.  We're building a high-level language,
after all, so that we can write programs in way that easier to
understand and interact with.

We can just write @racket[length] in Racket.  We can debug and test it
in Racket.  We might come up with something like:

@#reader scribble/comment-reader
(racketblock
;; [Listof X] -> Natural
(define (length xs)
  (if (empty? xs)
      0
      (add1 (length (cdr xs)))))
)

Or, now knowing more about tail recursion, we might write an iterative
version:

@#reader scribble/comment-reader
(racketblock
;; [Listof X] -> Natural
(define (length xs)
  (length/a xs 0))

;; [Listof X] Natural -> Natural
(define (length/a xs n)
  (if (empty? xs)
      n
      (length (cdr xs))))
)

But here's the thing: we've sort of reached a critical tiping point
with our compiler.  Rather than write the assembly code for
@racket[length] by hand, we can write the code in Racket @emph{and
compile it!}


@#reader scribble/comment-reader
(ex
(compile-define
 (parse-define
  ;; [Listof X] -> Natural
  '(define (length xs)
     (length/a xs 0))))

(compile-define
 (parse-define
  ;; [Listof X] Natural -> Natural
  '(define (length/a xs n)
     (if (empty? xs)
         n
         (length/a (cdr xs) (add1 n))))))
)

In fact, we can declare @racket[length]'s label as a @racket[Global],
declare some other labels that will be linked in later, and print
these instructions to a file, like so:

@#reader scribble/comment-reader
(ex
(with-output-to-file "length.s"
  #:exists 'truncate
  (λ ()
   (displayln
    (asm-string
     (seq (Global (symbol->label 'length))
	  (Extern 'raise_error_align)     
          (externs)
          (compile-define
           (parse-define
            ;; [Listof X] -> Natural
            '(define (length xs)
               (length/a xs 0))))
          (compile-define
           (parse-define
	    ;; [Listof X] Natural -> Natural
            '(define (length/a xs n)
               (if (empty? xs)
                   n
                   (length/a (cdr xs) (add1 n))))))))))))

We can confirm the file now contains the instructions in NASM syntax:

@shellbox["cat length.s"]

Now we can assemble the instructions into an object file and link it
together with the runtime system to obtain a new runtime system that
includes @racket[length]:

@shellbox[(format "nasm -g -f ~a -o length.o length.s"
                  (if (eq? (system-type) 'macosx)
		      "macho64"
		      "elf64"))
          "ld -r runtime.o length.o -o runtime-plus-length.o"]

Now we can compile programs that use @racket[length] without further
work from our compiler by simply linking them against this new runtime:


@#reader scribble/comment-reader
(ex
(current-objs '("runtime-plus-length.o"))
(unload/free
  (asm-interp
    (seq (Extern (symbol->label 'length))
         (Global 'raise_error_align)
         (compile
	  (parse
	   '[(length (cons 1 (cons 2 (cons 3 '()))))]))))))

This suggests a path forward for extending the language with new
functionality:

@itemlist[

@item{implement functions within the language the compiler can handle,}

@item{compile the function definition, and link in to the runtime
system so that other programs can now make use of the new functions.}

]

As you can see from the examples above, most of the work of adding
functionality in this way concerns declaring the appropriate labels
@racket[Global] or @racket[Extern] as appropriate and then building
and linking the parts of the runtime system.

Before addressing these issues, let's instead turn to another, closely
related issue: supporting the compilation of multi-module programs.

@section[#:tag-prefix "knock"]{Modules}

