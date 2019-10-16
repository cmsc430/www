#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "hustle/semantics.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "knock" f))))))
	   '("interp.rkt" "compile.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Knock"]{Knock: first-class function (pointers)}

@table-of-contents[]

@section[#:tag-prefix "knock"]{First-class function (pointers)}

With Iniquity and Jig, we have introduced functions and function
calls, but functions are second-class language mechanisms: functions
are not values.  They cannot be computed.  They cannot be stored in a
list or box.  They cannot be passed as arguments are returned as
results of other functions.

This is too bad since so many program designs depend on the idea of
computation-as-a-value at the heart of functional and object-oriented
programming.

Let's now remedy this problem by making functions first-class values
in our language.  We'll call it @bold{Knock}.


We add to the syntax two new forms, one for reference functions and
one for calling a function where the function position is an arbitrary
expression (that should evaluate to a function):

@verbatim|{
;; type Expr =
;; | ....
;; | `(fun ,Variable)
;; | `(call ,Expr ,@(Listof Expr))
}|

These new syntactic forms are temporary forms that don't correspond
anything in Racket but make it a bit easier to present how first-class
functions work.  The @racket[(fun _f)] form is a reference to a
function @racket[_f], which is defined in the program.  The
@racket[(call _e0 _es ...)] form is a function call where the function
position, @racket[_e0] is an arbitrary expression that should produce
a function value.

We will end up eliminating them in future versions of the compiler;
they are simply a crutch for now.

@section[#:tag-prefix "knock"]{A Compiler with Function pointers}

The main idea in making functions into values is we will need to have
a representation of functions.  We will use a representation similar
to boxes: functions will be heap allocated data structures.  What will
be stored in the heap?  The address of the label of the function.

A function reference, @racket[(fun _f)], will allocate a 64-bit
segment of the heap, store the location of the function's label,
i.e. a pointer to the instructions for the function, and tag the
pointer as a ``procedure value,'' which is new, disjoint kind of
value.

@#reader scribble/comment-reader
(racketblock
;; Variable -> Asm
(define (compile-fun f)
  `(; rax <- address of label f
    (lea rax (offset ,(symbol->label f) 0))
    ; write in to heap
    (mov (offset rdi 0) rax)
    ; rax <- pointer into heap
    (mov rax rdi)
    ; tag as procedure pointer
    (or rax ,type-proc)
    ; alloc
    (add rdi 8)))
)

A function call, @racket[(call _e0 _es ...)] will evaluate on the
subexpressions.  The @racket[_e0] expression should produce a
function, i.e. tagged pointer.  We can erase the tag to compute the
address in the heap.  Dereferencing that location, gets us the label
address, which can then jump to.

@#reader scribble/comment-reader
(racketblock
;; Expr (Listof Expr) CEnv -> Asm
(define (compile-fun-call e0 es c)
  (let ((cs (compile-es es (cons #f c)))
        (c0 (compile-e e0 c))
        (i (- (add1 (length c))))
        (stack-size (* 8 (length c))))
    `(,@c0
      ; save f in stack    
      (mov (offset rsp ,i) rax)
      ,@cs
      ; restore f      
      (mov rax (offset rsp ,i))      
      ,@assert-proc
      (sub rsp ,stack-size)
      (xor rax ,type-proc)
      ; call f
      (call (offset rax 0))
      (add rsp ,stack-size))))
)

A tail call version of the above can be defined as:

@#reader scribble/comment-reader
(racketblock
;; Expr (Listof Expr) CEnv -> Asm
(define (compile-fun-tail-call e0 es c)
  (let ((cs (compile-es es (cons #f c)))
        (c0 (compile-e e0 c))
        (i (- (add1 (length c)))))
    `(,@c0
      (mov (offset rsp ,i) rax)
      ,@cs
      (mov rax (offset rsp ,i))
      ,@(move-args (length es) i)
      ,@assert-proc
      (xor rax ,type-proc)
      (jmp (offset rax 0)))))
)

The complete compiler:

@codeblock-include["knock/compile.rkt"]

We can verify that the compiler works for programs that use functions
like before:

@ex[
(asm-interp
   (compile '(begin (define (f x)
                      (if (zero? x)
		          0
			  (add1 (call (fun f) (sub1 x)))))
                    (call (fun f) 10))))
]

But it also works when functions are put in lists:

@ex[
(asm-interp
   (compile '(begin (define (f x) x)
                    (call (car (cons (fun f) '())) 7))))
]

And functions that produce functions:

@ex[
(asm-interp
   (compile '(begin (define (f x) (fun h))
                    (define (h y) y)
                    (call (call (fun f) 5) 9))))
]