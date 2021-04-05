#lang scribble/manual

@(require (for-label (except-in racket compile ...) a86))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path notes "knock"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

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
;; | (Fun Id)
;; | (Call Expr (Listof Expr))
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
;; Id -> Asm
(define (compile-fun f)
       ; Load the address of the label into rax
  (seq (Lea rax (symbol->label f))
       ; Copy the value onto the heap
       (Mov (Offset rbx 0) rax)
       ; Copy the heap address into rax
       (Mov rax rbx)
       ; Tag the value as a proc
       (Or rax type-proc)
       ; Bump the heap pointer
       (Add rbx 8)))
)

A function call, @racket[(call _e0 _es ...)] will evaluate on the
subexpressions.  The @racket[_e0] expression should produce a
function, i.e. tagged pointer.  We can erase the tag to compute the
address in the heap.  Dereferencing that location, gets us the label
address, which can then jump to.

Similar to `compile-app` from Iniquity, we have to be concerned about 16-byte
alignment for `rsp`. However, the wrinkle is that we also have the function
pointer on the stack, so we have to do the calculation with an `extended` env:
`env`:

@#reader scribble/comment-reader
(racketblock
(define (compile-fun-call e0 es c)
  (let ((d (length es))
        (env (cons #f c)))
       ; We have to computer the function pointer either way.
       (seq (compile-e e0 c)
            (assert-proc rax)
            (Push rax)

       ; Then we worry about alignment
       (if (even? (+ d (length env)))

           ; We will be 16-byte aligned
           (seq (compile-es es env)
                (Mov rax (Offset rsp (* 8 d)))
                (Xor rax type-proc)
                (Call (Offset rax 0))
                (Add rsp (* 8 (add1 d))))

           ; We won't be 16-byte aligned, and need to adjust `rsp`
           (seq (Sub rsp 8)
                (compile-es es env)
                (Mov rax (Offset rsp (* 8 (add1 d))))
                (Xor rax type-proc)
                (Call (Offset rax 0))
                ; pop arguments, padding, and function pointer
                (Add rsp (* 8 (+ 2 d))))))))

)

A tail call version of the above can be defined as:

@#reader scribble/comment-reader
(racketblock
;; Variable (Listof Expr) CEnv -> Asm
;; Compile a call in tail position
(define (compile-tail-fun-call f es c)
  (let ((cnt (length es)))
       (seq (compile-e f c)
            (assert-proc rax)
            (Push rax)
            (compile-es es (cons #f c))
            (move-args cnt (+ cnt (add1 (in-frame c))))
            (Mov rax (Offset rsp (* 8 cnt)))
            (Xor rax type-proc)
            (Add rsp (* 8 (+ cnt (add1 (in-frame c)))))
            (Jmp (Offset rax 0)))))

)

The complete compiler:

@codeblock-include["knock/compile.rkt"]

We can verify that the compiler works for programs that use functions
like before:

@ex[
(current-objs '("runtime.o"))
(asm-interp
   (compile (parse '(begin (define (f x)
                            (if (zero? x)
      		          0
      			  (add1 (call (fun f) (sub1 x)))))
                          (call (fun f) 10)))))
]

But it also works when functions are put in lists:

@ex[
(current-objs '("runtime.o"))
(asm-interp
   (compile (parse '(begin (define (f x) x)
                            (call (car (cons (fun f) '())) 7)))))
]

And functions that produce functions:

@ex[
(current-objs '("runtime.o"))
(asm-interp
   (compile (parse '(begin (define (f x) (fun h))
                            (define (h y) y)
                            (call (call (fun f) 5) 9)))))
]
