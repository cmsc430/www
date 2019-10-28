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

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "loot" f))))))
	   '("interp.rkt" "compile.rkt" "syntax.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Loot"]{Loot: lambda the ultimate}

@table-of-contents[]

@section[#:tag-prefix "loot"]{Functions in their most general form}

We've been building up the pieces of functions, first with
second-class functions, then with tail-calls, then with first-class
function pointers.  You've seen variable arity functions and the
@racket[apply] operator in @seclink["Assignment 6"]{Assignment
6}.

Now we're ready to deal with functions in their most general form:
@racket[λ]-expressions.


We add @racket[λ]-expressions to the syntax and remove the
@racket[(fun ,Variable)] and @racket[(call ,Expr ,@(Listof Expr))]
forms.  We no longer need a separate syntactic form for referencing
the name of a function, we can just use variable binding.  Likewise,
we use the same syntax as Racket for function application:

@verbatim|{
;; type Expr =
;; | ....
;; | `(λ ,Formals ,Expr)
;; | `(,Expr ,@(Listof Expr))
}|

For the moment, @tt{Formals} can be defined as a list of variables:

@verbatim|{
;; type Formals = (Listof Variable)
}|

But it's possible to extend the @racket[λ]-notation to include the
ability to define variable-arity functions, along the lines of
@secref["Assignment 6"].

@section[#:tag-prefix "loot"]{Long Live Lambda!}

Let's start by developing the interpreter for Loot, where the relevant
forms are @racket[λ]s and applications:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)  '...]
    [`(,e . ,es)        '...])
)

These two parts of the interpreter must fit together: @racket[λ] is
the constructor for functions and application is deconstructor.  An
application will evaluate all its subexpressions and the value
produced by @racket[_e] ought to be the kind of value constructed by
@racket[λ].  That value needs to include all the necessary information
to, if given the values of the arguments @racket[es], evaluate the
body of the function in an environment associating the parameter names
with the arguments' values.

So how should functions be @emph{represented}?  Here is a simple idea
following the pattern we've used frequently in the interpreter:

@itemlist[
@item{Q: How can we represent strings?}
@item{A: With strings!}

@item{Q: How can we represent booleans?}
@item{A: With booleans!}

@item{Q: How can we represent numbers?}
@item{A: With numbers!}

@item{Q: How can we represent pairs?}
@item{A: With pairs!}

@item{Q: etc.}
@item{A: etc.}
]

So now:
@itemlist[
@item{Q: How can we represent functions?}
@item{A: With functions!?}
]

Great, so we will use function to represent functions.  We can
implement function application with function application.  Let's fill
in what we know so far:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ ??? '...)]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)


It's not totally clear what parameters the representation of a
function should have or what we should in the body of that function.
However, the code in the interpretation of an application sheds light
on both.  First, it's clear a function should potentially take any
number of arguments:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ vs '...)]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)


Second, what should happen when a function is applied?  It should
produce the answer produced by the body of the @racket[λ] expression
in an environment that associates @racket[xs] with @racket[vs].
Translating that to code, we get:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ vs (interp-env e (zip xs vs)))]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)

And now we have simultaneously arrived at our representation of function values:
@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | ....
;; | (Value ... -> Answer)
)

and completed the implementation of the interpreter.

There are, however, problems.

For one, this approach does not model how @racket[λ]-expressions are
able to capture the environment in which they are evaluated.  Consider:

@racketblock[
(let ((y 8))
  (λ (x) (+ x y)))
]

This evaluates to a function that, when applied, should add 8 to its
argument.  It does so by evaluating the body of the @racket[λ], but in
an environment that both associates @racket[x] with the value of the
argument, but also associates @racket[y] with @racket[8].  That
association comes from the environment in place when the
@racket[λ]-expression is evaluated.  The interpreter as written will
consider @racket[y] is unbound!

The solution is easy: in order for (Loot) functions to capture their
(implicit) environment, we should capture the (explicit) environment
in the (Racket) function:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ (vs) (interp-env e (append (zip xs vs) r)))]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (apply f vs))])
)

The last remaining issue is we should do some type and arity-checking:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     (λ (vs)
       (if (= (length xs) (length vs))
           (interp-env e (append (zip xs vs) r))
	   'err))]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (if (procedure? f)
           (apply f vs)
	   'err))])
)


The complete interpreter is:

@codeblock-include["loot/interp.rkt"]


We now have the full power of @racket[λ] expressions in our language.
We can write recursive functions, using only anonymous functions, via
the Y-combinator:

@ex[
(interp
  '(λ (t)
     ((λ (f) (t (λ (z) ((f f) z))))
      (λ (f) (t (λ (z) ((f f) z)))))))
]

For example, computing the triangular function applied to 10:

@ex[
(interp
  '(((λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))
     (λ (tri)
       (λ (n)
         (if (zero? n)
             1
             (+ n (tri (sub1 n)))))))
    10))
]

One of the niceties of using Racket functions to represent Loot
functions is we can define Racket functions via the interpretation of
Loot functions:

@ex[
(define Y
  (interp
    '(λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))))

(define tri
  (interp '(λ (tri)
             (λ (n)
               (if (zero? n)
                   1
                   (+ n (tri (sub1 n))))))))
]

And then use them from within Racket:

@ex[
((Y tri) 10)
]

We can also ``import'' Racket functions in to Loot:

@ex[
(interp-env '(expt 2 10)
            `((expt ,expt)))
]


@section[#:tag-prefix "loot"]{Lambda is Dead!}

Now the question you might naturally wonder is: how does implementing
functions in terms of functions help me implement functions in x86,
which after all, doesn't have @racket[λ]?

The answer is that from this point, in which we have an understandable
account of functions, we can iteratively revise the interpreter to
eliminate the use of functions while computing equivalent results.
Doing so will shed light on the lower-level implementation of
functions in the compiler.

Consider again what it is that a @racket[λ]-expression is doing for
you:

@itemlist[

@item{it is packaging up the parameters, body, and environment, so
that}

@item{when applied it can evaluate the body, binding the parameters,
in an extension of the functions environment.}

]

We can achive these things without using a function value by:

@itemlist[
@item{creating a data structure to hold the parameters, body, and environment, and}
@item{rewriting the application of the function to use those values to evaluate
the body, binding the parameters, etc.}
]

So we are changing the representation of functions from:

And now we have simultaneously arrived at our representation of function values:
@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | ....
;; | (Value ... -> Answer)
)

To:

@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | ....
;; | `(closure ,Formals ,Expr ,Env)
)

When a @racket[λ] is evaluated, a closure is created.  When a function
is applied, we deconstruct the closure and execute the code that used
to be in the (Racket) function:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv -> Answer
(define (interp-env e r)
    ;;...
    [`(λ (,xs ...) ,e)
     `(closure ,xs ,e ,r)]
    [`(,e . ,es)
     (let ((f (interp-eval e r))
           (vs (interp-eval* es r)))
       (match f
         [`(closure ,xs ,e ,r)
	  (if (= (length vs) (length xs))
              (interp-env e (append (zip xs vs) r))
	      'err)]
	 [_ 'err]))])
)

We can give it a try:


@(ev `(require (file ,(path->string (build-path notes "loot" "interp-defun.rkt")))))

@ex[
(interp '(λ (x) x))
(interp '((λ (x) (λ (y) x)) 8))
]

Notice in the second example how the closure contains the body of the
function and the environment mapping the free variable @racket['x] to
8.

We can also confirm our larger example works:

@ex[
(interp
  '(((λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))
     (λ (tri)
       (λ (n)
         (if (zero? n)
             1
             (+ n (tri (sub1 n)))))))
    10))
]

While can't apply the interpretation of functions in Racket
like we did previously, we can @racket[apply-function] the
interpretation of functions:

@ex[
(define Y
  (interp
    '(λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))))

(define tri
  (interp '(λ (tri)
             (λ (n)
               (if (zero? n)
                   1
                   (+ n (tri (sub1 n))))))))

(apply-function (apply-function Y tri) 10)
]

The process we used to eliminate function values from the interpreter
is an instance of a general-purpose whole-program transformation
called @bold{defunctionalization} for replacing function values with
data structures.

@section[#:tag-prefix "loot"]{Defunctionalization at work}

Let's digress for a moment and learn this very useful transformation.

Here is a data type for representing regular expressions:

@#reader scribble/comment-reader
(racketblock
;; type Regexp =
;; | 'zero
;; | 'one
;; | `(char ,Char)
;; | `(times ,Regexp ,Regexp)
;; | `(plus ,Regexp ,Regexp)
;; | `(star ,Regexp)
)

The regular expression @racket['zero] matches nothing; @racket['one]
matches the empty string; @racket[`(char ,_c)] matches the character
@racket[_c]; @racket[`(times ,_r1 ,_r2)] matches the concatenation of
a string matching @racket[_r1] followed by a string matching
@racket[_r2]; @racket[`(plus ,_r1 ,_r2)] matching either a string
matching @racket[_r1] or a string matching @racket[_r2]; and
@racket[`(star ,_r)] matches a string made up of any number of
substrings, each of which match @racket[_r].

A really nice way to write a matcher is to use a continuation-passing
style that keeps track of what is required of the remainder of the
string after matching a prefix against the regexp:

@codeblock-include["loot/regexp.rkt"]

@(ev `(require (file ,(path->string (build-path notes "loot" "regexp.rkt")))))

Let's give it a try:
@ex[
(accepts `(star (char #\a)) "aaaaa")
(accepts `(star (char #\a)) "aaaab")
(accepts `(star (plus (char #\a) (char #\b))) "aaaab")
]
	
But what if needed to program this regular expression matching without
the use of function values?  We can arrive at such code systematically
by applying defunctionalization.

@codeblock-include["loot/regexp-defun.rkt"]

@(ev `(require (file ,(path->string (build-path notes "loot" "regexp-defun.rkt")))))


And we get the same results:

@ex[
(accepts `(star (char #\a)) "aaaaa")
(accepts `(star (char #\a)) "aaaab")
(accepts `(star (plus (char #\a) (char #\b))) "aaaab")
]


@section[#:tag-prefix "loot"]{Compiling Loot}

Compiling a @racket[λ]-expression will involve generating two
different chunks of instructions:

@itemlist[

@item{one to implement the function, i.e. the code to be executed when
the function created by the @racket[λ]-expression is called, and}

@item{one to create a closure, i.e. to capture the environment at the
point the @racket[λ]-expression is evaluated.}

]

@section[#:tag-prefix "loot"]{Compiling Function Definitions}

The first part closely follows the appoach of defining a function
definition @racket[(define (_f _x ...) _e)] from our previous compilers.

Ther are two important differences from the past though:

@itemlist[

@item{@racket[λ]-expressions don't have a name, and}

@item{the body of the @racket[λ]-expression may reference variables
bound outside of the @racket[λ]-expression.}

]

To deal with the first issue, we first make a pass over the program
inserting computed names for each @racket[λ]-expression.

To accomodate this, we will introduce the following data type for
``labelled'' expressions:

@#reader scribble/comment-reader
(racketblock
;; type LExpr =
;; ....
;; | `(λ ,Formals ',Symbol ,Expr)
)

An @tt{LExpr} is just like a @tt{Expr} except that
@racket[λ]-expressions have the form like @racket[(λ (x) 'fred (+ x
x))].  The symbol @racket['fred] here is used to give a name to the
@racket[λ]-expression.  The use of @racket[quote] is so that
@tt{LExprs} are still a valid subset of Racket expressions.

The first step of the compiler will be to label every
@racket[λ]-expression using the following function:

@#reader scribble/comment-reader
(racketblock
;; Expr -> LExpr
(define (label-λ e)
  (match e
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`(box ,e0)            `(box ,(label-λ e0))]
    [`(unbox ,e0)          `(unbox ,(label-λ e0))]
    [`(cons ,e0 ,e1)       `(cons ,(label-λ e0) ,(label-λ e1))]
    [`(car ,e0)            `(car ,(label-λ e0))]
    [`(cdr ,e0)            `(cdr ,(label-λ e0))]
    [`(add1 ,e0)           `(add1 ,(label-λ e0))]
    [`(sub1 ,e0)           `(sub1 ,(label-λ e0))]
    [`(zero? ,e0)          `(zero? ,(label-λ e0))]
    [`(empty? ,e0)         `(empty? ,(label-λ e0))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(label-λ e0) ,(label-λ e1) ,(label-λ e2))]
    [`(+ ,e0 ,e1)          `(+ ,(label-λ e0) ,(label-λ e1))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(label-λ e0))) ,(label-λ e1))]
    [`(λ ,xs ,e0)          `(λ ,xs ',(gensym) ,(label-λ e0))]
    [`(,e . ,es)           `(,(label-λ e) ,@(map label-λ es))]))
)

Here it is at work:

@ex[
(label-λ
  '(λ (t)
    ((λ (f) (t (λ (z) ((f f) z))))
     (λ (f) (t (λ (z) ((f f) z)))))))
]

Now turning to the second issue--@racket[λ]-expression may reference
variables bound outside of the expression---let's consider how to
compile something like @racket[(λ (x) z)]?

There are many possible solutions, but perhaps the simplest is to
compile this as a function that takes @emph{two} arguments,
i.e. compile it as if it were: @racket[(λ (x z) z)].  The idea is that
a @racket[λ]-expression defines a function of both explicit arguments
(the parameters) and implicit arguments (the free variables of the
@racket[λ]-expression).

This will have to work in concert with closure creation and function
calls.  When the @racket[λ]-expression is evaluated, a closure will be
created storing the value of @racket[z].  When the function is
applied, the caller will need to retrieve that value and place it as
the second argument on stack before calling the function's code.

To implement this, we will need to compute the free variables, which
we do with the following function:

@#reader scribble/comment-reader
(racketblock
;; LExpr -> (Listof Variable)
(define (fvs e)
  (define (fvs e)
    (match e
      [(? symbol? x)         (list x)]
      [(? imm? i)            '()]
      [`(box ,e0)            (fvs e0)]
      [`(unbox ,e0)          (fvs e0)]
      [`(cons ,e0 ,e1)       (append (fvs e0) (fvs e1))]
      [`(car ,e0)            (fvs e0)]
      [`(cdr ,e0)            (fvs e0)]
      [`(add1 ,e0)           (fvs e0)]
      [`(sub1 ,e0)           (fvs e0)]
      [`(zero? ,e0)          (fvs e0)]
      [`(empty? ,e0)         (fvs e0)]
      [`(if ,e0 ,e1 ,e2)     (append (fvs e0) (fvs e1) (fvs e2))]
      [`(+ ,e0 ,e1)          (append (fvs e0) (fvs e1))]
      [`(let ((,x ,e0)) ,e1) (append (fvs e0) (remq* (list x) (fvs e1)))]
      [`(λ ,xs ,l ,e0)       (remq* xs (fvs e0))]
      [`(,e . ,es)           (append (fvs e) (apply append (map fvs es)))]))          
  (remove-duplicates (fvs e)))
)

We can now write the function that compiles a labelled
@racket[λ]-expression into a function in assembly:

@#reader scribble/comment-reader
(racketblock
;; Lambda -> Asm
(define (compile-λ-definition l)
  (match l
    [`(λ ,xs ',f ,e0)
     (let ((c0 (compile-tail-e e0 (reverse (append xs (fvs l))))))
       `(,f
         ,@c0
         ret))]))
)

Here's what's emitted for a @racket[λ]-expression with a free variable:
@ex[
(compile-λ-definition '(λ (x) 'f z))
]

Notice that it's identical to a @racket[λ]-expression with an added
parameter and no free variables:
@ex[
(compile-λ-definition '(λ (x z) 'f z))
]

The compiler will need to generate one such function for each
@racket[λ]-expression in the program.  So we use a helper function for
extracting all the @racket[λ]-expressions and another for compiling
each of them:


@#reader scribble/comment-reader
(racketblock
;; LExpr -> (Listof LExpr)
;; Extract all the lambda expressions
(define (λs e)
  (match e
    [(? symbol? x)         '()]
    [(? imm? i)            '()]
    [`(box ,e0)            (λs e0)]
    [`(unbox ,e0)          (λs e0)]
    [`(cons ,e0 ,e1)       (append (λs e0) (λs e1))]
    [`(car ,e0)            (λs e0)]
    [`(cdr ,e0)            (λs e0)]
    [`(add1 ,e0)           (λs e0)]
    [`(sub1 ,e0)           (λs e0)]
    [`(zero? ,e0)          (λs e0)]
    [`(empty? ,e0)         (λs e0)]
    [`(if ,e0 ,e1 ,e2)     (append (λs e0) (λs e1) (λs e2))]
    [`(+ ,e0 ,e1)          (append (λs e0) (λs e1))]
    [`(let ((,x ,e0)) ,e1) (append (λs e0) (λs e1))]
    [`(λ ,xs ,l ,e0)       (cons e (λs e0))]
    [`(,e . ,es)           (append (λs e) (apply append (map λs es)))]))

;; (Listof Lambda) -> Asm
(define (compile-λ-definitions ls)
  (apply append (map compile-λ-definition ls)))
)


The top-level @racket[compile] function now labels inserts labels and
compiles all the @racket[λ]-expressions to functions:

@#reader scribble/comment-reader
(racketblock
;; Expr -> Asm
(define (compile e)
  (let ((le (label-λ e)))
    `(entry
      ,@(compile-tail-e le '())
      ret
      ,@(compile-λ-definitions (λs le))
      err
      (push rbp)
      (call error)
      ret)))
)

What remains is the issue of compiling @racket[λ]-expressions to code
to create a closure.

@section[#:tag-prefix "loot"]{Save the Environment: Create a Closure!}

We've already seen how to create a reference to a function pointer,
enabling functions to be first-class values that can be passed around,
returned from other functions, stored in data structures, etc.  The
basic idea was to allocate a location in memory and save the address
of a function label there.

A closure is just this, plus the environment that needs to be restored
with the function is called.  So representing a closure is fairly
straightforward: we will allocate a location in memory and save the
function label, plus each value that is needed from the environment.
In order to keep track of how many values there are, we'll also store
the length of the environment.

Here's the function for emitting closure construction code:

@#reader scribble/comment-reader
(racketblock
;; (Listof Variable) Label Expr CEnv -> Asm
(define (compile-λ xs f e0 c)
  `(;; Save label address
    (lea rax (offset ,f 0))
    (mov (offset rdi 0) rax)
    
    ;; Save the environment
    (mov r8 ,(length ys))
    (mov (offset rdi 1) r8)
    (mov r9 rdi)
    (add r9 16)
    ,@(copy-env-to-heap ys c 0)
      
    ;; Return a pointer to the closure
    (mov rax rdi)
    (or rax ,type-proc)
    (add rdi ,(* 8 (+ 2 (length ys))))))
)

Compared the previous code we say for function pointer references, the
only difference is the code to store the length and value of the free
variables of the @racket[λ]-expression.  Also: the amount of memory
allocated is no longer just a single cell, but depends on the number
of free variables being closed over.

The @racket[copy-env-to-heap] function generates instructions for
dereferencing variables and copying them to the appropriate memory
location where the closure is stored:

@#reader scribble/comment-reader
(racketblock
;; (Listof Variable) CEnv Natural -> Asm
;; Pointer to beginning of environment in r9
(define (copy-env-to-heap fvs c i)
  (match fvs
    ['() '()]
    [(cons x fvs)
     `((mov r8 (offset rsp ,(- (add1 (lookup x c)))))
       (mov (offset r9 ,i) r8)
       ,@(copy-env-to-heap fvs c (add1 i)))]))
)

That's all there is to closure construction!

@section[#:tag-prefix "loot"]{Calling Functions}

The last final peice of the puzzle is making function calls and
closures work together.  Remember that a @racket[λ]-expression is
compiled into a function that expects two sets of arguments on the
stack: the first are the explicit arguments that given at the call
site; the other arguments are the implicit arguments corresponding to
free variables the @racket[λ]-expression being called.  The value of
these arguments are given by the environment saved in the closure of
the @racket[λ]-expressions.

So the code generated for a function call needs to manage running each
subexpression, the first of which should evaluate to a function (a
pointer to a closure).  The arguments are saved on the stack, and then
the values stored in the environment part of the closure need to be
copied from the heap to the stack:

@#reader scribble/comment-reader
(racketblock
;; LExpr (Listof LExpr) CEnv -> Asm
(define (compile-call e0 es c)
  (let ((cs (compile-es es (cons #f c)))
        (c0 (compile-e e0 c))
        (i (- (add1 (length c))))
        (stack-size (* 8 (length c))))
    `(,@c0
      (mov (offset rsp ,i) rax)
      ,@cs
      (mov rax (offset rsp ,i))
      ,@assert-proc
      (xor rax ,type-proc)
      (sub rsp ,stack-size)      
      ,@(copy-closure-env-to-stack (add1 (length es)))
      (call (offset rax 0))      
      (add rsp ,stack-size))))
)

The only new bit is the use of @racket[copy-closure-env-to-stack].
Unlike the closure construction code, in which we statically know what
and how many variables to save in a closure, we must dynamically
loop over the environment to move values to the stack:

@#reader scribble/comment-reader
(racketblock
;; Natural -> Asm
;; Copy closure's (in rax) env to stack skipping n spots
(define (copy-closure-env-to-stack n)
  (let ((copy-loop (gensym 'copy_closure))
        (copy-done (gensym 'copy_done)))
    `((mov r8 (offset rax 1)) ; length
      (mov r9 rax)
      (add r9 16)             ; start of env
      (mov rcx rsp)           ; start of stack
      (add rcx ,(- (* 8 (add1 n))))
      ,copy-loop
      (cmp r8 0)
      (je ,copy-done)                  
      (mov rbx (offset r9 0))       
      (mov (offset rcx 0) rbx)
      (sub r8 1)
      (add r9 8)
      (sub rcx 8)      
      (jmp ,copy-loop)
      ,copy-done)))
)

Let's try it out:

@ex[
(asm-interp (compile '((let ((x 8)) (λ (y) x)) 2)))
(asm-interp (compile '(((λ (x) (λ (y) x)) 8) 2)))
(asm-interp (compile '((λ (f) (f (f 0))) (λ (x) (add1 x)))))
]

@section[#:tag-prefix "loot"]{Recursive Functions}

Writing recursive programs with the Y-combinator is a bit
inconvenient.  Let us now add a recursive function binding construct:
@racket[letrec].

A @racket[letrec]-expression has a shape like a
@racket[let]-expression, but variables are bound in both the body
@emph{and} the right-hand-side of the @racket[letrec].  To keep
matters simple, we will assume the right-hand-sides of a
@racket[letrec] are all @racket[λ]-expressions.  (Racket eases this
restriction, but it significantly complicates compilation.)

So for example, writing the @racket[even?] and @racket[odd?] functions
using @racket[letrec] looks like:

@ex[
(letrec ((even?
          (λ (x)
            (if (zero? x)
                #t
                (odd? (sub1 x)))))
         (odd?
          (λ (x)
            (if (zero? x)
                #f
                (even? (sub1 x))))))
  (even? 10))
]


To compile a @racket[letrec]-expression, we can compile the
@racket[λ]-expression as functions just as before.  Notice that the
recursive (or mutually recursive) occurrence will be considered a free
variable within the @racket[λ]-expression, so just like any other free
variable, the closure creation should capture the value of this
binding.

We need to extend the syntax functions for computing free variables,
extracting @racket[λ]-expressions, and so on.  All of this is
straightforward.

The key complication to compiling a @racket[letrec]-expression is that
the name of a function should be bound---to itself---within the body
of the function.  The key insight into achieving this is to first
allocate closures, but to delay the actual population of the closures'
environments.

The way that compiling a @racket[letrec]-expression works is roughly:

@itemlist[

@item{allocate a closure for each of the right-hand-side
@racket[λ]-expressions, but do not copy the (relevant parts of the)
environment in to closures (yet),}

@item{push each of these closures on to the stack (effectively binding
the left-hand-sides to the unitialized closures),}

@item{now that the names are bound, we can populate the closures, and
references to any of the @racket[letrec]-bound variables will be
captured correctly,}

@item{then compile the body in an environment that includes all of the
@racket[letrec]-bound variables.}

]

The @racket[compile-letrec] function takes a list of variables to
bind, the right-hand-side @racket[λ]-expressions, body, and
compile-time environment.  It relies on three helper functions to
handle the tasks listed above:

@#reader scribble/comment-reader
(racketblock
;; (Listof Variable) (Listof Lambda) Expr CEnv -> Asm
(define (compile-letrec fs ls e c)  
  (let ((c0 (compile-letrec-λs ls c))
        (c1 (compile-letrec-init fs ls (append fs c)))
        (c2 (compile-e e (append fs c))))
    `(,@c0
      ,@c1
      ,@c2)))
)

The first two tasks are taken care of by @racket[compile-letrec-λs],
which allocates unitialized closures and pushes each on the stack.

@#reader scribble/comment-reader
(racketblock
;; (Listof Lambda) CEnv -> Asm
;; Create a bunch of uninitialized closures and push them on the stack
(define (compile-letrec-λs ls c)
  (match ls
    ['() '()]
    [(cons l ls)
     (let ((cs (compile-letrec-λs ls (cons #f c)))
           (ys (fvs l)))
       `((lea rax (offset ,(second (third l)) 0))
         (mov (offset rdi 0) rax)
         (mov rax ,(length ys))
         (mov (offset rdi 1) rax)
         (mov rax rdi)
         (or rax ,type-proc)
         (add rdi ,(* 8 (+ 2 (length ys))))
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@cs))]))
)

The @racket[compile-letrec-init] goes through each function and
initializes its closure now that all of the function pointers are
available.  Finally the body is compiled in an extended environment.

@#reader scribble/comment-reader
(racketblock
;; (Listof Variable) (Listof Lambda) CEnv -> Asm
;; Initialize closures bound to each variable in fs
(define (compile-letrec-init fs ls c)
  (match fs
    ['() '()]
    [(cons f fs)
     (let ((ys (fvs (first ls)))
           (cs (compile-letrec-init fs (rest ls) c)))
       `((mov r9 (offset rsp ,(- (add1 (lookup f c)))))
         (xor r9 ,type-proc)
         (add r9 16) ; move past label and length
         ,@(copy-env-to-heap ys c 0)
         ,@cs))]))
)

We can give a spin:

@ex[
(asm-interp (compile '(letrec ((even?
                                (λ (x)
                                  (if (zero? x)
                                      #t
                                      (odd? (sub1 x)))))
                               (odd?
                                (λ (x)
                                  (if (zero? x)
                                      #f
                                      (even? (sub1 x))))))
                        (even? 10))))
]


@section[#:tag-prefix "loot"]{Syntactic sugar for function definitions}

The @racket[letrec] form is a generlization of the
@racket[(begin (define (_f _x ...) _e) ... _e0)] form we started with
when we first started looking at adding functions to the language.  To
fully subsume the language of @seclink["Iniquity"]{Iniquity}, we can
add this form back in to the language as syntactic sugar for
@racket[letrec], i.e. we can eliminate this form from programs by
rewriting them.

Let @tt{Expr+} refer to programs containing @racket[(begin (define (_f
_x ...) _e) ... _e0)].  The @racket[desugar] function writes
@tt{Expr+}s into @tt{Expr}s.

@#reader scribble/comment-reader
(racketblock
;; Expr+ -> Expr
(define (desugar e+)
  (match e+
    [`(begin ,@(list `(define (,fs . ,xss) ,es) ...) ,e)
     `(letrec ,(map (λ (f xs e) `(,f (λ ,xs ,(desugar e)))) fs xss es)
        ,(desugar e))]
    [(? symbol? x)         x]
    [(? imm? i)            i]
    [`(box ,e0)            `(box ,(desugar e0))]
    [`(unbox ,e0)          `(unbox ,(desugar e0))]
    [`(cons ,e0 ,e1)       `(cons ,(desugar e0) ,(desugar e1))]
    [`(car ,e0)            `(car ,(desugar e0))]
    [`(cdr ,e0)            `(cdr ,(desugar e0))]
    [`(add1 ,e0)           `(add1 ,(desugar e0))]
    [`(sub1 ,e0)           `(sub1 ,(desugar e0))]
    [`(zero? ,e0)          `(zero? ,(desugar e0))]
    [`(empty? ,e0)         `(empty? ,(desugar e0))]
    [`(if ,e0 ,e1 ,e2)     `(if ,(desugar e0) ,(desugar e1) ,(desugar e2))]
    [`(+ ,e0 ,e1)          `(+ ,(desugar e0) ,(desugar e1))]
    [`(let ((,x ,e0)) ,e1) `(let ((,x ,(desugar e0))) ,(desugar e1))]
    [`(letrec ,bs ,e0)
     `(letrec ,(map (λ (b) (list (first b) (desugar (second b)))) bs)
        ,(desugar e0))]
    [`(λ ,xs ,e0)          `(λ ,xs ,(desugar e0))]    
    [`(,e . ,es)           `(,(desugar e) ,@(map desugar es))]))    
)

The compiler now just desugars before labeling and compiling expressions.

And here's the complete compiler, including tail calls, @racket[letrec], etc.:

@codeblock-include["loot/compile.rkt"]

