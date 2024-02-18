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
@(ev `(current-directory ,(path->string (build-path notes "loot"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(void (ev '(current-objs '("runtime.o"))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define this-lang "Loot")

@title[#:tag this-lang]{@|this-lang|: lambda the ultimate}

@src-code[this-lang]

@table-of-contents[]

@section[#:tag-prefix "loot"]{Functions in their most general form}

We've added function calls and function definitions, but what we don't
have and really should is function @bold{values}.

Programming with functions as values is a powerful idiom that is at
the heart of both functional programming and object-oriented
programming, which both center around the idea that computation itself
can be packaged up in a suspended form as a value and later run.

Now we're ready to deal with functions in their most general form:
@racket[λ]-expressions.


Let's call it @bold{Loot}.

We add @racket[λ]-expressions to the syntax of expressions:

@racketblock[
(λ (_x0 ...) _e0)
]

Here @racket[_x0 ...] are the formal parameters of the function and
@racket[_e0] is the body.

The syntax is evocative of function definitions:

@racketblock[
(define (_f _x0 ...) _e0)
]

However, you'll notice:

@itemlist[

@item{There is no function name in the @racket[λ]-expression; it is an
@bold{anonymous} function.}

@item{The new form is an expression---it can appear any where as a
subexpression in a program, whereas definitions were restricted to be
at the top-level.}

]

There also is a syntactic relaxation on the grammar of application
expressions (a.k.a. function calls).  Previously, a function call
consisted of a function name and some number of arguments:

@racketblock[
(_f _e0 ...)
]

But since functions will now be considered values, we can generalize
what's allowed in the function position of the syntax for calls to be
an arbitrary expression.  That expression is expected to produce a
function value (and this expectation gives rise to a new kind of
run-time error when violated: applying a non-function to arguments),
which can called with the value of the arguments.

Hence the syntax is extended to:

@racketblock[
(_e _e0 ...)
]

In particular, the function expression can be a @racket[λ]-expression,
e.g.:

@racketblock[
((λ (x) (+ x x)) 10)
]

But also it may be expression which produces a function, but isn't
itself a @racket[λ]-expression:


@racketblock[
(define (adder n)
  (λ (x)
    (+ x n)))
((adder 5) 10)
]

Here, @racket[(adder 5)] is the function position of @racket[((adder
5) 10)].  That subexpression is itself a function call expression,
calling @racket[adder] with the argument @racket[5].  The result of
that subexpression is a function that, when applied, adds @racket[5]
to its argument.

In terms of the AST, here's how we model the extended syntax:

@filebox-include-fake[codeblock "loot/ast.rkt"]{
#lang racket
;; type Expr = ...
;;           | (App Expr (Listof Expr))
;;           | (Lam (Listof Id) Expr)
}

So for example, the expression @racket[((adder 5) 10)] would be parsed
as:

@racketblock[
(App (App (Var 'adder) (Int 5)) (Int 10))
]

and @racket[(λ (x) (+ x n))] would be parsed as:

@racketblock[
(Lam (list 'x) (Prim2 '+ (Var 'x) (Var 'n)))
]

We will actually use a slight tweak of this AST when it comes to
representing the syntax of @racket[λ]-expressions.  Although functions
are anynomous, it will nonetheless be useful to syntactically
distinguish one @racket[λ]-expression @emph{occurrence} from an
otherwise identical occurrence.

Consider for example:

@racketblock[
(let ((g1 (let ((x 100)) (λ (y) (+ x y))))
      (g2 (let ((x   9)) (λ (y) (+ x y)))))
  ...)      
]

This program has two occurrences of the expression @racket[(λ (y) (+ x
y))].  Even though these expressions are identical and both evaluate
to functions, they @emph{do not} evaluate to the same function!  One
is the ``add 100'' function and the other is the ``add 9'' function.

It will be useful to distinguish these two occurrences so we can talk
about @emph{this} or @emph{that} @racket[λ]-expression.

The way we accomplish this is we will assume the AST representation of
each distinct occurrence of a @racket[λ]-expression has it's own
unique name (represented with a symbol).  We choose to have the parser
take of labelling @racket[λ]-expressions by inserting a
@racket[gensym]'d symbol.  So, we touch-up the @racket[Lam] AST type
definition as follows:

@#reader scribble/comment-reader
(racketblock
;; type Expr = ...
;;           | (Lam Id (Listof Id) Expr)
)

and these two occurrence would be distinguished by having distinct
symbols for the label of the expression:

@ex[
(Lam (gensym) (list 'x) (Prim2 '+ (Var 'x) (Var 'y)))
(Lam (gensym) (list 'x) (Prim2 '+ (Var 'x) (Var 'y)))
]

@section[#:tag-prefix "loot"]{Long Live Lambda!}

Let's start by developing the interpreter for Loot, where the relevant
forms are @racket[λ]s and applications:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv Defns -> Answer
(define (interp-env e r ds)
  (match e
    ;; ...
    [(Lam _ xs e)  '...]
    [(App e es)    '...]))
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
;; Expr REnv Defns -> Answer
(define (interp-env e r ds)
  (match e
    ;; ...
    [(Lam _ xs e)
     (λ ??? '...)]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (apply f vs)])])]))
)


It's not totally clear what parameters the representation of a
function should have or what we should in the body of that function.
However, the code in the interpretation of an application sheds light
on both.  First, it's clear a function should potentially take any
number of arguments:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv Defns -> Answer
(define (interp-env e r ds)
  (match e
    ;; ...
    [(Lam _ xs e)
     (λ vs '...)]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (apply f vs)])])]))
)

Second, what should happen when a function is applied?  It should
produce the answer produced by the body of the @racket[λ] expression
in an environment that associates @racket[xs] with @racket[vs].
Translating that to code, we get:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv Defns -> Answer
(define (interp-env e r ds)
  (match e
    ;; ...
    [(Lam _ xs e)
     (λ vs (interp-env e (zip xs vs) ds))]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (apply f vs)])])]))
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
;; Expr REnv Defns -> Answer
(define (interp-env e r ds)
  (match e
    ;; ...
    [(Lam _ xs e)
     (λ vs (interp-env e (append (zip xs vs) r)) ds)]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (apply f vs)])])]))
)

The last remaining issue is we should do some type and arity-checking:

@#reader scribble/comment-reader
(racketblock
;; Expr REnv Defns -> Answer
(define (interp-env e r ds)
  (match e
    ;; ...
    [(Lam _ xs e)
     (λ vs
       ; check arity matches
       (if (= (length xs) (length vs))           
           (interp-env e (append (zip xs vs) r) ds)
           'err))]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (if (procedure? f)
               (apply f vs)
               'err)])])]))
)

We have a final issue to deal with.  What should we do about
references to functions defined at the top-level of the program?  In
other words, how do we make function applicaton when the function was
defined with @racket[define]?

One possible answer to re-use our new power of
@racket[lambda]-expression by considering @racket[define]-bound names
as just regular old variables, but changing the way that variables are
interpreted so that when evaluating a variable that is not bound in
the local environment, we consult the program definitions and
construct the function value at that moment.

There will turn out to be a better, more uniform approach, but this we
will work for now and is simple.

So for now we interpret variables as follows:

@#reader scribble/comment-reader
(racketblock
;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f xs e) (interp-env (Lam f xs e) '() ds)]
            [#f 'err])]
    [v v]))
)

You'll notice that the function is constructed by interpreting a
@racket[lambda]-expression corresponding to the function definition
and that this happens in an empty environment; that's because function
definitions can only occur at the top-level and therefore the only
variables they can reference are other @racket[define]-bound
functions, given in @racket[ds].


The complete interpreter is:

@codeblock-include["loot/interp.rkt"]

We now have the full power of @racket[λ] expressions in our language.
We can write recursive functions, using only anonymous functions, via
the Y-combinator:



@ex[
(define (run . p) (interp (apply parse p)))

(run
 '(λ (t)
    ((λ (f) (t (λ (z) ((f f) z))))
     (λ (f) (t (λ (z) ((f f) z)))))))
]

For example, computing the triangular function applied to 10:

@ex[
(run
 '(((λ (t)
      ((λ (f) (t (λ (z) ((f f) z))))
       (λ (f) (t (λ (z) ((f f) z))))))
    (λ (tri)
      (λ (n)
        (if (zero? n)
            0
            (+ n (tri (sub1 n)))))))
    36))
]

One of the niceties of using Racket functions to represent Loot
functions is we can define Racket functions via the interpretation of
Loot functions:

@ex[
(define Y
  (run
   '(λ (t)
      ((λ (f) (t (λ (z) ((f f) z))))
       (λ (f) (t (λ (z) ((f f) z))))))))

(define tri
  (run
   '(λ (tri)
      (λ (n)
        (if (zero? n)
            0
            (+ n (tri (sub1 n))))))))
]

And then use them from within Racket:

@ex[
((Y tri) 36)
]


We can also ``import'' Racket functions in to Loot:


@ex[
(interp-env (parse-e '(expt 2 10))
            (list (list 'expt expt))
	    '())
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
;; | (Closure [Listof Id] Expr Env)
)

When a @racket[λ] is evaluated, a closure is created.  When a function
is applied, we deconstruct the closure and execute the code that used
to be in the (Racket) function:


@#reader scribble/comment-reader
(racketblock
;; Expr REnv Defns -> Answer
(define (interp-env e r ds)
  (match e
    ;;...
    [(Lam _ xs e)
     (Closure xs e r)]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (match f
  	     [(Closure xs e r)        
              ; check arity matches
              (if (= (length xs) (length vs))           
                  (interp-env e (append (zip xs vs) r) ds)
                  'err)]
             [_ 'err])])])]))
)

We can give it a try:


@(ev `(require (file ,(path->string (build-path notes "loot" "interp-defun.rkt")))))

@ex[
(define (run . p) (interp (parse p)))

(run '(λ (x) x))
(run '((λ (x) (λ (y) x)) 8))
]

Notice in the second example how the closure contains the body of the
function and the environment mapping the free variable: @racket['x] to
8.

We can also confirm our larger example works:

@ex[
(run
  '(((λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))
     (λ (tri)
       (λ (n)
         (if (zero? n)
             0
             (+ n (tri (sub1 n)))))))
    36))
]

While can't apply the interpretation of functions in Racket
like we did previously, we can @racket[apply-function] the
interpretation of functions:

@#reader scribble/comment-reader
(ex
(define Y
  (run
    '(λ (t)
       ((λ (f) (t (λ (z) ((f f) z))))
        (λ (f) (t (λ (z) ((f f) z))))))))

(define tri
  (run
    '(λ (tri)
       (λ (n)
         (if (zero? n)
             0
             (+ n (tri (sub1 n))))))))

;; Value Value ... -> Answer
(define (apply-function f . vs)
  (match f
    [(Closure xs e r)        
     ; check arity matches
     (if (= (length xs) (length vs))           
         (interp-env e (append (zip xs vs) r) '())
         'err)]
    [_ 'err]))

(apply-function (apply-function Y tri) 36)
)


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

This is the reason for the generated name field in the @racket[Lam] constructor.

@#reader scribble/comment-reader
(racketblock
;; type Expr =
;; ....
;; | (Lam Id [Listof Id] Expr)
)

These labels are inserted by the parser.   Here it is at work:

@ex[
(parse-e
  '(λ (t)
    ((λ (f) (t (λ (z) ((f f) z))))
     (λ (f) (t (λ (z) ((f f) z)))))))
]

Now turning to the second issue--@racket[λ]-expression may reference
variables bound outside of the expression---let's consider how to
compile something like @racket[(λ (x) z)]?

There are many possible solutions, but here is one.  Every function
can be passed an implicit first argument which will point to a section
of memory that contains all of the values for the free variables.

In other words, the code for functions will accept an additional
argument that plays the role of the environment for this particular
instance of the function.

The first thing the function does once called is copies these values
from memory to the stack and then executes the body of the function in
an environment that binds both the free variables and the formal
parameters.

This will have to work in concert with closure creation and function
calls.  When the @racket[λ]-expression is evaluated, a closure will be
created storing the value of @racket[z] in memory.  When the function
is applied, the caller will need to retrieve that value and place it
as the first argument on stack before calling the function's code.

To implement this, we will need to compute the free variables, which
we do with the following function:

@codeblock-include["loot/fv.rkt"]

We can now write the function that compiles a labelled
@racket[λ]-expression into a function in assembly:

@#reader scribble/comment-reader
(racketblock
;; Lam -> Asm
(define (compile-lambda-define l)
  (let ((fvs (fv l)))
    (match l
      [(Lam f xs e)
       (let ((env  (append (reverse fvs) (reverse xs) (list #f))))
         (seq (Label (symbol->label f))              
              (Mov rax (Offset rsp (* 8 (length xs))))
              (Xor rax type-proc)
              (copy-env-to-stack fvs 8)
              (compile-e e env #t)
              (Add rsp (* 8 (length env))) ; pop env
              (Ret)))])))
)

Notice how similar it is to our previous function definition compiler:

@#reader scribble/comment-reader
(racketblock
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs) #t)
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))
)

The key difference here is that we are expecting the caller to leave
the closure at the top of the stack.  When called, the function
fetches the closure and copies its environment to the stack, hence the
body of the function has a static environment which includes the free
variables followed by the parameters followed by the closure.

The copying of the values from the closure environment to the stack is
achieved by this helper function:

@#reader scribble/comment-reader
(racketblock
;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov r9 (Offset rax off))
          (Push r9)
          (copy-env-to-stack fvs (+ 8 off)))]))
)

When the body of the function completes, all of these elements are
popped off the stack and the function returns.

Here's what's emitted for a @racket[λ]-expression with a free variable:
@ex[
(compile-lambda-define (Lam 'f '(x) (Var 'z)))
]


The compiler will need to generate one such function for each
@racket[λ]-expression in the program.  So we use a helper function for
extracting all the @racket[λ]-expressions:


@codeblock-include["loot/lambdas.rkt"]

And another for compiling each of them:

@#reader scribble/comment-reader
(racketblock
;; [Listof Lam] -> Asm
(define (compile-lambda-defines ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l)
          (compile-lambda-defines ls))]))
)


The top-level @racket[compile] function now extracts and compiles all
the @racket[λ]-expressions to functions:

@#reader scribble/comment-reader
(racketblock
;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '() #t)
           (Ret)
           (compile-lambda-defines (lambdas e))
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))
)

What remains is the issue of compiling @racket[λ]-expressions to code
to create a closure and using closures to provide the appropriate
environment when called.


@section[#:tag "closure" #:tag-prefix "loot"]{Save the Environment: Create a Closure!}


The basic challenge we are faced with is designing a representation of
functions as values.  Like other kinds of values, functions will be
disjoint kind of value, meaning bits representing a function will need
to be tagged distinctly from other kinds of values.  Functions will
need to represent all of the run-time information in the
@racket[Closure] structure used in the interpreter.  Looking back, a
@racket[Closure] contains the formal parameters of the
@racket[lambda]-expression, the body, and the environment in place at
the time the @racket[lambda]-expression was evaluated.

The parameters and body expression are relevant
@racket[compile-lambda-define].  What's relevant for the closure is
the label of @racket[lambda]-expression and the environment.  For the
compiler, the environment can be represented by the sequence of values
it contains at run-time.

So, the way we will represent a closure is by a tagged pointer to a
sequence in memory that contains the label of the closure's code and a
sequence of values that were bound to the free variables when the
@racket[lambda]-expression was evaluated.

When a @racket[lambda]-expression is evaluated, we allocate a closure
on the heap, write the @racket[lambda]'s label, followed by the values
of the free variables.  The result of evaluating the expression is the
tagged pointer to the memory just written.

Here's the function for emitting closure construction code:

@#reader scribble/comment-reader
(racketblock
;; Id [Listof Id] Expr CEnv -> Asm
(define (compile-lam f xs e c) 
  (let ((fvs (fv (Lam f xs e))))
    (seq (Lea rax (symbol->label f))
         (Mov (Offset rbx 0) rax)
         (free-vars-to-heap fvs c 8)
         (Mov rax rbx) ; return value
         (Or rax type-proc)         
         (Add rbx (* 8 (add1 (length fvs)))))))
)

It relies on a helper function for emitting instructions to copy the
value of free variables, i.e. variables bound in the current
environment but outside of the @racket[lambda]-expression.  It fetches
these values just like a variable reference would: it computes the
variables lexical address and fetches it from the stack, then writes
it to the heap.

@#reader scribble/comment-reader
(racketblock
;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (Mov r8 (Offset rsp (lookup x c)))
          (Mov (Offset rbx off) r8)
          (free-vars-to-heap fvs c (+ off 8)))]))
)

That's all there is to closure construction!

@section[#:tag-prefix "loot"]{Calling Functions}

The last peice of the puzzle is making function calls and closures
work together.  Remember that a @racket[λ]-expression is compiled into
a function that expects a closure @emph{plus} its arguments on the
stack.

So the code generated for a function call needs to manage running each
subexpression, the first of which should evaluate to a function (i.e.
a pointer to a label and environment in memory) and then fetching the
function's label and jumping to it.

Here is the code for the non-tail-calls:

@#reader scribble/comment-reader
(racketblock
;; Expr [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail e es c)
  (let ((r (gensym 'ret))
        (i (* 8 (length es))))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c))         
         (Mov rax (Offset rsp i))
         (assert-proc rax)
         (Xor rax type-proc)
         (Mov rax (Offset rax 0)) ; fetch the code label
         (Jmp rax)
         (Label r))))
)

Compared to the previous version of this code, it additionally
executes the code for @racket[_e].  After all the subexpression are
evaluated, it fetches the value of @racket[_e] off the stack, checks
that it is a function, then fetches the label for the function's code
and jumps to it.  Notice how the stack naturally has the function as
the top-most element.  This is used by the code for the function to
fetch the values stored in the closure.

The code for tail calls is similar, but adapted to avoid pushing a
return frame and to pop the local environment before jumping:

@#reader scribble/comment-reader
(racketblock
;; Expr [Listof Expr] CEnv -> Asm
(define (compile-app-tail e es c)
  (seq (compile-es (cons e es) c)
       (move-args (add1 (length es)) (length c))
       (Add rsp (* 8 (length c)))
       (Mov rax (Offset rsp (* 8 (length es))))
       (assert-proc rax)
       (Xor rax type-proc)
       (Mov rax (Offset rax 0))
       (Jmp rax)))
)


We've now implemented all there is to first-class functions.  It's
possible to write recursive functions using the Y-combinator, although
that's no so convenient.  Next we can tackle the issue of recursive or
even sets of mutually recursive functions by dealing with top-level
function definitions.


@section[#:tag-prefix "loot"]{Recursive Functions}

Writing recursive programs with the Y-combinator is a bit
inconvenient.

We previously had the ability to write recursive or even mutually
recursive function definitions by defining them at the top-level with
@racket[define], although that was before functions were considered
first-class values.

What changes now?

Well, one view is that @racket[(define (f x) (add1 x))] is really just
defining a function and giving it a name.  In other words, it's really
just saying @racket[(define f (lambda (x) (add1 x)))].  We already
know how to compile @racket[lambda]-expressions and we all ready know
how to bind variable names to values, so it would seem this is not so
difficult to accomodate.

A program consisting of a series of function definitions followed by
an expression can first compile all the function definitions, then
create a series of closures, push them on the stack, then execute the
main expression in an environment that includes the names of the
defined functions.

That will work just fine for an example like @racket[(define (f x) (add1
x)) (f 5)].

Where it breaks down is in a program like this:

@#reader scribble/comment-reader
(racketblock
(define (f n)
  (if (zero? n)
      1
      (+ n (f (sub1 n)))))

(f 10)
)

Why?  Because the (implicit) @racket[lambda]-expression here has a
free variable @racket[f].  In the closure representation, what should
the value of this variable be?  It should be the function @racket[f]
itself.  In other words, it should be a tagged pointer to the closure,
meaning that the closure representation of a recursive function is a
cyclic data structure!

But how can we create such a structure?  In creating the closure
representation of the function @racket[f] we would need to write the
pointer to the value we are constructing @emph{as we construct it}.

To make matters worse, consider a set of mutually recursive functions
like this:

@#reader scribble/comment-reader
(racketblock
(define (even? x)
  (if (zero? x)
      #t
      (odd? (sub1 x))))
(define (odd? x)
  (if (zero? x)
      #f
      (even? (sub1 x))))

(even? 101)
)

Both @racket[even?] and @racket[odd?] contain a free variable: for
@racket[even?] it's @racket[odd?] and for @racket[odd?] it's
@racket[even?].  Hence the closure representation of @racket[even?]
should be two words long; the first words will be the address of the
label that contains @racket[even?]'s code and the second word will be
the tagged pointer to the @racket[odd?] closure.  Likewise, the
closure representation of @racket[odd?] will be two words long,
containing the address of the label for @racket[odd?] followed by the
tagged pointer to the @racket[even?] closure.

How can we possible construct these two closures that must each point
to the other?

The solution here is to recognize that the closures can be constructed
in a staged way.  We can lay out the memory for each closure but delay
writing the value of the free variables.  This is possible because all
we need to know in order to allocate the memory for a closure is the
number of free variables that occur in the syntax of the
@racket[lambda]-expression.  Once we have addresses for each closure
we are constructing, we can @emph{then} go back and initialize each
closure writing the value of its free variables.  Doing this staged
initialization is safe because we know that none of these functions
can be called before the initialization is complete.  (Try to convince
yourself of this by considering the grammar of programs.)

Using that idea, we can compile the functions defined at the top-level
in a slightly different way from @racket[lambda]-expressions.  We will
first allocate memory for all of the closures and push tagged pointers
for each of them on the stack, effectively binding the defined
function names to their (unitialized) closures.  We then copy free
variable values to memory, initializing the closures.  Doing it in
this way allows functions to refer back to themselves or other
top-level function definitions.

First, the easy stuff: the code of a top-level function definition is
compiled just like a @racket[lambda]-expression:

@#reader scribble/comment-reader
(racketblock
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (compile-lambda-define (Lam f xs e))]))
)

We extend this to lists of function definitions in the obvious way:

@#reader scribble/comment-reader
(racketblock
;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))
)

And in compiling a program @racket[(Prog ds e)] we make sure to emit
@racket[(compile-defines ds)].

Now we have to turn to creating all of the closures for @racket[ds].
To accomplish this, we write a function
@racket[(compile-defines-values ds)] that will create a closure for
each function definition and push it on the stack.

The top-level expression @racket[e] will no longer be compiled in the
empty environment, but instead in an environment that includes all of
the names defined as functions.  So to compile @racket[(Prog ds e)] we
@racket[(compile-e e (reverse (define-ids ds)) #t)], where
@racket[define-ids] is a simple function for fetching the list of
function names defined by @racket[ds] (the list of names is reversed
because the functions are pushed on in the order they appear, hence
the last function is the most recently pushed).


Here is the definition of @racket[compile-defines-values]:

@#reader scribble/comment-reader
(racketblock
;; Defns -> Asm
;; Compile the closures for ds and push them on the stack
(define (compile-defines-values ds)
  (seq (alloc-defines ds 0)
       (init-defines ds (reverse (define-ids ds)) 8)
       (add-rbx-defines ds 0)))
)

It does the staged allocation and initialization of the closures as
described earlier.  Once the closures are allocated and initialized,
it bumps @racket['rbx] by the total size of all the allocated closures.

The @racket[alloc-defines] function allocates, but leaves
uninitialized, each of the closures and pushes them on the stack:

@#reader scribble/comment-reader
(racketblock
;; Defns Int -> Asm
;; Allocate closures for ds at given offset, but don't write environment yet
(define (alloc-defines ds off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (Lea rax (symbol->label f))
            (Mov (Offset rbx off) rax)         
            (Mov rax rbx)
            (Add rax off)
            (Or rax type-proc)
            (Push rax)
            (alloc-defines ds (+ off (* 8 (add1 (length fvs)))))))]))
)

The @racket[init-defines] function intializes each of the closures
using @racket[free-vars-to-heap]:

@#reader scribble/comment-reader
(racketblock
;; Defns CEnv Int -> Asm
;; Initialize the environment for each closure for ds at given offset
(define (init-defines ds c off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (free-vars-to-heap fvs c off)
            (init-defines ds c (+ off (* 8 (add1 (length fvs)))))))]))
)

Finally, the @racket[add-rbx-defines] function computes the total size
of all the closures and adjusts @racket['rbx] appropriately:

@#reader scribble/comment-reader
(racketblock
;; Defns Int -> Asm
;; Compute adjustment to rbx for allocation of all ds
(define (add-rbx-defines ds n)
  (match ds
    ['() (seq (Add rbx (* n 8)))]
    [(cons (Defn f xs e) ds)
     (add-rbx-defines ds (+ n (add1 (length (fv (Lam f xs e))))))]))
)


@section[#:tag-prefix "loot"]{A Complete Compiler}

Putting all the pieces together, we have the complete compile for Loot:

@codeblock-include["loot/compile.rkt"]
