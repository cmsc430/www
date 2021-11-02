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

@title[#:tag "Loot"]{Loot: lambda the ultimate}

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
(define (run . p) (interp (parse p)))

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
function and the environment mapping the free variable @racket['x] to
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

@codeblock-include["loot/fv.rkt"]

We can now write the function that compiles a labelled
@racket[λ]-expression into a function in assembly:

@#reader scribble/comment-reader
(racketblock
;; Lam -> Asm
(define (compile-lambda-define l)
  (match l
    [(Lam f xs e)
     (let ((env (append (fv l) (reverse xs))))
       (seq (Label (symbol->label f))
            (compile-e e env #t)
            (Add rsp (* 8 (length env))) ; pop env
            (Ret)))]))
)

Notice how similar it is to our function definition compiler:

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

The only real difference is that the values of the free variables of
the function will be supplied as addition implicit arguments when the
function is called.  The values will be retrieved from the
representation of a closure and placed on the stack before jumping the
function's code.


Here's what's emitted for a @racket[λ]-expression with a free variable:
@ex[
(compile-lambda-define (Lam 'f '(x) (Var 'z)))
]

Notice that it's identical to a @racket[λ]-expression with an added
parameter and no free variables:
@ex[
(compile-lambda-define (Lam 'f '(x z) (Var 'z)))
]

The compiler will need to generate one such function for each
@racket[λ]-expression in the program.  So we use a helper function for
extracting all the @racket[λ]-expressions:


@codeblock-include["loot/lambdas.rkt"]

And another for compiling each of them:

@#reader scribble/comment-reader
(racketblock
;; [Listof Lam] -> Asm
(define (compile-lambda-defines ds)
  (seq
    (match ds
      ['() (seq)]
      [(cons d ds)
       (seq (compile-λ-definition d)
            (compile-λ-definitions ds))])))
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
           (compile-defines ds)
           (compile-lambda-defines (lambdas e))
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))
)

What remains is the issue of compiling @racket[λ]-expressions to code
to create a closure and using closures to provide the appropriate
value of free variables when called.

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


@;{

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
;; (Listof Variable) Label (Listof Variable) CEnv -> Asm
(define (compile-λ xs f ys c)
  (seq
    ; Save label address
    (Lea rax (symbol->label f))
    (Mov (Offset rbx 0) rax)

    ; Save the environment
    (%% "Begin saving the env")
    (Mov r8 (length ys))

    (Mov (Offset rbx 8) r8)
    (Mov r9 rbx)
    (Add r9 16)
    (copy-env-to-heap ys c 0)
    (%% "end saving the env")

    ; Return a pointer to the closure
    (Mov rax rbx)
    (Or rax type-proc)
    (Add rbx (* 8 (+ 2 (length ys))))))
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
    ['() (seq)]
    [(cons x fvs)
     (seq
       ; Move the stack item  in question to a temp register
       (Mov r8 (Offset rsp (lookup x c)))

       ; Put the iterm in the heap
       (Mov (Offset r9 i) r8)

       ; Do it again for the rest of the items, incrementing how
       ; far away from r9 the next item should be
       (copy-env-to-heap fvs c (+ 8 i)))]))
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
;; Expr (Listof Expr) CEnv -> Asm
(define (compile-call f es c)
  (let* ((cnt (length es))
         (aligned (even? (+ cnt (length c))))
         (i (if aligned 1 2))
         (c+ (if aligned
                 c
                 (cons #f c)))
         (c++ (cons #f c+)))
    (seq

      (%% "Begin compile-call")
      ; Adjust the stack for alignment, if necessary
      (if aligned
          (seq)
          (Sub rsp 8))

      ; Generate the code for the thing being called
      ; and push the result on the stack
      (compile-e f c+)
      (%% "Push function on stack")
      (Push rax)

      ; Generate the code for the arguments
      ; all results will be put on the stack (compile-es does this)
      (compile-es es c++)
  
      ; Get the function being called off the stack
      ; Ensure it's a proc and remove the tag
      ; Remember it points to the _closure_
      (%% "Get function off stack")
      (Mov rax (Offset rsp (* 8 cnt)))
      (assert-proc rax)
      (Xor rax type-proc)

      (%% "Get closure env")
      (copy-closure-env-to-stack)
      (%% "finish closure env")

      ; get the size of the env and save it on the stack
      (Mov rcx (Offset rax 8))
      (Push rcx)
  
      ; Actually call the function
      (Call (Offset rax 0))
  
      ; Get the size of the env off the stack
      (Pop rcx)
      (Sal rcx 3)

      ; pop args
      ; First the number of arguments + alignment + the closure
      ; then captured values
      (Add rsp (* 8 (+ i cnt)))
      (Add rsp rcx))))
)

The main aspect involving lambdas is @racket[copy-closure-env-to-stack].
Unlike the closure construction code, in which we statically know what
and how many variables to save in a closure, we must dynamically
loop over the environment to move values to the stack:

@#reader scribble/comment-reader
(racketblock
;; -> Asm
;; Copy closure's (in rax) env to stack in rcx
(define (copy-closure-env-to-stack)
  (let ((copy-loop (symbol->label (gensym 'copy_closure)))
        (copy-done (symbol->label (gensym 'copy_done))))
    (seq

      (Mov r8 (Offset rax 8)) ; length
      (Mov r9 rax)
      (Add r9 16)             ; start of env
      (Label copy-loop)
      (Cmp r8 0)
      (Je copy-done)
      (Mov rcx (Offset r9 0))
      (Push rcx)              ; Move val onto stack
      (Sub r8 1)
      (Add r9 8)
      (Jmp copy-loop)
      (Label copy-done))))
)

Let's try it out:

@ex[
(asm-interp (compile (parse '((let ((x 8)) (λ (y) x)) 2))))
(asm-interp (compile (parse '(((λ (x) (λ (y) x)) 8) 2))))
(asm-interp (compile (parse '((λ (f) (f (f 0))) (λ (x) (add1 x))))))
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
  (seq
    (compile-letrec-λs ls c)
    (compile-letrec-init fs ls (append (reverse fs) c))
    (compile-e e (append (reverse fs) c))
    (Add rsp (* 8 (length fs)))))
)

The first two tasks are taken care of by @racket[compile-letrec-λs],
which allocates unitialized closures and pushes each on the stack.

@#reader scribble/comment-reader
(racketblock
;; (Listof Lambda) CEnv -> Asm
;; Create a bunch of uninitialized closures and push them on the stack
(define (compile-letrec-λs ls c)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (match l
       [(Lam lab as body)
        (let ((ys (fvs l)))
             (seq
               (Lea rax (Offset (symbol->label lab) 0))
               (Mov (Offset rbx 0) rax)
               (Mov rax (length ys))
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-proc)
               (Add rbx (* 8 (+ 2 (length ys))))
               (Push rax)
               (compile-letrec-λs ls (cons #f c))))])]))
)

The @racket[compile-letrec-init] goes through each function and
initializes its closure now that all of the function pointers are
available.  Finally the body is compiled in an extended environment.

@#reader scribble/comment-reader
(racketblock
;; (Listof Variable) (Listof Lambda) CEnv -> Asm
(define (compile-letrec-init fs ls c)
  (match fs
    ['() (seq)]
    [(cons f fs)
     (let ((ys (fvs (first ls))))
          (seq
            (Mov r9 (Offset rsp (lookup f c)))
            (Xor r9 type-proc)
            (Add r9 16) ; move past label and length
            (copy-env-to-heap ys c 0)
            (compile-letrec-init fs (rest ls) c)))]))
)

We can give a spin:

@ex[
(asm-interp (compile (parse
                      '(letrec ((even?
                                (λ (x)
                                  (if (zero? x)
                                      #t
                                      (odd? (sub1 x)))))
                               (odd?
                                (λ (x)
                                  (if (zero? x)
                                      #f
                                      (even? (sub1 x))))))
                        (even? 10)))))

(asm-interp 
  (compile (parse
    '(letrec ((map (λ (f ls)
                    (letrec ((mapper (λ (ls)
                                       (if (empty? ls)
                                         '()
                                         (cons (f (car ls)) (mapper (cdr ls)))))))
                      (mapper ls)))))
      (map (λ (f) (f 0))
           (cons (λ (x) (add1 x))
                 (cons (λ (x) (sub1 x))
                       '())))))))
]




@section[#:tag-prefix "loot" #:tag "sugar"]{Syntactic sugar for function definitions}

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
(define (desugar e+)
  (match e+
    [(Prog '() e)    (Prog '() (desugar e))]
    [(Prog ds e)     (let ((defs (map desugar ds)))
                          (Prog '() (LetRec defs e)))]
    [(Defn f xs e)   (list f (Lam f xs e))]
    [(Prim1 p e)     (Prim1 p (desugar e))]
    [(Prim2 p e1 e2) (Prim2 p (desugar e1) (desugar e2))]
    [(If e1 e2 e3)   (If (desugar e1) (desugar e2) (desugar e3))]
    [(Begin e1 e2)   (Begin (desugar e1) (desugar e2))]
    [(Let x e1 e2)   (Let x (desugar e1) (desugar e2))]
    [(LetRec bs e1)  (LetRec (map (lambda (xs) (map desugar xs)) bs) (desugar e1))]
    [(Lam n xs e)    (Lam (gensym 'lam) xs (desugar e))]
    [(App f es)      (App (desugar f) (map desugar es))]
    [_               e+]))
)

The compiler now just desugars before labeling and compiling expressions.

And here's the complete compiler, including tail calls, @racket[letrec], etc.:

@codeblock-include["loot/compile.rkt"]
}