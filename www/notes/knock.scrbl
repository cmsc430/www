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
   (parameterize ([current-directory (build-path langs "knock")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))


@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path langs "knock"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(ev '(current-objs '("runtime.o")))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("interp.rkt" "compile.rkt" "ast.rkt" "parse.rkt" "types.rkt"))

@(define this-lang "Knock")

@title[#:tag this-lang]{@|this-lang|: pattern matching}

@src-code[this-lang]

@table-of-contents[]

@section[#:tag-prefix "knock"]{Matching}

One feature we've taken advantage extensively in the writing of our
compilers is the Racket's @racket[match] facility for pattern
matching.

Let's add a similar feature to our own language.

We'll call it @bold{@this-lang}!

In @|this-lang|, we will support a limited form of pattern matching of
the form:

@racketblock[
(match _e
  [_p0 _e0]
  ...)
]

A pattern matching expression is used to perform case-analysis,
deconstruction, and binding on the value produced by @racket[_e].  A
match consists of any number of clauses, where each clause consists of
a pattern @racket[_pi] and expression @racket[_ei].  Each @racket[_pi]
is a @emph{pattern}, which can include literal booleans, characters,
integers, and the empty list, or can be a pattern variable, a
wildcard, a @racket[cons]-pattern, or a @racket[and]-pattern. Clauses
are matched in the order in which they appear and if a pattern matches
the value of @racket[_e], then the corresponding expression is
evaluated in an environment that binds any pattern variables to the
matching parts of @racket[_e]'s value.  If no patterns match
@racket[_e]'s value, an error is signalled.

The syntax is extended as follows:

@codeblock-include["knock/ast.rkt"]


@section[#:tag-prefix "knock"]{Match by Example}

Since we've been using pattern matching throughout the course, it
probably is pretty natural at this point, but let's quickly walk
through some examples to try and disentangle the different aspects of
@racket[match].

Perhaps the simplest form of a @racket[match]-expression uses a
pattern that just consists of a variable, e.g.

@racketblock[
(match _e
  [x _e0])]

This expression is equivalent to @racket[(let ((x _e)) _e0)] because a
pattern variable matches any value and binds that name in the scope of
its right-hand expression.  We can see from this example that
@racket[match] is doing variable binding.

Relatedly, a ``wildcard'' pattern can be used to match anything
@emph{without} binding the value to a name:

@racketblock[
(match _e
  [_ _e0])]

This expression is equivalent to @racket[(begin _e _e0)].

Another simple form of pattern is to use a literal such as an integer,
character, etc. which matches when the value is the same as the
literal.  This form of pattern doesn't bind any names, but is used to
discriminate between different cases of what the value may be.  For
example:

@racketblock[
(match _e
  [#f _e1]
  [_  _e2])]

This expression is equivalent to @racket[(if _e _e2 _e1)]. Here we can
see that @racket[match] is doing conditional evaluation, selecting
@racket[_e1] if @racket[_e] produces @racket[#f], and selecting
@racket[_e2] otherwise.

A more complicated pattern involves a constructor-style pattern like
@racket[cons]:

@racketblock[
(match _e
  [(cons x y) _e1]
  [_ _e2])
]

Here, the @racket[cons] pattern is both discriminating between
@racket[cons] and non-@racket[cons] values, matching only when
@racket[_e] is a pair, but also binding the names @racket[x] and
@racket[y] to the components of the pair when the value is in fact a
pair; these names are bound in the scope of @racket[_e1].  In this
way, the pattern is used to @emph{destructure} compound values such as
pair.

The @racket[x] and @racket[y] in this example are actually just
instances of patterns themselves, and patterns can be nested
arbitrarily deep.  So for example, if we wanted only to match
a pair containing @racket[1] and @racket[2], we could write:

@racketblock[
(match _e
  [(cons 1 2) _e1]
  [_ _e2])]

The @racket[and]-pattern is used to match the conjunction of two
patterns, so @racket[(and _p1 _p2)] matches whenever @racket[_p1] and
@racket[_p2] both match and binds all of the names in @racket[_p1] and
@racket[_p2].  For example,

@racketblock[
(match _e
  [(and (cons 1 x) (cons y 2)) _e1]
  [_ _e2])
]

The first clause matches when @racket[_e] evaluates to @racket[(cons 1
2)] and binds the name @racket[x] to @racket[2] and @racket[y] to
@racket[1] in the scope of @racket[_e1].


Here are some complete examples and how they are parsed:

@ex[
(parse-e '(match z [x x]))
(parse-e '(match z [_ #t]))
(parse-e '(match z [1 #t]))
(parse-e '(match z [1 #t] [2 #f]))
(parse-e '(match z [(cons x y) #t]))
(parse-e '(match z [(cons 1 2) #t]))
(parse-e '(match z [(and (cons x 2) (cons 1 y)) #t]))
(parse-define
  '(define (length xs)
     (match xs
       ['() 0]
       [(cons x xs)
        (add1 (length xs))])))
]

@section[#:tag-prefix "knock"]{An Interpreter for Pattern Matching}

At the heart of interpreter for @this-lang is the function:

@#reader scribble/comment-reader
(racketblock
;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r) ...)
)

This function takes a single pattern and value, along with an
environment, and determines whether the pattern matches the value, and
if so, an environment that binds the variables in the pattern to the
sub-parts of the value that match.  If the pattern doesn't match,
@racket[#f] is produced.

So for example, if the pattern is simply a variable @racket[x], the
function produces @racket[r] extended to bind @racket[x] to
@racket[v].  If the pattern is a wildcard, it produces @racket[r],
indicating a match, but with no new bindings.  Likewise, if the
pattern is a literal, it produces @racket[r] when the value is the
same as the literal.  The more interesting cases are of @racket[cons]-
and @racket[and]-patterns which recursively match the sub-patterns.


It's important to see that this function's return type is
communicating multiple things at the same time.  If the pattern
doesn't match, it produces @racket[#f].  If it produces an
environment, it means the pattern matched @emph{and} the environment
communicates the binding of the pattern variables to values.

Let's consider some examples:
@ex[
(interp-match-pat (Var '_) 99 '())
]

Here the pattern matches, but binds no variables so the result is the
same environment as given.

@ex[
(interp-match-pat (Var 'x) 99 '())
]

Here the pattern matches and binds @racket[x] to @racket[99], which is
reflected in the output environment.

@ex[
(interp-match-pat (Lit 99) 99 '())
]

Here the pattern matches but binds nothing.

@ex[
(interp-match-pat (Lit 100) 99 '())
]

Here the pattern doesn't match.


@ex[
(interp-match-pat (Conj (Lit 99) (Var 'x)) 99 '())
]

Here the pattern matches and binds @racket[x] to @racket[99].

@ex[
(interp-match-pat (Conj (Lit 100) (Var 'x)) 99 '())
]

Here the pattern doesn't match.

@ex[
(interp-match-pat (Cons (Var 'x) (Var 'y)) 99 '())
]

Here the pattern doesn't match.

@ex[
(interp-match-pat (Cons (Var 'x) (Var 'y)) (cons 99 100) '())
]

Here the pattern matches and binds @racket[x] to @racket[99] and
@racket[y] to @racket[100].

As you can see, the patterns can be nested arbitrarily deep but the
environment produced will bind each variable to the appropriate
sub-part of the given value:

@ex[
(interp-match-pat (Cons (Cons (Var 'x) (Var 'y))
                         (Cons (Var 'p) (Var 'q)))
                  (cons (cons 99 100)
                        (cons #t #f))
                  '())
]

The complete code for @racket[interp-match-pat] is:

@#reader scribble/comment-reader
(racketblock
;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    [(Var '_) r]
    [(Var x) (ext r x v)]
    [(Lit l) (and (eqv? l v) r)]
    [(Box p)
     (match v
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(Cons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(Conj p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]))
)


With @racket[interp-match-pat], we can then build up the function for
interpreting a @racket[match] expression:

@#reader scribble/comment-reader
(racketblock
;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds) ...)
)

This function traverses the patterns in order until finding one that
matches (using @racket[interp-match-pat]) and then evaluating the
corresponding right-hand expression in the environment that
@racket[interp-match-pat] produced.  If it runs out of clauses without
finding a matching, it produces an error.

It's fairly straightforward:

@#reader scribble/comment-reader
(racketblock
;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (interp-env e r ds)])]))
)

The complete interpreter:

@codeblock-include["knock/interp.rkt"]


We can now see it in action:

@ex[
(define (run e)
  (interp-env (parse-e e) '() '()))

(run '(match 1 [1 #t] [_ #f]))
(run '(match 2 [1 #t] [_ #f]))
(run '(match 2 [x x] [_ #f]))
(run '(match (cons 1 2) [(cons x y) x] [_ #f]))
(run '(match (box 1) [(box x) x] [_ #f]))
(run '(match (box 1) [(box 2) #t] [_ #f]))
]

And we can use pattern matching to define functions in style similar
to what we've been using all semester:

@ex[
(interp
 (parse
  '(define (length xs)
     (match xs
       ['() 0]
       [(cons x xs)
        (add1 (length xs))]))
  '(length (cons 7 (cons 8 (cons 9 '()))))))
]


@section[#:tag-prefix "knock"]{A Compiler for Pattern Matching}

The compilation of pattern matching expression is significantly more
complicated compared to interpretation.

Most of the complication is due to the fact that the computation of
the binding structure in the interpreter must be split and mirrored
across compile-time and run-time in the compiler.  Each
right-hand-side of a clause must be compiled in a static environment
that is dependent on the variables occurring in the left-hand-side
pattern.  At run-time, these variables will be bound by pushing parts
of the matched value on the stack.

To make matters worse, the stack will also be needed to save
intermediate results for later processing.  For example, in matching a
@racket[cons]-pattern, we must push the @racket[cdr] of the pair on
the stack while pattern-matching the @racket[car].

The function @racket[compile-pat] has the following signature:

@#reader scribble/comment-reader
(racketblock
;; Pat CEnv Symbol -> (list Asm Asm CEnv)
(define (compile-pat p cm next) ...)
)

It consumes a single pattern, which it is compiling, a static
environment that describes the bindings that have occurred so far, and
label name which denotes where to jump to in order to try matching the
next pattern.

It produces three things:

@itemlist[

@item{a sequence of instructions which determine whether the value in
@racket['rax] match the pattern @racket[p] and bind any variables that
may occur in @racket[p],}

@item{a sequence of instructions which handle what to do if @racket[p]
doesn't match such as restoring the stack to its state before the
match started and jumping to @racket[next], and}

@item{a static environment that describes the bindings of the pattern
in case it matches.}

]


Let's look at some examples.  First, consider the wildcard pattern:

@ex[
(compile-pattern (Var '_) '() 'next)
]

When the pattern is a wildcard, it produces an empty sequence of
instructions for the ``determine if the pattern matches'' part.  This
is because the pattern @emph{always} matches.  There's nothing to do.
Similarly, it produces an empty sequence of instructions for the
``what to do if it doesn't match'' part because that's impossible; this
pattern always matches.  Finally, it produces the environment it was
given because it doesn't bind anything.

Now pattern variables:

@ex[
(compile-pattern (Var 'x) '() 'next)
]

A pattern variable always matches and binds the value to @racket[x],
so in the ``determine and bind'' part it simply pushes @racket['rax]
on to the stack to bind the value.

It has empty sequences of instructions for the ``failing'' part
because it always matches just like a wildcard.  Finally the static
environment part adds @racket[x] to the environment because this
pattern binds @racket[x] when it matches.

Pattern literals:

@ex[
(compile-pattern (Lit 0) '() 'next)
]

In the ``determine and bind'' part, we compare the value in
@racket['rax] to the literal.  If they are not equal, the pattern
doesn't match so control jumps a generated label that is defined in
the ``fail'' part.  The instructions in the ``fail'' part pop off all
of the current bindings in the pattern (in this example there are
none) and then jumps to @racket[next].

The environment stays the same because a literal doesn't bind anything.

Supposing we had changed the example to:

@ex[
(compile-pattern (Lit 0) '(x y z) 'next)
]

This is essentially saying ``compile the pattern @racket[(Lit 0)]
assuming it occurs in the context of a surrounding pattern that binds
@racket[x], @racket[y], and @racket[z] before getting to this point.''
If it fails, it needs to pop all three bindings of the stack, hence
the ``fail'' code adds @racket[24] to @racket['rsp] before jumping to
@racket['next].

Now we get to the inductive patterns, which will be more interesting.
Let's start with the @racket[box]-pattern.

@ex[
(compile-pattern (Box (Var '_)) '() 'next)
]

This ``determine and bind'' part moves the value to a temporary
register and masks the final three bits then compares the result to
the type tag for boxes.  If they are not equal, the value in
@racket['rax] is not a box, so it jumps to the generated label for the
``fail'' part, which pops all bound pattern variables before jumping
to @racket['next].  If the value is a box, it is untagged and the
value inside the box is fetched to @racket['rax] for the subsequent
pattern to match against, in this case the wildcard.  Nothing is bound
so no changes in the output environment.

Let's change the wild card to a literal:

@ex[
(compile-pattern (Box (Lit 0)) '() 'next)
]

This works just like before but now in the ``determine and bind''
instructions, it compares the unboxed value to @racket[0].

Notice that the code here is modifying @racket['rax].  As it descends
into the box and tries to match the inner pattern, it moves the value
inside the box into @racket['rax].  This is important because it
maintains the invariant that the pattern is being matched against the
value in @racket['rax], but it also means that in compound patterns,
we may have to do more work to ensure the right value is in
@racket['rax].

Let's consider a @racket[cons]-pattern.  A @racket[cons]-pattern is
similar to a @racket[box] pattern in that the first thing it needs to
do is determine if the value is a pointer tagged with the appropriate
type, in this case the @racket[cons] tag.  Then it needs to move a
value into @racket['rax] and check if a subpattern matches.  In
particular, it needs to move the @racket[car] value into @racket['rax]
and check if the first subpattern matches.

Assuming it does match, what happens next?  We need to move the
@racket[cdr] value in to @racket['rax] and check it matches the second
subpattern.  But where can we get the @racket[cdr]?  The moment we
overwrite @racket['rax] with the @racket[car], we've lost a handle on
the pair and thus access to the @racket[cdr].

The solution is to use the same mechanism we've always used to save
values: push it on the stack and fetch it later.  With this in mind,
consider the following example for matching @racket[(cons 0 0)]:

@ex[
(compile-pattern (Cons (Lit 0) (Lit 0)) '() 'next)
]

This starts off like the @racket[box] pattern checking the tag bits of
the value.  But then, before moving the @racket[car] into
@racket['rax], it pushes the @racket[cdr] on the stack.  It then
installs the @racket[car] and checks if it matches @racket[0].  If it
does, it then installs the @racket[cdr] off the stack and into
@racket['rax] to check if it too is @racket[0].  Note that if either
subpatterns fail to match, they both jump to code that pops a single
element off the stack, which is the stashed away @racket[cdr] value
that was pushed.

Also note that the static environment produced is @racket['(#f)] to
account for the @racket[cdr] value that was pushed.

The @racket[and]-pattern is a bit like @racket[cons] in that it has to
push a value on the stack in order to restore it after matching the
first subpattern:

@ex[
(compile-pattern (Conj (Lit 0) (Lit 0)) '() 'next)
]

The @racket[compile-pattern] function is used by
@racket[compile-match-clause] which takes care of compiling a single
@racket[match] clause.  It is given a pattern patterns and a
right-hand-side expression to execute should the pattern match, an
environment that describes the current bindings, a label to jump to
when the code is done, i.e. the correct result is in @racket['rax],
and finally a boolean indicating if this @racket[match] expression is
in tail position.

@#reader scribble/comment-reader
(racketblock
;; Pat Expr CEnv Symbol Bool -> Asm
(define (compile-match-clause p e c done t?) ...)
)

This function stitches together the parts returned by
@racket[compile-pattern] to implement a clause.  This function assumes
the value to be matched is the top element of the stack, so the first
thing is does is fetch the value and install it in @racket['rax].  It
then executes the ``determine if the pattern matches and bind'' code
followed by the right hand side expression, then pops all the
pattern-bound values off the stack and jumps to @racket[done].  After
this it emits the code for what to do if the pattern doesn't fail
(thus jumping to @racket[done] will jump past this code).

Consider a match clause like @racket[[_ #t]]:

@ex[
(compile-match-clause (Var '_) (Lit #t) '() 'done #f)
]

Here we can see the value being matched is fetched from the top of the
stack.  Since this pattern always matches, it next executes the
right-hand-side by moving the bit-representation of @racket[#t] into
@racket['rax].  It pops everything matching the pattern pushed on the
stack (in this case nothing), then jumps to @racket[done].  The final
label, which is never reached, is where control should jump to in
order to try matching the next clause.

Let's look at a literal; consider a clause @racket[[0 #t]]:

@ex[
(compile-match-clause (Lit 0) (Lit #t) '() 'done #f)
]

As always, it starts by fetching the top of the stack and putting the
value in @racket['rax].  It then does the ``determine if matches and
bind'' instructions followed by the right-hand-side.  If the value in
@racket['rax] is not @racket[0] it will jump to code that handles the
failure to match by popping of anything pushed to the stack (in this
case nothing) and then jumping to the next clause (in this case,
that's the next label, but this isn't the case in general).  If the
value in @racket['rax] is @racket[0], @racket[#t] is moved into
@racket['rax], the stack is popped, and control jumps to
@racket[done].

Let's see what a clause involving a pattern variable looks like,
e.g. @racket[[x x]].  Here we're going to reference the variable bound
in the pattern in the right-hand-side:

@ex[
(compile-match-clause (Var 'x) (Var 'x) '() 'done #f)
]

The value being matched is fetched from the stack.  It's immediately
pushed (again) to the stack because the variable pattern always
matches and binds.  We then execute the right hand side, which is just
a reference to @racket[x], hence it fetches the top element of the
stack, then pops this off and jumps to @racket[done].

OK, now let's try something like @racket[[(box x) x]]:

@ex[
(compile-match-clause (Box (Var 'x)) (Var 'x) '() 'done #f)
]

The value being matched is fetched from the stack.  It's checked for
whether it is a box, jump away when it isn't.  Otherwise it unboxes
the value and pushes it on the stack to bind to @racket[x], the
executes the RHS, which fetches @racket[x] into @racket['rax], pops,
and jumps to @racket[done].

Here is the complete code for @racket[compile-match-clause]:

@#reader scribble/comment-reader
(racketblock
;; Pat Expr CEnv Symbol Bool -> Asm
(define (compile-match-clause p e c done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i f cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) t?)
            (Add rsp (* 8 (length cm)))
            (Jmp done)
            f
            (Label next))])))
)

Generating code for a sequence of @racket[match] clauses is as simple
as generate the code for each clause in sequence:

@#reader scribble/comment-reader
(racketblock
;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> Asm
(define (compile-match-clauses ps es c done t?)
  (match* (ps es)
    [('() '()) (seq)]
    [((cons p ps) (cons e es))
     (seq (compile-match-clause p e c done t?)
          (compile-match-clauses ps es c done t?))]))
)

Finally, we have a function for compiling a complete @racket[match]
expression:

@#reader scribble/comment-reader
(racketblock
;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> Asm
(define (compile-match e ps es c t?)
  (let ((done (gensym)))  
    (seq (compile-e e c t?)
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) done t?)
         (Jmp 'raise_error_align)
         (Label done)
         (Add rsp 8)))) ; pop the saved value being matched
)

We can check that the compiler works for a complete example:

@ex[
(define (run . p)
  (bits->value (asm-interp (compile (apply parse p)))))

(run
 '(define (length xs)
    (match xs
      ['() 0]
      [(cons x xs) (add1 (length xs))]))
 '(length (cons 7 (cons 8 (cons 9 '())))))
]



With these pieces in place, here's the complete compiler:

@codeblock-include["knock/compile.rkt"]
