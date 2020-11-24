#lang scribble/manual

@(require (for-label (except-in racket ...)))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(for-each (λ (f) (ev `(require (file ,(path->string (build-path notes "shakedown" f))))))
	   '("compile.rkt" "syntax.rkt" "asm/interp.rkt" "asm/printer.rkt"))

@title[#:tag "Shakedown"]{Shakedown: Calling functions C functions}

@table-of-contents[]

@section[#:tag-prefix "shakedown"]{No Man Is An Island}

@emph{No man is an island,
Entire of itself;
Every man is a piece of the continent, 
A part of the main.} -- John Donne

So far we've been creating new languages by adding more sophisticated features
to a language we have already implemented. By the time we added lambdas in
@secref{Loot}, our languages are @emph{universal} in that any computation that
can be expressed, is able to be expressed in our languages (modulo constraints
on the machine they are run on).

Being able to express any possible computation only gets you so far, however.
In order for general purpose languages to be @emph{useful} in addition to
@emph{interesting}, they must be able to interact with their context. This can
take various forms: being able to perform input/output, being able to
communicate on a network, etc. Ultimately, it is the host operating system that
enables this sort of interaction and communication, via @emph{system calls}.

We could implement system calls directly, but this approach has a few
downsides:


@itemlist[

@item{System calls are not portable across different operating systems (and
      often not portable across different architectures, even with the same OS)}

@item{Adding new functionality would often mean changing the @emph{compiler} in
      order to implement a new system call}
]

Instead, we can implement a @emph{Foreign Function Interface} (FFI), a way for
our language to communicate with another programming language directly. The
question then becomes: which language, and how?

@section[#:tag-prefix "shakedown"]{Pick your Poison}

There are a few languages you might choose to implement as a target for an FFI,
many languages end up with FFIs that can communicate with a selection of
languages. For our FFI, we are going to target C. This is because, for better
or worse, C is the lingua franca of computing.

Most non-embedded systems provide C-libraries that provide a wrapper around
their system calls, often these libraries will actually provide the same
functionality across OSs and machine architectures. The GNU project produces
@tt{glibc}, which is available for most major platforms, and there are projects
such as @tt{musl} which serve a similar purpose through various tradeoffs. By
providing a C-FFI, we can access these standard libraries and any other library
that provides a C interface.

In addition to the motivating factors above, C is appealing as an FFI target
because many languages provide C FFIs, which means that C can be treated as a
`middle ground' between two languages.

@section[#:tag-prefix "shakedown"]{Some considerations}

An FFI can take several forms. As usual, these forms come with varying
tradeoffs. Some FFIs are `type aware', and can do the @emph{marshalling} (the
conversion between data representations in one format to another)
automatically. Other FFIs require the programmer to write the marshaling code
themselves.

Orthogonally, some languages provide facilities for marshalling data in the
language itself (Racket and Haskell do this, for example), while other
languages require the programmer to write `wrapper' code in C that performs
this functionality (OCaml and Lua do this, for example).

We are going to write an FFI that is not type-aware requiring us to write the
marshalling code ourselves, and requires writing C wrapper code. This done so
that we can focus on the core new concept, calling conventions, without
@emph{also} having to solve the problems of marshalling data automatically or
providing functionality for reasoning about C-types in our language.


@section[#:tag-prefix "shakedown"]{What's it look like?}

Now that we've described the high-level shape of our problem, we can take a look
at how we might express that in our language:

@#reader scribble/comment-reader
(racketblock
(ccall function_in_c 3)
)

This program would call a function written in C, @tt{function_in_c}, with an
integer argument with a value of 3. This raises the following issues that we
have to solve:


@itemlist[
@item{How do we represent this in our AST?}
@item{How does our runtime system know where @tt{function_in_c} is?}
@item{How do we `tell' @tt{function_in_c} that the argument is 3?}
@item{How do we `get' the result from @tt{function_in_c}?}
]

@subsection[#:tag-prefix "shakedown"]{Representation Matters}

In addressing the first issue, we can create a new AST node for FFI calls:

@#reader scribble/comment-reader
(racketblock
(struct ccall-e (f es))
)

There are other possible representations (treating it as a primitive, for
example), but this representation will make the next step slightly easier.

@subsection[#:tag-prefix "shakedown"]{Who you gonna call?}

The second issue is a bit more intricate. It deals with how we @emph{link} our
program. In order to safely call (or jump to) @tt{function_is_c} we have to
convince our assembler (NASM) and our linker that we know where
@tt{function_in_c} is!

Our assembler, @tt{nasm}, requires that we declare which symbols are not
defined locally. This is so that the assembler can catch simple errors, like
misspelling the target of a jump. Not all assemblers require this, but because
@tt{nasm} does, we need to address accommodate it.

First we can collect all the uses of the @tt{ccall} construct that we are
introducing. This is a straightforward traversal of the AST, keeping track of
the symbols used for a @tt{ccall}.

@#reader scribble/comment-reader
(racketblock
;; LExpr -> (Listof Symbol)
;; Extract all the calls to C Functions
(define (ffi-calls e)
  (match e
    [(? imm? i)       '()]
    [(var-e v)        '()]
    [(prim-e p es)    (apply append (map ffi-calls es))]
    [(if-e e0 e1 e2)  (append (ffi-calls e0) (ffi-calls e1) (ffi-calls e2))]
    [(let-e (list (binding v def)) body)
                      (append (ffi-calls def) (ffi-calls body))]
    [(letr-e bs body) (append (apply append (map ffi-calls (get-defs bs))) (ffi-calls body))]
    [(lam-e xs e0)    (ffi-calls e0)]
    [(lam-t _ xs e0)  (ffi-calls e0)]
    [(ccall-e f es)   (cons f (apply append (map ffi-calls es)))]
    [(app-e f es)     (append (ffi-calls f) (apply append (map ffi-calls es)))]))
)

Once we've collected all the uses of @tt{ccall} we can adapt our
@tt{compile-entry} function so that all of the external symbols are generated
in our assembly file:

@#reader scribble/comment-reader
(racketblock
;; Expr -> Asm
(define (compile-entry e)
    `(,@(make-externs (ffi-calls e)) 
      (section text)
      entry
      ,@(compile-tail-e e '())
      ret
      ,@(compile-λ-definitions (λs e)) 
      err
      (push rbp)
      (call error)
      ret))
)

The addition of the @tt{(section text)} directive is something we were doing in
our printer before, as part of the preamble for our generated code. Now that we
are adding the @tt{extern} directives we need make the distinction between the
preamble and the code itself (text stands for code in ASM).

The next two points are related by a single concept: Calling Conventions


@section[#:tag-prefix "shakedown"]{Calling Conventions}

How functions accept their arguments and provide their results is known as a
@emph{calling convention}. All of our languages since @secref["Iniquity"] have
had calling conventions, but it's been mostly up to us (modulo the issue with
moving @tt{rsp}. This has worked @emph{because} we haven't had to communicate
with any other language, that expects its arguments to be provided in a
specific manner.

The calling convention we are using the
@link["https://uclibc.org/docs/psABI-x86_64.pdf"]{x86_64 System V Application
Binary Interface (ABI)} (which means this may not work on Windows systems). The
document is quite long (approximately 130 pages), so we will only focus on some
of the basics. For every limitation that our implementation, the details on how
we might address that limitation will be in that document. For example, we will
only deal with integer arguments and results here, but the System V ABI also
describes the convention for Floating Point arguments/results.

In short, a calling convention specifies @emph{at least} the following:

@itemlist[
@item{How do you pass arguments?}
@item{What things is the @emph{caller} responsible for keeping track of?}
@item{What things is the @emph{callee} responsible for keeping track of?}
]




@subsection[#:tag-prefix "shakedown"]{Determining the point of the argument}

We can now discuss

@subsection[#:tag-prefix "shakedown"]{Securing the result}

@codeblock-include["shakedown/compile.rkt"]

