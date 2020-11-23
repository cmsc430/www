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
@item{How does our runtime system know where @tt{function_in_c} is?}
@item{How do we `tell' @tt{function_in_c} that the argument is 3?}
@item{How do we `get' the result from @tt{function_in_c}?}
]

The first of these issues has to do with how we @emph{link} our program. In
order to safely call (or jump to) @tt{function_is_c} we have to convince our
assembler (NASM) and our linker that we know where @tt{function_in_c} is!

@codeblock-include["shakedown/compile.rkt"]

