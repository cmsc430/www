Standard Library and Modules
============================

Standard Library
----------------

Here's a proposal for how to add a standard library.  I think with
some small tweaks it can be cleaner and more uniform and support
user-defined libraries and we can drop the distinction between a
program and a library and just deal with modules.  But first, let me
describe what I've added:

There is a new file `compile-library.rkt` that given a file name,
reads the definitions in that file, compiles them, and then declares
those definitions as `Global`.

There is a standard library implementation in `stdlib.rkt`.

The `Makefile` now declares the runtime depends on `stdlib.o` and
there is a special rule to compile `stdlib.s` by using
`compile-library.rkt` instead of `compile-file.rkt`.

The program compiler has been updated to declared standard library
functions as `External` and the parser will parse any non-primitive
application as `(App <id> <exp> ...)` so any use of a standard library
function is just a function call.

Discussion
----------

A library and a program are pretty similar.  The key difference is a
library has no top-level expression, so the compiler does not emit an
`entry` label.

The other difference is a library declares all defined functions with
`External`.

One way to unify the two is to handle `provide` specs at the top of a
library/program.  Everything in the `provide` gets declared
`External`.  The programs we've been writing implicit have an empty
`provide` spec.

It's also pretty easy to handle `require` specs.  A `require` gives a
filename, the compiler reads that file fetching the `provides` and
declares those things `External`.

The `Makefile` would need to be updated to compute the dependencies of
.rkt file by reading the requires.  Every file implicitly depends on
stdlib.rkt and there would have to be a special case when computing
stdlib.rkt's dependecies (so it wouldn't be circular).

At that point, I think compile-library.rkt can go away and there can
be a single notion of a "module".

EXCEPT: I don't quite know how to resolve the issue of there being a
distinguished module that contains the entry point the runtime should
jump to.

One idea is that the runtime should jump to a `main` function.  So one
of the modules will need to provide `main`, and now everything is just
module with a uniform compilation strategy.  The parser could be set
up so that if there's only an expression in the file, it parses it as
`(define (main) e)`.
