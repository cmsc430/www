Standard Library and Modules
============================

This is a work in progress on a uniform approach to a standard library
and modules.  It's mostly OK, but needs work on the file system
aspects and the Makefile is a little brittle.  Not sure it fits in
with the `test-progs` approach to testing either.



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

Example
-------

Here's an example of a program using a standard library function
`reverse`; notice how `stdlib.rkt` gets compiled with
`compile-library.rkt`:

```
> more example.rkt
#lang racket
(reverse (cons 1 (cons 2 (cons 3 '()))))

> make example.run
gcc -fPIC -c -g -o main.o main.c
gcc -fPIC -c -g -o values.o values.c
gcc -fPIC -c -g -o print.o print.c
racket -t compile-library.rkt -m stdlib.rkt > stdlib.s
nasm -g -f macho64 -o stdlib.o stdlib.s
gcc -fPIC -c -g -o io.o io.c
ld -r main.o values.o print.o stdlib.o io.o -o runtime.o
racket -t compile-file.rkt -m example.rkt > example.s
nasm -g -f macho64 -o example.o example.s
gcc runtime.o example.o -o example.run
rm example.o example.s
>  ./example.run
'(3 2 1)
```


Discussion
----------

A library and a program are pretty similar.  The key difference is a
library has no top-level expression, so the compiler does not emit an
`entry` label.

The other difference is a library declares all defined functions with
`Global`.

One way to unify the two is to handle `provide` specs at the top of a
library/program.  Everything in the `provide` gets declared
`Global`.  The programs we've been writing implicitly have an empty
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
`(provide main) (define (main) e)`.

Modules
-------

OK, I implemented roughly what's described above.  Now there's just a
notion of a module and a uniform way of compiling modules.

A module that has an expression in it or a main function is considered
the main module and the run-time will jump to it.

I more or less am happy with this EXCEPT, building programs is more of
a pain now because `make` doesn't just magically work.  If a module
requires another module, you have to build its object code and then
link it by hand.

Here's an example; the main module is example.rkt, and it requires
sum.rkt, and also uses a standard library function, `reverse`:

```
> cat sum.rkt
#lang racket
(provide sum)
(define (sum xs)
  (if (empty? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

> cat example.rkt
#lang racket
(require "sum.rkt")
(sum (reverse (cons 1 (cons 2 (cons 3 '())))))

> make runtime.o
gcc -fPIC -c -g -o main.o main.c
gcc -fPIC -c -g -o values.o values.c
gcc -fPIC -c -g -o print.o print.c
racket -t compile-file.rkt -m stdlib.rkt > stdlib.s
nasm -g -f macho64 -o stdlib.o stdlib.s
gcc -fPIC -c -g -o io.o io.c
ld -r main.o values.o print.o stdlib.o io.o -o runtime.o
rm stdlib.s
> make sum.o
racket -t compile-file.rkt -m sum.rkt > sum.s
nasm -g -f macho64 -o sum.o sum.s
rm sum.s
> make example.o
racket -t compile-file.rkt -m example.rkt > example.s
nasm -g -f macho64 -o example.o example.s
rm example.s
> gcc example.o sum.o runtime.o -o example.run
> ./example.run
6
```

The problem with `make example.run` is that the Makefile doesn't know
anything about `example.rkt` requiring `sum.rkt`, so it doesn't know
to make `sum.o` and link it in to the final executable.

I think part of the problem is that the Makefile is playing two roles:

* as a Makefile for building the compiler
* as a utility for running the compiler

Maybe the latter is an abuse of the Makefile.  I'm not sure if we
should do something else, or continue the abuse.  But if it's the
latter, I think we have to do something like this:

https://www.gnu.org/software/make/manual/html_node/Automatic-Prerequisites.html

which seems kinda awful.



