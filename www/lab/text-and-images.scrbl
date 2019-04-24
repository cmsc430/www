#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title{Text and Images}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e image library at the top of your definitions:
@racketblock[(require 2htdp/image)]

@colorize["red"]{Hint}: You can search for documentation by right-clicking any
identifier @tt{foo} in the @italic{definitions} or @italic{interactions} windows
and choosing 'Search in Help Desk for "foo"' menu option. You can also press
<F1> to search for documentation about any identifier under the cursor.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section{Strings and Text}

Your TAs hope to break into the lucrative & high-stakes world of banner making
and advertising. Unrelated, the theme of today's lab is working with strings,
text, and images.

@larger{@bold{Ex 1}}: Define a function @tt{greet-tracked-person} that, given the
string name of some person and a string slogan, returns a string with a
personalized slogan such as:

@itemlist[
  @item{@verbatim{(greet-tracked-person "Jerry" "Hungry for Milk")
; => "Hungry for Milk, Jerry"}}
  @item{@verbatim{(greet-tracked-person "Michele" "Think Dissimilar")
; => "Think Dissimilar, Michele"}}
  @item{@verbatim{(greet-tracked-person "Patrick" "Can You Hear Me Presently? Good")
; => "Can You Hear Me Presently? Good, Patrick"}}
]

You may find the function @racket[string-append] useful. Test your function with
@racket[check-expect].

@larger{@bold{Ex 2}}: Strings and images are different types of values and we
want both in our advertisements. Write a function @tt{big-and-red} that, given
some string @tt{str}, returns an image of @tt{str} colored red and in a 64pt
font of your choice.

@colorize["red"]{Hint}: When you need to do something but don't know how, look
at the signatures of functions in
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{the
documentation}. If you need to convert a string into an image, don't just search
for @tt{string->image} and give up if you see no results, look for a function
that takes in a string a returns an image. Test it out, and if it's not what you
need, keep looking!

@larger{@bold{@colorize["red"]{Bonus}}}: Some slogans are just too long, so
define a new function @tt{no-more-than-32} that, given a string, returns that
string with no more than the first 32 characters. One of the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html#%28part._htdp-beginner._.Strings%29"]{BSL
string operations} provides a simple way to do this for strings at least 32
characters long, but may not work for shorter strings. A clever use of
@racket[min] and @racket[string-length] will let you solve this without
resorting to @tt{cond}, which we'll use in the future.


@section{Using Text and Images}

Swap @bold{Head} and @bold{Hands}.

@larger{@bold{Ex 3}}: Logos and slogans go hand-in-hand, so define a function
@tt{logo-and-slogan} that, given an image @tt{logo} and a string @tt{slogan},
returns an image with the logo to the left of the text of the slogan (try to
make it large and in an obnoxious font). Copy your favorite logos from the
internet into your definitions window to test your implementation.

@larger{@bold{Ex 4}}: Some companies find that people accidentally overlook even
their best advertisements. Define a function @tt{four-must-be-better} that,
given an image, stacks four copies of that image vertically so it can't be
missed.

@larger{@bold{Ex 5}}: We find the close button in our prototype is too easy to
click, so we want to change the size of our advertisements dynamically so the
close button is harder to click. Define a function @tt{on-background-of-width}
that, given an image and some positive integer @tt{width}, places the image on a
background rectangle of any color that is 200 pixels high and @tt{width} pixels
wide.

@larger{@bold{@colorize["red"]{Bonus}}}: Using the functions you've defined,
define a function @tt{animation} that, given a non-negative integer @tt{width},
returns an image of your favorite logo and slogan on a background 200 pixels
high and @tt{width} pixels wide. Require the
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{@tt{2htdp/universe}}
library, and use the function @racket[animate] to demo the growing
advertisement.
