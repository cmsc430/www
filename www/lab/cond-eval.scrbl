#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title{Conditional Eval}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e image and universe libraries at the top of your
definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section{Temperature and Composition}

@larger{@bold{Ex 1}}: Define a function @tt{what-temp} that consumes a number
(in degrees Fahrenheit) and produces one of three strings: "cold" for cold
temperatures (say, less than 45), "hot" for hot temperatures (at or greater than
75), and "comfy" for anything in between.

@larger{@bold{Ex 2}}: Some international students (or just anyone tired of the
Imperial system) may prefer to use Celsius for their input temperatures. Define
a function @tt{celsius->fahrenheit} that converts a number from degrees Celsius
to degrees Fahrenheit. Search for the conversion online if you don't know it
off-hand.

@larger{@bold{Ex 3}}: Define a function
@link["https://xkcd.com/526/"]{@tt{what-temp/celsius}} that takes in a number (in
degrees Celsius) and returns the same three strings as in Ex 1 for the
appropriate temperature ranges.


@section{Traffic Control}

Swap @bold{Head} and @bold{Hands}.

You've been hired by UMD's Department of Transportation Services to
build their next generation of traffic lights, which work basically
the same as all the existing traffic lights: they go from red to green
to yellow.  DOTS has specified that traffic lights should display red
for 5 seconds, then green for 3 seconds, then yellow for 2 seconds
(drivers better stay alert!).

Your job is to create a traffic light simulator using the
@racket[animate] function.  Recall that @racket[animate] uses a frame
rate of 28 frames per second.  The end result should look like this
(visual embellishments are fine):

@centered{@image["img/stop-light.gif"]{(Stop-light animation is missing)}}

@larger{@bold{Ex 4}}: Define a function @tt{which-light} that, given a
number between 0 and 280, returns an image of either the red, green,
or yellow light.  Think of the input to this function as the amount of
time passed in a single traffic light cycle, measured in 1/28ths of a
second.  So if given 28, the function should respond with the
appropriate light for being 1 second in to the cycle.  If given 224,
it should produce the light appropriate for 8 seconds in to the cycle.

@larger{@bold{Ex 5}}: Define a function @tt{time->light} that, given
any natural number representing 1/28ths of a second, returns the
appropriate light for that moment in time.  Animate your light by
giving @tt{time->light} to @racket[animate].

@colorize["red"]{Hint}: There may be a helpful, built-in
@link["https://docs.racket-lang.org/htdp-langs/beginner.html#%28def._htdp-beginner._%28%28lib._lang%2Fhtdp-beginner..rkt%29._modulo%29%29"]{numeric
function} to make @tt{time->light} easy to implement with @tt{which-light}.

@larger{@bold{Ex 6}}: Rework your program to use defined constants for
any "magic numbers."  It should be possible to change any of the
following with a single edit to the program: the size of the light,
the length of the light cycle, and the length of time for any of the
light colors within the cycle.
