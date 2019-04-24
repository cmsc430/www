#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt" "../utils.rkt")

@title{Getting Started}

FIXME: remove head and hands, use exercise format.

@section{Introduction(s)}

You'll work in labs and on problem sets in pairs, and we've randomly assigned
your partner for this first lab (and this week's problem set). Find your partner
and introduce yourself. If your partner is @bold{not} present, let one of your
TAs know.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You both should install DrRacket, but only one instance should be use during the
lab. At the end you'll submit the lab as a pair via the
@link["https://submit.cs.umd.edu"]{UMD CS Submit Server} so we can keep an eye
on lab attendance.


@section{Install DrRacket}

Download and install DrRacket, part of the Racket platform, available
for download from
@link["https://download.racket-lang.org/"]{https://download.racket-lang.org/}.

@panopto-vid["https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=fca012d7-29a3-4b30-af98-a9ad01537e17"]

@section{Meet DrRacket}

You should already have DrRacket installed on your computer, but now
is the chance to get help (or catch up) if you don't yet.  DrRacket is
the program you'll use to design and run your programs.
@link["https://download.racket-lang.org"]{Download}, install, and run
DrRacket to get started.

Help your partner install it on their machine if you're done first. Then, pick
the first @bold{Head} and @bold{Hands} and get continue on only one machine.

Explore DrRacket's interface. First, find out how to set the current
@link["https://docs.racket-lang.org/drracket/choose-language.html"]{Language} to
the @link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning
Student Language} (BSL). We'll tell you which language to use at the beginning
of each lab and problem set.

(Note: if you change the language, you'll have to hit the <Run> button for it to
take effect.)

Next, look through the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{documentation for
the BSL}. (Hint: also check the Help menu.) You should get comfortable searching
and reading the documentation for anything you need to know about the language
and its libraries.

Locate the @italic{definitions window} and the @italic{interactions window}.

The @italic{interactions window} lets you quickly make simple calculations. You
can type in some expression and hit <Enter> to run it. Test out a few
expressions, try to do some arithmetic with big numbers and fractions (or
anything else you want to test out).

The @italic{definitions window} is where you'll define and develop programs. You
execute the code in the @italic{definitions window} by hitting the <Run> button.

There's a whole bunch more to explore, but that's all you need for now. Feel
free to try out the Stepper or other features as you program.

If something you type results in @colorize["red"]{red text} being printed in the
@italic{interactions window}, the Dr is telling you something unexpected
happened. Errors happen a lot, and are a @colorize["blue"]{GoodThing™}! The Dr
is giving you the very best help that it can. Read the message! Figure out what
went wrong. Try to fix the problem.


@section{Finger exercises}

@larger{@bold{Ex 1}}: Define a function @tt{how-long-at-60-mph} that, when given
a number that represents distance in miles, will return the time (in hours) it
takes to travel that distance when going 60 MPH. Write your function in the
@italic{definitions window}, click Run, then use the @italic{interactions
window} to test your function.

@larger{@bold{Ex 2}}: Define a function @tt{how-far-at-70-mph} that, when given
a number representing time in minutes, will return the distance traveled when
going 70 MPH, rounding up to whole miles.

@bold{@colorize["red"]{Hint}}: If you don't know how to perform some particular
calculation, search the docs! If you can't find it in the docs, ask a TA (and
show them what you searched for in the docs).

@larger{@bold{Ex 3}}: Swap @bold{Head} and @bold{Hands}!

When you program, you encounter three kinds of errors:
@itemlist[
  @item{syntax errors, meaning what you wrote is not a BSL expression;}
  @item{run-time errors, that is, you wrote a BSL expression but when you
        interact with it, DrRacket signals an error (because a function is
        applied to too many or too few arguments, the wrong kinds of
        arguments, and so on); and}
  @item{logical errors, which do not manifest themselves as some red text in
        the interactions area. Instead, you apply a function and it gives you
        the wrong value back as a result.}
]

Define three variants of the function from @bold{Ex 2}:
@tt{how-far-at-70-mph/{syntax,run-time,logical}}, each of which demonstrates a
different kind of error. Comment out the functions once you're done.

@larger{@bold{Ex 4}}: Add the following expression at the top of your definitions window:
@racketblock[(require 2htdp/image)]

The @tt{require} form lets you use external library definitions in your
code. Hit <Run> after adding the @tt{require} to load the image library.

Find a picture of your favorite animal on the internet (talking to you,
@bold{Head}). Copy and paste it into DrRacket's @italic{definitions window} and
give it a name (like Chip!).

Draw a frame with a green border around your picture of Chip. As always, the
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{docs} can help
if you don't know what functions to use to, for example, make a
@racket[rectangle] or @racket[place-image]s on one another.

@larger{@bold{Ex 5}}: Define a function @tt{in-a-frame} that given a string
color (like "red", "blue", "green") places Chip on a frame of that
color.


@;{
@section[#:tag "lab1:submit"]{Submit as a pair}

Save your program as @tt{lab1.rkt}.  Using a web browser, submit this
file on @link["https://submit.cs.umd.edu"]{submit.cs.umd.edu} (you will need to use your university
Directory ID to log in).  Only one partner needs to submit and should
select the name of their partner.

If you have trouble submitting through the submit server, be sure to
talk with TAs.  You will follow the same process for submitting
assignment 1.  We recommend you use lab time to submit assignment 1 if
you can.
}