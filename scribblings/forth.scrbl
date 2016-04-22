#lang scribble/manual
@require[racket/include]
@require[scribble/eval]

@; TODO can we use the REPL in examples?
@(define forth-eval (make-base-eval '(require forth/private/command)))

@title[#:tag "top"]{Forth}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[forth]

@hyperlink["https://en.wikipedia.org/wiki/Forth_(programming_language)"]{Forth} is a stack-based calculator language.
This package implements a subset of Forth as a Racket @tt{#lang}.

@section{Install}

To install from the package server, run:
@racketblock[
  raco pkg install forth
]

To install from source, run:
@racketblock[
  git clone https://github.com/bennn/forth
  raco pkg install ./forth
]


@section{Usage}

Files starting with @tt{#lang forth} are interpreted as a newline-separated sequence of commands.
Alternatively, run @tt{raco forth} to start an interactive session.


@section{Commands}

These commands are the base environment, but the list of commands can grow as a program runs.
Type @tt{help} at any time to see the currently available commands.

@defform[(exit)]{
  End the program or REPL session immediately.
}

@defform[(help maybe-cmd)]{
  If no arguments is given, print the current list of commands.
  Otherwise, print known information about the symbol @racket[maybe-cmd].
}

@defform[(+ - * /)]{
  Pop the top two values from the stack, perform an arithmetic operation, push the result back on the stack.
}

@defform[(drop)]{
  Delete the top item from the stack.
}

@defform[(dup)]{
  Duplicate the top item of the stack.
}

@defform[(over)]{
  Duplicate the top item of the stack, but save the result as the 3rd item on the stack.
}

@defform[(swap)]{
  Switch the positions of the top two items on the stack.  
}

@defform*[((push N) N)]{
  Put the number @racket[N] on the stack.
}

@defform[(show)]{
  Print the current stack.
}

@defform[(: id cmd* ...)]{
  Define a new command with name @racket[id] as the composition of existing commands @racket[cmd* ...].
  Later calls to @racket[id] will execute the commands @racket[cmd* ...] in sequence, and later calls to @racket[help] will display information about @racket[id].
}

@section{Example}

Running this program should produce the list @racket[(4 2)].
Forth programs always return their final stack.
Note that @racket["push 2"] and @racket["2"] have the same effect.

@(racketmod forth

push 2
push 2
+
dup
2
swap
-
swap

: incr 1 +
incr
incr
+
)
