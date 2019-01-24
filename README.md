Forth
=====
[![Build Status](https://travis-ci.org/bennn/forth.svg)](https://travis-ci.org/bennn/forth)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/forth/index.html)

Forth language! Implemented as a Racket `#lang`.


Install
---

From the package server:
```
> raco pkg install forth
```

From source:
```
> git clone https://github.com/bennn/forth
> raco pkg install ./forth
```


Usage
---

All files starting with `#lang forth` are interpreted as a sequence of Forth commands.

```
#lang forth

push 2      ;; add `2` to the stack
push 2
+           ;; add the top two numbers on the stack
dup         ;; duplicate head of stack
2           ;; short for `push 2`
swap        ;; swap order of top 2 stack arguments
-

: incr 1 +  ;; define a new command `incr`
incr
incr
+
```

To open an interactive session, run `raco forth`.


`#lang` Details
---

Running `raco pkg install forth` installs `forth` as a Racket package.
Since the package contains a directory named `lang/` that contains a file `reader.rkt`, the new package can be used as a `#lang`.
Racket uses to reader defined in `reader.rkt` to process programs that begin with `#lang forth`.
(Official docs [here](http://docs.racket-lang.org/guide/language-collection.html))

All `reader.rkt` modules need to provide the functions `read` and `read-syntax`.
These functions must produce a Racket module (docs [here](http://docs.racket-lang.org/guide/hash-lang_reader.html)).

In this project, `read` just calls `read-syntax`, which reads all lines from the file and interprets them as forth commands.
The interpreting happens during compilation and produces a stack.
After evaluating all commands in the module, our `read-syntax` function returns a module; inside the module, we save the stack to a variable.
Doing so makes the stack available inside the Dr. Racket interactions panel.

One more thing: this package's `info.rkt` file defines the `raco forth` command.
 Docs [here](http://docs.racket-lang.org/raco/command.html).)
