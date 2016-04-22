Forth
=====
[![Build Status](https://travis-ci.org/bennn/forth.svg)](https://travis-ci.org/bennn/forth)
[![Coverage Status](https://coveralls.io/repos/bennn/forth/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/forth?branch=master)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/forth/index.html)

Forth emulator! Implemented as a Racket `#lang`.


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

The call to `raco` installs `forth` as a new language.
While installing, `raco` sees the directory `forth/lang` the file `reader.rkt` inside.
These names (`forth`, `lang`, and `reader`) direct `raco` to interpret all programs starting with `#lang forth` using the functions from `reader.rkt`.
(Official docs [here](http://docs.racket-lang.org/guide/language-collection.html))

All `reader.rkt` files need to provide the functions `read` and `read-syntax`.
These functions must produce a Racket module (docs [here](http://docs.racket-lang.org/guide/hash-lang_reader.html)).

In this project, `read` just calls `read-syntax`, which reads all lines from the file and interprets them as forth commands.
The interpreting happens during compilation and produces a stack.
After evaluating all commands in the module, our `read-syntax` function returns a module; inside the module, we save the stack to a variable.
Doing so makes the stack available inside the Dr. Racket interactions panel.

(One more thing: `raco forth` works because we defined `raco-commands` inside `info.rkt`.
 Docs [here](http://docs.racket-lang.org/raco/command.html).)
