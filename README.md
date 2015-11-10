Forth
=====
[![Build Status](https://travis-ci.org/bennn/forth.svg)](https://travis-ci.org/bennn/forth)
[![Coverage Status](https://coveralls.io/repos/bennn/forth/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/forth?branch=master)

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

push 2
push 2
+
dup
2
swap
-
swap
```

To open an interactive session, run `raco forth`.

