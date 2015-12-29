#lang info

(define collection "forth")
(define deps '("base"))
(define build-deps '(
  "scribble-lib"
  "racket-doc"
  "rackunit-lib"
  "rackunit-abbrevs"))
(define pkg-desc "Forth emulator")
(define version "0.1")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/forth.scrbl" () (omit-start))))
(define raco-commands '(("forth" (submod forth main) "Open a Forth REPL session" #f)))
