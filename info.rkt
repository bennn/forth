#lang info

(define collection "forth")
(define deps '("base"))
(define build-deps '(
  "cover"
  "cover-coveralls"
  "scribble-lib"
  "racket-doc"
  "rackunit-lib"
  "rackunit-abbrevs"))
(define pkg-desc "Forth emulator")
(define version "0.1")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/forth.scrbl")))
(define raco-commands '(("forth" (submod forth main) "Open a Forth REPL session" #f)))
