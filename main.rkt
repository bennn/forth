#lang racket/base

(module+ main
  (require xrepl)
  (require forth/private/stack forth/private/command)
  (forth-repl))
