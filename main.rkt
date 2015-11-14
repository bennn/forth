#lang racket/base

;; Running this file with:
;;   `racket main.rkt`
;; opens a forth repl session.

(module+ main
  (require forth/private/stack forth/private/command)
  (forth-repl))
