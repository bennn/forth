#lang racket/base

(module+ main
  ;; Maybe, move this to a main.rkt file
  (require xrepl forth/private/stack forth/private/command)
  (forth-repl)
) 
