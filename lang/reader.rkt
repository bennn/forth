#lang racket/base

(provide (rename-out
  [forth-read read]
  [forth-read-syntax read-syntax]))

(require
 (only-in syntax/strip-context strip-context))

;; =============================================================================

(define (forth-read in)
  (syntax->datum (forth-read-syntax #f in)))

(define (forth-read-syntax src-path in)
  (strip-context
   #'(module forth-program racket/base
       (require forth/private/stack forth/private/command)
       (forth-eval* (env-init) (stack-init) in))))

;; =============================================================================

(module+ main
  ;; Maybe, move this to a main.rkt file
  (require xrepl forth/private/stack forth/private/command)
  (forth-repl (env-init) (stack-init))
)
