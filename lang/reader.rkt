#lang racket/base

(provide (rename-out
  [forth-read read]
  [forth-read-syntax read-syntax]))

(require
 forth/private/command
 (only-in syntax/strip-context strip-context))

;; =============================================================================

(define (forth-read in)
  (syntax->datum (forth-read-syntax #f in)))

(define (forth-read-syntax src-path in)
  ;; Beware, environment would be 3D syntax
  (let-values ([(E S) (forth-eval* in)])
    (strip-context
     #`(module forth-program racket/base
         (require forth/private/stack forth/private/command)
         ;; TODO save envionrment/stack?
         (define stack '#,S)
         stack))))

;; =============================================================================

(module+ main
  ;; Maybe, move this to a main.rkt file
  (require xrepl forth/private/stack forth/private/command)
  (forth-repl)
)
