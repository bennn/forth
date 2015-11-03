#lang racket/base

;; Forth commands

(provide
  forth-eval*
  ;; (-> Env Stack Input-Port Stack)
  ;; Reads lines from the input port until exhausted.
  ;; Each line is interpreted as a Forth command and evaluated with
  ;;  the current stack & environment.

  forth-repl
  ;; (-> Env Stack Void)
  ;; Prompts the user for input,
  ;;  evaluates the input Forth expression,
  ;;  updates the current stack and environment,
  ;;  repeats.

  ;; ---
  
  ;; type Env = Listof Command

  env-init
  ;; (-> Env)
  ;; Create an base command environment
)

;; -----------------------------------------------------------------------------

(require
 racket/match
 (only-in racket/string string-join)
 (only-in racket/port with-input-from-string)
)

;; =============================================================================
;; -- Commands

(struct command (
  id ;; : Symbol
  exec ;; : (-> Any (U 'EXIT #f Any))
  descr ;; : String
) #:transparent
  #:property prop:procedure
  (struct-field-index exec))
;; (define-type Command command)

(define CMD* (list
  (command
    'exit
    (lambda (v)
      (if (or (eof-object? v)
              (and (symbol? v)
                   (exit? v))
              (and (list? v)
                   (not (null? v))
                   (exit? (car v))))
          'EXIT
          #f))
    "End the REPL session")
  (command
   'help
   (lambda (v)
     (cond
      [(and (symbol? v) (help? v))
       (show-help)]
      [(and (list? v) (help? (car v)))
       (show-help (cdr v))]
      [else
       #f]))
   "Print help information")
  ;; (command
  ;;  '+
  ;;  )
  ;; (command
  ;;  '-
  ;;  )
  ;; (command
  ;;  '*
  ;;  )
  ;; (command
  ;;  '/
  ;;  )
  ;; (command
  ;;  'dup
  ;;  )
  ;; (command
  ;;  'drop
  ;;  )
  ;; (command
  ;;  'swap
  ;;  )
  ;; (command
  ;;  'over
  ;;  )
  ;; (command
  ;;  'define
  ;;  )
))

(define HELP-STR
  (string-join
    (for/list ([c (in-list CMD*)])
      (format "    ~a : ~a" (command-id c) (command-descr c)))
    "\n"
    #:before-first "Available commands:\n"))

;; (: exit? (-> Symbol Boolean))
(define (exit? sym)
  (memq sym '(exit quit q leave bye)))

(define (find-command sym)
  (for/first ([c (in-list CMD*)]
              #:when (eq? sym (command-id c)))
    c))

(define (help? sym)
  (memq sym '(help ? ??? -help --help h)))

(define (show-help [v #f])
  (match v
    [#f HELP-STR]
    [(or (list (? symbol? s))
         (? symbol? s))
     (define c (find-command s))
     (if c
         (command-descr c)
         (format "Unknown command '~a'" s))]
    [x
     (format "Cannot help with '~a'" x)]))

;; -----------------------------------------------------------------------------

;; (: forth-eval* (-> Env Stack Input-Port Stack))
(define (forth-eval* E S in)
  (for/fold ([e E]
             [s S])
            ([ln (in-lines in)])
    (forth-eval e s (forth-tokenize ln))))

(define (forth-eval E S token*)
  (match (for/or ([c (in-list CMD*)]) (c E S token*))
    ['EXIT
     ;; TODO should 'break' from the outer recursion
     (values E S)]
    [#f
     ;; TODO suggest command
     (printf "Unrecognized command '~a'.\n" token*)
     (values E S)]
    [E+S
     (values (car E+S) (cdr E+S))]))
     
;; (: forth-tokenize (-> String (Listof Any)))
(define (forth-tokenize str)
  (parameterize ([read-case-sensitive #f]) ;; Converts symbols to lowercase
    (with-input-from-string str
      (lambda () (let loop ()
                   (match (read)
                     [(? eof-object?) '()]
                     [val (cons val (loop))]))))))

(define (forth-repl E S)
  (error 'forth-repl "Not implemented!"))

(define (env-init)
  CMD*)

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)
  
  ;; -- exit?
  ;; (check-true* exit?

  ;; -- find-command

  ;; -- help?

  ;; -- show-help
  
  ;; -- forth-eval*

  ;; -- forth-eval

  ;; -- forth-tokenize
  (check-apply* forth-tokenize
   ["hello world" == '(hello world)]
   ["Hello WORLD" == '(hello world)]
  )
   

  ;; -- forth-repl
  
  ;; -- env-init
  (check-not-equal? (env-init) '())

)
