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
 forth/private/stack
 (only-in racket/string string-join)
 (only-in racket/port with-input-from-string)
)

;; =============================================================================
;; -- Commands

(struct command (
  id ;; : Symbol
  exec ;; : (-> Env State Any (U 'EXIT #f (Pairof Env State)))
  descr ;; : String
) #:transparent
  #:property prop:procedure
  (struct-field-index exec))
;; (define-type Command command)

(define CMD* (list
  (command
    'exit
    (lambda (E S v)
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
   (lambda (E S v)
     (cond
      [(and (symbol? v) (help? v))
       (show-help)]
      [(and (list? v) (help? (car v)))
       (show-help (cdr v))]
      [else
       #f]))
   "Print help information")
  (command
   '+
   (lambda (E S v)
     (match v
      ['(+)
       (let*-values ([(v1 S1) (stack-pop S)]
                     [(v2 S2) (stack-pop S1)])
         (values E (stack-push S2 (+ v1 v2))))]
      [_ #f]))
   "Add the top two numbers on the stack.")
  (command
   '-
   (lambda (E S v)
     (match v
      ['(-)
       (let*-values ([(v1 S1) (stack-pop S)]
                     [(v2 S2) (stack-pop S1)])
         (values E (stack-push S2 (- v1 v2))))]
      [_ #f]))
   "Subtract the top item of the stack from the second item.")
  (command
   '*
   (lambda (E S v)
     (match v
      ['(*)
       (let*-values ([(v1 S1) (stack-pop S)]
                     [(v2 S2) (stack-pop S1)])
         (values E (stack-push S2 (* v1 v2))))]
      [_ #f]))
   "Multiply the top two item on the stack.")
  (command
   '/
   (lambda (E S v)
     (match v
      ['(/)
       (let*-values ([(v1 S1) (stack-pop S)]
                     [(v2 S2) (stack-pop S1)])
         (values E (stack-push S2 (/ v1 v2))))]
      [_ #f]))
   "Divide the top item of the stack by the second item.")
  (command
   'define
   (lambda (E S v)
     (match v
      [(cons ': (cons w defn*))
       (define cmd
         (command w
                  (lambda (E S v)
                    (if (equal? v (list w))
                        (for/fold ([e E] [s S])
                                  ([d (in-list defn*)])
                            (forth-eval e s (list d)))
                        #f))
                  (format "~a" defn*)))
       (values (cons cmd E) S)]
      [_ #f]))
   "Define a new command as a sequence of existing commands")
  (command
   'drop
   (lambda (E S v)
     (match v
       ['(drop)
        (values E (stack-drop S))]
       [_ #f]))
   "Drop the top item from the stack")
  (command
   'dup
   (lambda (E S v)
     (match v
       ['(dup)
        (values E (stack-dup S))]
       [_ #f]))
     "Duplicate the top item of the stack")
  (command
   'over
   (lambda (E S v)
     (match v
       ['(over)
        (values E (stack-over S))]
       [_ #f]))
   "Duplicate the top item of the stack, but place the duplicate in the third position of the stack.")
  (command
   'push
   (lambda (E S v)
     (match v
       [`(push ,(? number? n))
        (values E (stack-push S v))]
       [_ #f]))
   "Push a number onto the stack")
  (command
   'swap
   (lambda (E S v)
     (match v
       ['(swap)
        (values E (stack-swap S))]
       [_ #f]))
   "Swap the first two numbers on the stack")
))

;; (: exit? (-> Symbol Boolean))
(define (exit? sym)
  (memq sym '(exit quit q leave bye)))

(define (find-command sym)
  (for/first ([c (in-list CMD*)]
              #:when (eq? sym (command-id c)))
    c))

(define (help? sym)
  (memq sym '(help ? ??? -help --help h)))

(define (show-help E [v #f])
  (define HELP-STR
    (string-join
     (for/list ([c (in-list E)])
       (format "    ~a : ~a" (command-id c) (command-descr c)))
     "\n"
     #:before-first "Available commands:\n"))
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
  (match (for/or ([c (in-list E)]) (c E S token*))
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
