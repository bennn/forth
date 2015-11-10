#lang racket/base

;; Forth commands

(provide
  forth-eval*
  ;; (-> Input-Port Stack)
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
)

;; -----------------------------------------------------------------------------

(require
 racket/match
 forth/private/stack
 (only-in racket/string string-join)
 (only-in racket/port with-input-from-string)
 (for-syntax racket/base racket/syntax syntax/parse)
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

(define (singleton-list? v)
  (and (list? v)
       (not (null? v))
       (null? (cdr v))))

;; Create a binary operation command.
;; Command is recognized by its identifier,
;;  the identifier is then applied to the top 2 numbers on the stack.
(define-syntax binop-command
  (syntax-parser
   [(_ name:id doc:str)
    #:with name-sym (syntax->datum #'name)
    #'(command 'name-sym
               (lambda (E S v)
                 (and (singleton-list? v)
                      (eq? 'name-sym (car v))
                      (let*-values ([(v1 S1) (stack-pop S)]
                                    [(v2 S2) (stack-pop S1)])
                        (cons E (stack-push S2 (name v1 v2))))))
               doc)]))

(define-syntax stack-command
  (syntax-parser #:literals (quote)
   [(_ (quote cmd:id) doc:str)
    #:with stack-cmd (format-id #'cmd "stack-~a" (syntax->datum #'cmd))
    #'(command 'cmd
               (lambda (E S v)
                 (and (singleton-list? v)
                      (eq? 'cmd (car v))
                      (cons E (stack-cmd S))))
               doc)]))

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
      [(and (list? v) (not (null? v)) (help? (car v)))
       (show-help (cdr v))]
      [else
       #f]))
   "Print help information")
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
       (cons (cons cmd E) S)]
      [_ #f]))
   "Define a new command as a sequence of existing commands")
  (binop-command + "Add the top two numbers on the stack")
  (binop-command - "Subtract the top item of the stack from the second item.")
  (binop-command * "Multiply the top two item on the stack.")
  (binop-command / "Divide the top item of the stack by the second item.")
  (stack-command 'drop "Drop the top item from the stack")
  (stack-command 'dup  "Duplicate the top item of the stack")
  (stack-command 'over "Duplicate the top item of the stack, but place the duplicate in the third position of the stack.")
  (stack-command 'swap "Swap the first two numbers on the stack")
  (command
   'push
   (lambda (E S v)
     (match v
       [`(push ,(? number? n))
        (cons E (stack-push S n))]
       [(? number? n)
        (cons E (stack-push S n))]
       [_ #f]))
   "Push a number onto the stack")
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
(define (forth-eval* in)
  ;; TODO let loop
  (for/fold ([e CMD*]
             [s (stack-init)])
            ([ln (in-lines in)])
    (define token* (forth-tokenize ln))
    (if (null? token*)
        (values e s)
        (forth-eval e s token*))))

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

(define (forth-repl [E CMD*] [S (stack-init)])
  (error 'forth-repl "Not implemented!"))

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

)
