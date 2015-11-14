#lang racket/base

;; Forth commands, parsing and executing.

;; The evaluator keeps two pieces of state:
;; - E : an environment of commands
;; - S : a stack of numbers

(provide
  forth-eval*
  ;; (-> Input-Port Stack)
  ;; Reads lines from the input port until exhausted.
  ;; Each line is interpreted as a Forth command and evaluated with
  ;;  the current stack & environment.

  forth-repl
  ;; (-> Void)
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
 (only-in racket/string string-join string-split)
 (only-in racket/port with-input-from-string)
 (for-syntax racket/base racket/syntax syntax/parse)
)

;; =============================================================================
;; -- Commands

;; A Forth command is implemented as a callable struct.
;; For example:
;;   (define f (lambda arg* ...))
;;   (define c (command 'hello f "A simple command"))
;;   (c 4)
;; Calls the command struct `c` with the argument 4.
;; Normally calling a struct is an error, but since we declared `command`
;;  with the property `prop:procedure`, the call gets redirected to the struct
;;  field `exec`. i.e. `(c 4)` is the same as `(f 4)`.

(struct command (
  id    ;; : Symbol
  exec  ;; : (-> Env State Any (U 'EXIT #f (Pairof Env State)))
  descr ;; : String
) #:transparent
  #:property prop:procedure
  (struct-field-index exec))
;; (define-type Command command)

;; True if the argument is a list with one element
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
                        (cons E (stack-push S2 (name v2 v1))))))
               doc)]))

;; Turns a symbol into a stack command parser
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

;; Default environment of commands
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
       (displayln (show-help E))
       (cons E S)]
      [(and (list? v) (not (null? v)) (help? (car v)))
       (displayln (show-help E (and (not (null? (cdr v))) (cdr v))))
       (cons E S)]
      [else
       #f]))
   "Print help information")
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
       [`(,(? number? n))
        (cons E (stack-push S n))]
       [_ #f]))
   "Push a number onto the stack")
  (command
   'show
   (lambda (E S v)
     (match v
       [`(,(? show?))
        (displayln S)
        (cons E S)]
       [_ #f]))
   "Print the current stack")
  (command
   'define
   (lambda (E S v)
     (match v
      [(cons (or ': 'define) (cons w defn*))
       (define cmd
         (command w
                  (lambda (E S v)
                    (if (equal? v (list w))
                        (let-values ([(e+ s+)
                                      (for/fold ([e E] [s S])
                                          ([d (in-list defn*)])
                                        (forth-eval e s (list d)))])
                          (cons e+ s+))
                        #f))
                  (format "~a" defn*)))
       (cons (cons cmd E) S)]
      [_ #f]))
   "Define a new command as a sequence of existing commands")
))

;; (: exit? (-> Any Boolean))
(define (exit? sym)
  (memq sym '(exit quit q leave bye)))

;; Search the environment for a command with `id` equal to `sym`
(define (find-command E sym)
  (for/first ([c (in-list E)]
              #:when (eq? sym (command-id c)))
    c))

;; (: help? (-> Any Boolean))
(define (help? sym)
  (memq sym '(help ? ??? -help --help h)))

;; (: show? (-> Any Boolean))
(define (show? sym)
  (memq sym '(show print pp ls stack)))

;; Print a help message.
;; If the optional argument is given, try to print information about it.
(define (show-help E [v #f])
  (match v
    [#f
     (string-join
      (for/list ([c (in-list E)])
        (format "    ~a : ~a" (command-id c) (command-descr c)))
      "\n"
      #:before-first "Available commands:\n")]
    [(or (list (? symbol? s))
         (? symbol? s))
     (define c (find-command E s))
     (if c
         (command-descr c)
         (format "Unknown command '~a'" s))]
    [x
     (format "Cannot help with '~a'" x)]))

;; -----------------------------------------------------------------------------

;; The machinery for running commands.
;; Parses strings & ports to S-expressions,
;;  and feeds those expressions to the structs in the command environment.

;; (: forth-eval* (-> Input-Port Stack))
(define (forth-eval* in)
  (for/fold ([e CMD*]
             [s (stack-init)])
      ([ln (in-lines in)])
    (define token* (forth-tokenize ln))
    (cond
     [(or (null? token*)
          (not (list? e))) ;; Cheap way to detect EXIT
      (values e s)]
     [else
      (forth-eval e s token*)])))

;; (: forth-repl (->* [] [Env Stack] Void))
(define (forth-repl [E CMD*] [S (stack-init)])
  (display "forth> ")
  (define token* (forth-tokenize (read-line)))
  (if (null? token*)
      (displayln S)
      (let-values ([(E+ S+) (forth-eval E S token*)])
        (if (not (list? E+))
            (displayln S+)
            (forth-repl E+ S+)))))

;; (: forth-eval (-> Env Stack (Listof Any) (Values (U Env #f) Stack)))
(define (forth-eval E S token*)
  (match (for/or ([c (in-list E)]) (c E S token*))
    ['EXIT
     (values #f S)]
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
      (lambda () 
        (de-nest
         (let loop ()
           (match (read)
             [(? eof-object?) '()]
             [val (cons val (loop))])))))))

;; Remove all parentheses around a singleton list
(define (de-nest v*)
  (if (and (list? v*)
           (not (null? v*))
           (list? (car v*))
           (null? (cdr v*)))
      (de-nest (car v*))
      v*))

;; =============================================================================

;; Unit tests. Run these with:
;;   raco test command.rkt

(module+ test
  (require
   rackunit
   rackunit-abbrevs
   (only-in racket/format ~a))

  ;; -- exit?
  (check-true* (lambda (x) (if (exit? x) #t #f))
   ['exit]
   ['quit]
   ['q])

  (check-false* exit?
   ['()]
   [#f]
   [53]
   ['hello])
 
  ;; -- find-command
  (check-true* (lambda (sym) (eq? sym (command-id (find-command CMD* sym))))
   ['exit]
   ['dup]
   ['+])

  (check-false* (lambda (sym) (if (find-command CMD* sym) #t #f))
   ['hi]
   ['hi]
   ["yes"]
   [00])

  ;; -- help?
  (check-true* (lambda (v) (if (help? v) #t #f))
   ['help]
   ['?]
   ['--help])

  (check-false* help?
   ['exit]
   [#f]
   ['q]
   [21])

  ;; -- show?
  (check-true* (lambda (v) (if (show? v) #t #f))
   ['show]
   ['ls]
   ['print])

  (check-false* help?
   ['exit]
   [#f]
   ['q]
   [12])

  ;; -- show-help
  (check-equal? (length (string-split (show-help CMD*) "\n")) (+ 1 (length CMD*)))
  (check-equal? (length (string-split (show-help CMD* #f) "\n")) (+ 1 (length CMD*)))
  (check-regexp-match #rx"^Cannot help" (show-help CMD* "booo"))
  (check-regexp-match #rx"^Unknown command" (show-help CMD* 'booo))
  (check-regexp-match #rx"^Print help" (show-help CMD* 'help))

  ;; -- forth-eval*
  (let* ([eval* (lambda (v*)
                  (with-input-from-string (string-join (map ~a v*) "\n")
                    (lambda () (forth-eval* (current-input-port)))))]
         [eval/stack (lambda (v*) (let-values ([(e s) (eval* v*)]) s))])
    (check-apply* eval/stack
     ['(1 2 3)
      == '(3 2 1)]
     ['(1 1 +)
      == '(2)]
     ['(2 1 -)
      == '(1)]
     ['(8 8 8 * *)
      == '(512)]
     ['(2 1 3 /)
      == '(1/3 2)]
     ['(1 0 EXIT /)
      == '(0 1)]
     ['(": dup3 dup dup dup" 1 2 dup3)
      == '(2 2 2 2 1)]
     ['(1 2 drop)
      == '(1)]
     ['(1 2 3 over)
      == '(3 2 3 1)]
     ['(1 0 swap)
      == '(1 0)]
     ['(": switcheroo swap swap" 5 6 switcheroo switcheroo)
      == '(6 5)]
     ['("push 1" "push 2" +)
      == '(3)]))

  ;; -- forth-eval
  (let* ([S '(2 4 8)]
         [E CMD*]
         [eval/stack (lambda (token*)
                       (let-values ([(e s) (forth-eval E S token*)]) s))])
  (check-apply* eval/stack
   [#f
    == S]
   ['nada
    == S]
   ['(exit)
    == S]
   ['(help)
    == S]
   ['(: hi 3 2 1)
    == S]
   ['(+)
    == '(6 8)]
   ['(-)
    == '(2 8)]
   ['(*)
    == '(8 8)]
   ['(/)
    == '(2 8)]
   ['(drop)
    == (stack-drop S)]
   ['(dup)
    == (stack-dup S)]
   ['(over)
    == (stack-over S)]
   ['(swap)
    == (stack-swap S)]
   ['(1)
    == (stack-push S 1)]
   ['(push 8)
    == (stack-push S 8)]
   ['(show)
    == S]))

  (let* ([S '(6 6 6)]
         [E CMD*]
         [L (length E)]
         [eval/env-length (lambda (token*)
                     (let-values ([(e s) (forth-eval E S token*)]) (length e)))])
    (check-apply* eval/env-length
     ['(2)
      == L]
     ['(: new dup drop swap)
      == (+ 1 L)]
     ['(swap)
      == L]))

  ;; -- forth-tokenize
  (check-apply* forth-tokenize
   ["hello world" == '(hello world)]
   ["Hello WORLD" == '(hello world)]
   ["(((Hello WORLD)))" == '(hello world)]
   [": key val val val;" == '(: key val val val)]
   ["1 2 3" == '(1 2 3)])

  ;; -- de-nest
  (check-apply* de-nest
   ['a == 'a]
   ['(a) == '(a)]
   ['(((a))) == '(a)]
   ['(((a)) b) == '(((a)) b)])

)
