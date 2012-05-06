;; Northwestern University
;; EECS 322
;; L2c
;;
;; By Brad Weinberger & Ethan Romba
;; April 30, 2012

#lang racket

(define DEVELOPMENT #f)

(require "spill-lib.rkt")
(require "liveness-lib.rkt")
(require "graph-lib.rkt")
(require racket/cmdline)
(require test-engine/racket-tests)

;;
;; Helpers
;;

(define (label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define registers (set 'eax 'ebx 'ecx 'edi 'edx 'esi))
(define (register? sym) (set-member? registers sym))

(define reserved-words (set 'print 'allocate 'array-error 'mem 'return 'goto 'cjump 'tail-call 'call))
(define (reserved-word? sym) (set-member? reserved-words sym))

(define (variable? expr)
  (if (symbol? expr)
      (if (not (reserved-word? expr))
          (match (symbol->string expr)
            [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9]*$") #t]
            [_ #f])
          #f)
      #f))

;;
;; L2 -> L2 Translation
;;

(define (translate-l2-program sexpr)
  (cond
    [(label? sexpr) (string->symbol (string-append ":l2c_" (substring (symbol->string sexpr) 1)))]
    [(variable? sexpr) (string->symbol (string-append "l2c_" (symbol->string sexpr)))]
    [(list? sexpr) (map (lambda (e) (translate-l2-program e)) sexpr)]
    [else sexpr]))

;;
;; L2 -> L1 Translation
;;

(define spill-prefix 'spilled)

(define (get-variable-list sexpr)
  (filter-not register? 
              (remove-duplicates (flatten (append (kills sexpr) (gens sexpr))))))

(define (replace-variables function coloring)
  (if (empty? coloring)
      function
      (let ([variable (first (first coloring))]
            [register (second (first coloring))])
        (replace-variables (replace-list-elements function variable register) (rest coloring)))))

(define (allocate-scratch-space function num-spilled)
  (if (zero? num-spilled)
      function
      (let ([esp-adjustment (list 'esp '-= (* 4 num-spilled))]
            [esp-restore (list 'esp '+= (* 4 num-spilled))])
        (if (label? (first function))
            (list* (first function) esp-adjustment (rest function))
            (append (cons esp-adjustment function) (list esp-restore))))))


(define (compile-function-rec sexpr num-spilled variables)
  (let ([coloring (generate-colored-graph (generate-interference-graph sexpr))])
    (if (false? coloring)
        (if (empty? variables)
            #f
            (compile-function-rec (spill-function sexpr (first variables) (* -4 (+ num-spilled 1)) spill-prefix)
                                  (+ 1 num-spilled)
                                  (rest variables)))
        (allocate-scratch-space (replace-variables sexpr coloring) num-spilled))))

(define (compile-function sexpr)
  (compile-function-rec sexpr 0 (get-variable-list sexpr)))

(define (compile-program program)
  (let ([translated-program (translate-l2-program program)])
    (map compile-function translated-program)))

(let ([filename
       (command-line
        #:args (filename) filename)])
  (display (compile-program (call-with-input-file filename read))))