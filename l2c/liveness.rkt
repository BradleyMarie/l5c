;; Northwestern University
;; EECS 322
;; Liveness
;;
;; By Brad Weinberger & Ethan Romba
;; April 15, 2012

#lang plai
(require racket/set)

;;
;; Definitions
;;

(define (op? expr)
  (member expr (list '+= '-= '*= '&= '<<= '>>=)))

(define (cmp? expr)
  (member expr (list '< '<= '= )))

(define (label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define (num? expr)
  (number? expr))

(define (genable? expr)
  (and (not (num? expr)) (not (label? expr))))

(define (gens-set . elements)
  (list->set (filter genable? elements)))

(define caller-save
  (set 'ecx 'edx 'eax 'ebx))

(define args
  (set 'eax 'edx 'ecx))

(define callee-save
  (set 'esi 'edi))

(define result (set 'eax))

;;
;; Gen/Kill
;;

(define (gen-kill-instruction sexpr)
  (match sexpr
    ; Special cases
    ; (call u) ;; call a function
    [`(call ,read)
     (cons (set-union (gens-set read) args) (set-union caller-save result))]
    
    ; (tail-call u) ;; tail call a function
    [`(tail-call ,read)
     (cons (set-union (gens-set read) args callee-save) (set))]

    ; (return)
    [`(return)
     (cons (set-union result callee-save) (set))]
    
    ; Reads once
    ; (eax <- (print t))
    ; (goto label) ;; unconditional jump
    [(or `(eax <- (print ,read))
         `(goto ,read))
     (cons (gens-set read) (set))]
    
    ; Reads twice
    ; (eax <- (allocate t t))
    ; (eax <- (array-error t t))
    ; ((mem x n4) <- s) ;; update memory @ x+n4
    ; (cjump t cmp t label label) ;; conditional jump
    [(or `(eax <- (allocate ,read1 ,read2))
         `(eax <- (array-error ,read1 ,read2))
         `((mem ,read1 ,(? number?)) <- ,read2)
         `(cjump ,read1 ,(? cmp?) ,read2 ,(? symbol?) ,(? symbol?)))
     (cons (gens-set read1 read2) (set))]
    
    ; Reads twice, writes once
    ; (cx <- t cmp t) ;; save result of a comparison;
    [`(,write <- ,read1 ,(? cmp?) ,read2)
     (cons (gens-set read1 read2) (set write))]
    
    ; One destructive read, one regular read, writes once
    ; (x aop= t) ;; update x with an arith op and t.
    ; (x sop= sx) ;; update x with a shifting op and sx.
    [`(,readwrite ,(? op?) ,read)
     (cons (gens-set readwrite read) (set readwrite))]
    
    ; Read once, write once
    ; (x <- (mem x n4)) ; read from memory @ x+n4
    [(or `(,write <- (mem ,read ,(? number?)))
         `(,write <- ,read))
     (cons (gens-set read) (set write))]
    
    [_ (cons (set) (set))]))

(define (gen-kill-function sexpr)
  (map gen-kill-instruction sexpr))

;;
;; In/Out
;;

(define (in-instruction gen-kill-pair out-instruction)
  (set-union (car gen-kill-pair) (set-subtract out-instruction (cdr gen-kill-pair))))

(define (out-instruction in-instruction-list) 
  (if (empty? in-instruction-list) (set) (first in-instruction-list)))

(define (in-function-rec gen-kill-pair-list out-instruction-list output-in-function-list)
  (if (empty? gen-kill-pair-list)
      output-in-function-list
      (in-function-rec (rest gen-kill-pair-list) (rest out-instruction-list) 
                       (append output-in-function-list (list (in-instruction (first gen-kill-pair-list) 
                                                                        (first out-instruction-list)))))))

(define (in-function gen-kill-pair-list out-instruction-list)
  (in-function-rec gen-kill-pair-list out-instruction-list (list)))

(define (out-function-rec in-instruction-list output-out-instruction-list)
  (if (empty? in-instruction-list)
      output-out-instruction-list
      (out-function-rec (rest in-instruction-list)
                        (append output-out-instruction-list (list (out-instruction (rest in-instruction-list)))))))

; Returns in/out list
(define (out-function in-instruction-list)
  (list in-instruction-list
   (out-function-rec in-instruction-list (list))))

(define (liveness-function-rec gen-kill-pair-list old-in-out-list)
  (let ([new-in-out-list (out-function (in-function gen-kill-pair-list (second old-in-out-list)))])
    (if (equal? old-in-out-list new-in-out-list)
        old-in-out-list
        (liveness-function-rec gen-kill-pair-list new-in-out-list))))

(define (liveness-function gen-kill-pair-list)
  (liveness-function-rec
   gen-kill-pair-list
   (list
    (build-list (length gen-kill-pair-list) (lambda (x) (set)))
    (build-list (length gen-kill-pair-list) (lambda (x) (set))))))

;;
;; Perform liveness analysis on the file specified on the command line
;;

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (set->sortedlist set-of-symbols)
  (sort (set->list set-of-symbols) symbol<?))

(define (format-liveness-results liveness-results)
  (list (append (list 'in) (map set->sortedlist (first liveness-results)))
        (append (list 'out) (map set->sortedlist (second liveness-results)))))

(define (perform-liveness-analysis sexpr)
  (display (format-liveness-results (liveness-function (gen-kill-function sexpr)))))

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(perform-liveness-analysis (call-with-input-file filename read))