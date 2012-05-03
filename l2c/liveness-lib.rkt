;; Northwestern University
;; EECS 322
;; Liveness
;;
;; By Brad Weinberger & Ethan Romba
;; April 15, 2012

#lang racket
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

(define x86-caller-save
  (set 'ecx 'edx 'eax))

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
    
    ; (eax <- (allocate t t))
    ; (eax <- (array-error t t))
    [(or `(,write <- (allocate ,read1 ,read2))
         `(,write <- (array-error ,read1 ,read2)))
     (cons (gens-set read1 read2) (set-union (set write) x86-caller-save))]
    
    ; (eax <- (print t))
    [`(,write <- (print ,read))
     (cons (gens-set read) (set-union (set write) x86-caller-save))]
    
    ; Reads once
    ; (goto label) ;; unconditional jump
    [`(goto ,read)
     (cons (gens-set read) (set))]
    
    ; Reads twice
    ; ((mem x n4) <- s) ;; update memory @ x+n4
    ; (cjump t cmp t label label) ;; conditional jump
    [(or `((mem ,read1 ,(? number?)) <- ,read2)
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
    ; (x <- x) ; simple assignment
    [(or `(,write <- (mem ,read ,(? number?)))
         `(,write <- ,read))
     (cons (gens-set read) (set write))]
    
    
    [_ (cons (set) (set))]))

(define (gen-kill-function sexpr)
  (map gen-kill-instruction sexpr))

;;
;; Successor 
;;

(define (make-label-hash-map list-of-instructions list-of-instruction-indices)
  (make-hash (filter (lambda (numbered-instruction)
                       (label? (car numbered-instruction)))
                     (map (lambda (instruction index) (cons instruction index))
                          list-of-instructions list-of-instruction-indices))))

(define (successor-instruction sexpr index label-index-map)
  (match sexpr
    ; No Successors
    ; (return)
    ; (tail-call s)
    [(or `(return)
         `(tail-call ,(? symbol?)))
     (set)]
    
    ; (eax <- (array-error t t))
    [`(eax <- (array-error ,t1 ,t2))
     (set)]
    
    ; One Successor
    ; (goto label)
    [`(goto ,label)
     (set (hash-ref label-index-map label))]
    
    ; Two Successors
    ; (cjump t cmp t label label)
    [`(cjump ,lhs ,(? cmp?) ,rhs ,true-label ,false-label)
     (set (hash-ref label-index-map true-label) (hash-ref label-index-map false-label))]
    
    [_ (set (+ 1 index))]))

(define (successors-function list-of-instructions)
  (let* ([list-of-instruction-indices (build-list (length list-of-instructions) (lambda (x) x))]
         [label-index-map (make-label-hash-map list-of-instructions list-of-instruction-indices)])
    (map (lambda (instruction index) (successor-instruction instruction index label-index-map))
         list-of-instructions list-of-instruction-indices)))

;;
;; In generation code
;;

(define (in-instruction gen-kill-pair out-instruction)
  (set-union (car gen-kill-pair) (set-subtract out-instruction (cdr gen-kill-pair))))

(define (in-function-rec gen-kill-pair-list out-instruction-list output-in-function-vector)
  (if (empty? gen-kill-pair-list)
      output-in-function-vector
      (in-function-rec (rest gen-kill-pair-list) (rest out-instruction-list) 
                       (vector-append output-in-function-vector (vector (in-instruction (first gen-kill-pair-list) 
                                                                                      (first out-instruction-list)))))))

(define (in-function gen-kill-pair-list out-instruction-list)
  (in-function-rec gen-kill-pair-list out-instruction-list (vector)))

;;
;; Out generation code
;;

(define (out-instruction in-instruction-vector successor-instruction-set)
  (if (or (set-empty? successor-instruction-set)
          (equal? (set (vector-length in-instruction-vector)) successor-instruction-set))
      (set)
      (foldl (lambda (successor result) 
               (set-union result (vector-ref in-instruction-vector successor)))
             (set)
             (set->list successor-instruction-set))))

(define (out-function-rec in-instruction-vector list-of-successors output-out-instruction-list)
  (if (empty? list-of-successors)
      output-out-instruction-list
      (out-function-rec in-instruction-vector (rest list-of-successors)
                        (append output-out-instruction-list 
                                (list (out-instruction in-instruction-vector (first list-of-successors)))))))

(define (out-function in-instruction-vector list-of-successors)
  (out-function-rec in-instruction-vector list-of-successors (list)))

;;
;; Perform liveness analysis
;;

(define (liveness-function-rec gen-kill-pair-list successor-list old-in-out-list)
  (let* ([new-in-list (in-function gen-kill-pair-list (second old-in-out-list))]
         [new-out-list (out-function new-in-list successor-list)]
         [new-in-out-list (list new-in-list new-out-list)])
    (if (equal? old-in-out-list new-in-out-list)
        old-in-out-list
        (liveness-function-rec gen-kill-pair-list successor-list new-in-out-list))))

(define (liveness-function sexpr)
  (liveness-function-rec
   (gen-kill-function sexpr)
   (successors-function sexpr)
   (list
    (build-list (length sexpr) (lambda (x) (set)))
    (build-list (length sexpr) (lambda (x) (set))))))

;;
;; Output formatting Code
;;

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (set->sortedlist set-of-symbols)
  (sort (set->list set-of-symbols) symbol<?))

(define (format-liveness-results liveness-results)
  (list (append (list 'in) (vector->list (vector-map set->sortedlist (first liveness-results))))
        (append (list 'out) (map set->sortedlist (second liveness-results)))))

(define (liveness-analysis sexpr)
  (format-liveness-results (liveness-function sexpr)))

(provide liveness-analysis)

(define (kills sexpr)
  (map (lambda (element)
         (set->list (cdr element)))
       (gen-kill-function sexpr)))

(provide kills)

(define (gens sexpr)
    (map (lambda (element)
         (set->list (car element)))
       (gen-kill-function sexpr)))

(provide gens)