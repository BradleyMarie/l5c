;; Northwestern University
;; EECS 322
;; Spill
;;
;; By Brad Weinberger & Ethan Romba
;; April 15, 2012

#lang racket

;;
;; Helper Functions
;;

(define (op? expr)
  (member expr (list '+= '-= '*= '&= '<<= '>>=)))

(define (cmp? expr)
  (member expr (list '< '<= '= )))

(define (replace-list-elements list-of-symbols find-value replace-value)
  (cond
    [(eq? list-of-symbols find-value) replace-value]
    [(list? list-of-symbols) (map (lambda (e) (replace-list-elements e find-value replace-value)) list-of-symbols)]
    [else list-of-symbols]))

(define (read-memory variable offset)
  (list variable '<- (list 'mem 'ebp offset)))

(define (write-memory variable offset)
  (list (list 'mem 'ebp offset) '<- variable))

(define prefix-index -1)  ;; Number
(define prefix (void))       ;; String
(define offset (void))      ;; Number
(define old-variable (void))  ;; Symbol

(define (spill-variable)
  (begin
    (set! prefix-index (+ prefix-index 1))
    (string->symbol (string-append prefix (number->string prefix-index)))))

(define (spill-read original-expression)
  (let ([new-variable (spill-variable)])
    (list
      (read-memory new-variable offset)
      (replace-list-elements original-expression old-variable new-variable))))

(define (spill-write original-expression)
  (let ([new-variable (spill-variable)])
    (list
      (replace-list-elements original-expression old-variable new-variable)
      (write-memory new-variable offset))))

(define (spill-read-write original-expression)
  (let ([new-variable (spill-variable)])
    (list
      (read-memory new-variable offset)
      (replace-list-elements original-expression old-variable new-variable)
      (write-memory new-variable offset))))

(define (write-to-x dest)
  (list (list dest '<- (list 'mem 'ebp offset))))

(define (read-from-s source)
  (list (list (list 'mem 'ebp offset) '<- source)))

;;
;; Parser
;;

(define (spill-instruction sexpr)
  (match sexpr
    ; Reads once
    ; (eax <- (print t))
    ; (goto label) ;; unconditional jump
    ; (call u) ;; call a function
    ; (tail-call u) ;; tail call a function
    [(or `(eax <- (print ,read))
         `(goto ,read)
         `(call ,read)
         `(tail-call ,read))
     (if (eq? read old-variable) (spill-read sexpr) (list sexpr))]
    
    ; Reads twice
    ; (eax <- (allocate t t))
    ; (eax <- (array-error t t))
    ; ((mem x n4) <- s) ;; update memory @ x+n4
    ; (cjump t cmp t label label) ;; conditional jump
    [(or `(eax <- (allocate ,read1 ,read2))
         `(eax <- (array-error ,read1 ,read2))
         `((mem ,read1 ,(? number?)) <- ,read2)
         `(cjump ,read1 ,(? cmp?) ,read2 ,(? symbol?) ,(? symbol?)))
     (if (or (eq? read1 old-variable) (eq? read2 old-variable))
         (spill-read sexpr) (list sexpr))]
    
    ; Reads twice, writes once
    ; (cx <- t cmp t) ;; save result of a comparison;
    [`(,write <- ,read1 ,(? cmp?) ,read2)
     (cond
       [(and (eq? write old-variable) (or (eq? read1 old-variable) (eq? read2 old-variable)))
        (spill-read-write sexpr)]
       [(or (eq? read1 old-variable) (eq? read2 old-variable))
        (spill-read sexpr)]
       [(eq? write old-variable)
        (spill-write sexpr)]
       [else (list sexpr)])]
    
    ; One destructive read, one regular read, writes once
    ; (x aop= t) ;; update x with an arith op and t.
    ; (x sop= sx) ;; update x with a shifting op and sx.
    [`(,readwrite ,(? op?) ,read)
     (cond
       [(eq? readwrite old-variable) (spill-read-write sexpr)]
       [(eq? read old-variable) (spill-read sexpr)]
       [else (list sexpr)])]
    
    ; Read once, write once
    ; (x <- (mem x n4)) ; read from memory @ x+n4
    [`(,write <- (mem ,read ,(? number?)))
     (cond
       [(and (eq? write old-variable) (eq? read old-variable)) (spill-read-write sexpr)]
       [(eq? write old-variable) (spill-write sexpr)]
       [(eq? read old-variable) (spill-read sexpr)]
       [else (list sexpr)])]
    
    ; (x <- s) ;; assign to a register
    [`(,write <- ,read)
     (cond
       [(and (eq? read old-variable) (eq? write old-variable)) (list)]
       [(eq? read old-variable) (write-to-x write)]
       [(eq? write old-variable) (read-from-s read)]
       [else (list sexpr)])]
    
    [_ (list sexpr)]))

(define (spill-function function find off pfix)
  (begin
    (set! old-variable find)
    (set! offset (number->string off))
    (set! prefix (symbol->string pfix))
    (append* (map spill-instruction function))))

(provide spill-function)
