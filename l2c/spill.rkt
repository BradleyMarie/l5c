;; Northwestern University
;; EECS 322
;; L1 Compiler
;;
;; By Brad Weinberger & Ethan Romba
;; April 7, 2012

#lang plai

(define (op? expr)
  (member expr (list '+= '-= '*= '&= '<<= '>>=)))

(define (cmp? expr)
  (member expr (list '< '<= '= )))

;;
;; Helper Functions
;;

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
    ; (eax <- (print t))
    [`(eax <- (print ,source))
     (if (eq? source old-variable) (spill-read sexpr) (list sexpr))]
    
    ; (eax <- (allocate t t))
    ; (eax <- (array-error t t))
    [(or `(eax <- (allocate ,v1 ,v2))
         `(eax <- (array-error ,v1 ,v2)))
     (if (or (eq? v1 old-variable) (eq? v2 old-variable))
         (spill-read sexpr) (list sexpr))]
    
    ; (x <- (mem x n4)) ; read from memory @ x+n4
    [`(,dest <- (mem ,source-base ,source-offset))
     (cond
       [(and (eq? dest old-variable) (eq? source-base old-variable))
        (spill-read-write sexpr)]
       [(eq? dest old-variable) (spill-write sexpr)]
       [(eq? source-base old-variable) (spill-read sexpr)]
       [else (list sexpr)])]
    
    ; ((mem x n4) <- s) ;; update memory @ x+n4
    [`((mem ,dest-base ,dest-offset) <- ,source)
     (if (or (eq? dest-base old-variable) (eq? source old-variable))
         (spill-read sexpr) (list sexpr))]
    
    ; (x <- s) ;; assign to a register
    [`(,dest <- ,source)
     (cond
       [(and (eq? source old-variable) (eq? dest old-variable)) (list)]
       [(eq? source old-variable) (write-to-x dest)]
       [(eq? dest old-variable) (read-from-s source)]
       [else (list sexpr)])]
    
    ; (x aop= t) ;; update x with an arith op and t.
    ; (x sop= sx) ;; update x with a shifting op and sx.
    [`(,lhs ,(? op? op) ,rhs)
     (cond
       [(eq? lhs old-variable) (spill-read-write sexpr)]
       [(eq? rhs old-variable) (spill-read sexpr)]
       [else (list sexpr)])]
    
    ; (goto label) ;; unconditional jump
    ; (call u) ;; call a function
    ; (tail-call u) ;; tail call a function
    [(or `(goto ,label) `(call ,label) `(tail-call ,label))
     (if (eq? label old-variable) (spill-read sexpr) (list sexpr))]
    
    ; (cx <- t cmp t) ;; save result of a comparison;
    [`(,dest <- ,lhs ,(? cmp? cmp) ,rhs)
     (cond
       [(and (eq? dest old-variable) (or (eq? lhs old-variable) (eq? rhs old-variable)))
        (spill-read-write sexpr)]
       [(or (eq? lhs old-variable) (eq? rhs old-variable))
        (spill-read sexpr)]
       [(eq? dest old-variable)
        (spill-write sexpr)]
       [else (list sexpr)])]
    
    ; (cjump t cmp t label label) ;; conditional jump
    [`(cjump ,lhs ,(? cmp? cmp) ,rhs ,true-label ,false-label)
     (if (or (eq? lhs old-variable) (eq? rhs old-variable))
         (spill-read sexpr) (list sexpr))]
    
    [_ (list sexpr)]))

;;
;; Compile the file specified on the command line
;;

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(call-with-input-file filename
  (lambda (p)
    (begin
      (let ([sexpr (read p)])
        (set! old-variable (read p))
        (set! offset (number->string (read p)))
        (set! prefix (symbol->string (read p)))
        (display (append* (map spill-instruction sexpr)))))))