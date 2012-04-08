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

(define (print-line string)
  (begin
    (display string)
    (newline)))

(define (replace-list-elements element find-value replace-value)
  (cond
    [(eq? element find-value) replace-value]
    [(list? element) (map (lambda (e) (replace-list-elements e find-value replace-value)) element)]
    [else element]))

(define (print-modified-list find-value replace-value original-list)
  (print-line (map (lambda (element) (replace-list-elements element find-value replace-value)) original-list)))

(define (read-memory variable offset)
  (printf "(~a <- (mem ebp ~a))\n" variable offset ))

(define (write-memory variable offset)
  (printf "((mem ebp ~a) <- ~a)\n" offset variable ))

(define prefix-index -1)  ;; Number
(define prefix (void))       ;; String
(define offset (void))      ;; String
(define old-variable (void))  ;; Symbol

(define (spill-variable)
  (begin
    (set! prefix-index (+ prefix-index 1))
    (string-append prefix (number->string prefix-index))))

(define (spill-read original-expression)
  (let ([new-variable (spill-variable)])
    (begin
      (read-memory new-variable offset)
      (print-modified-list old-variable new-variable original-expression))))

(define (spill-write original-expression)
  (let ([new-variable (spill-variable)])
    (begin
      (print-modified-list old-variable new-variable original-expression)
      (write-memory new-variable offset))))

(define (spill-read-write original-expression)
  (let ([new-variable (spill-variable)])
    (begin
      (read-memory new-variable offset)
      (print-modified-list old-variable new-variable original-expression)
      (write-memory new-variable offset))))

(define (spill-from-memory x n4)
  (let ([new-variable (spill-variable)])
    (begin
      (printf "(~a <- (mem ~a ~a))\n" new-variable x n4)
      (write-memory new-variable offset))))

(define (spill-to-memory x n4)
  (let ([new-variable (spill-variable)])
    (begin
      (read-memory new-variable offset)
      (printf "((mem ~a ~a) <- ~a)\n" x n4 new-variable))))

(define (write-to-x dest)
  (printf "(~a <- (mem ebp ~a))\n" dest offset))

(define (read-from-s source)
  (printf "((mem ebp ~a) <- ~a)\n" offset source))

;;
;; Parser
;;

(define (spill-instruction sexpr)
  (match sexpr
    ; (eax <- (print t))
    [(list 'eax '<- (list 'print source))
     (if (eq? source old-variable) (spill-read sexpr) (print-line sexpr))]
    
    ; (eax <- (allocate t t))
    ; (eax <- (array-error t t))
    [(or (list 'eax '<- (list 'allocate v1 v2))
         (list 'eax '<- (list 'array-error v1 v2)))
     (if (or (eq? v1 old-variable) (eq? v2 old-variable))
         (spill-read sexpr) (print-line sexpr))]
    
    ; (x <- (mem x n4)) ; read from memory @ x+n4
    [(list dest '<- (list 'mem source-base source-offset ))
     (spill-from-memory source-base source-offset)]
    
    ; ((mem x n4) <- s) ;; update memory @ x+n4
    [(list (list 'mem dest-base dest-offset) '<- source) 
     (spill-to-memory dest-base dest-offset)]
    
    ; (x <- s) ;; assign to a register
    [(list dest '<- source)
     (cond
       [(and (eq? source old-variable) (eq? dest old-variable)) (void)]
       [(eq? source old-variable) (write-to-x dest)]
       [(eq? dest old-variable) (read-from-s source)]
       [else (print-line sexpr)])]
    
    ; (x aop= t) ;; update x with an arith op and t.
    ; (x sop= sx) ;; update x with a shifting op and sx.
    [(list lhs (? op? op) rhs)
     (cond
       [(eq? lhs old-variable) (spill-read-write sexpr)]
       [(eq? rhs old-variable) (spill-read sexpr)]
       [else (print-line sexpr)])]
    
    ; (goto label) ;; unconditional jump
    ; (call u) ;; call a function
    ; (tail-call u) ;; tail call a function
    [(or (list 'goto label) (list 'call label) (list 'tail-call label))
     (if (eq? label old-variable) (spill-read sexpr) (print-line sexpr))]
    
    ; (cx <- t cmp t) ;; save result of a comparison
    [(list dest '<- lhs (? cmp? cmp) rhs)
     (cond
       [(and (eq? dest old-variable) (or (eq? lhs old-variable) (eq? rhs old-variable)))
        (spill-read-write sexpr)]
       [(or (eq? lhs old-variable) (eq? rhs old-variable))
        (spill-read sexpr)]
       [(eq? dest old-variable)
        (spill-write sexpr)]
       [else (print-line sexpr)])]
    
    ; (cjump t cmp t label label) ;; conditional jump
    [(list 'cjump lhs (? cmp? cmp) rhs true-label false-label)
     (if (or (eq? lhs old-variable) (eq? rhs old-variable))
         (spill-read sexpr) (print-line sexpr))]))

(define (spill-function function)
    (map (lambda (instruction)
         (spill-instruction instruction))
       function))

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
        (spill-function sexpr)))))