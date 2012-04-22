;; Northwestern University
;; EECS 322
;; L1 Compiler
;;
;; By Brad Weinberger & Ethan Romba
;; April 2, 2012

#lang racket

(print-only-errors #t)

(define (num? expr)
  (number? expr))

(define (n4? expr)
  (and (num? expr) (eq? (modulo expr 4) 0)))

(define (sx? expr)
  (eq? 'ecx expr))

(define (cx? expr)
  (member expr (list 'eax 'ebx 'ecx 'edx)))

(define (x? expr)
  (or (cx? expr)
      (member expr (list 'esi 'edi 'ebp 'esp))))

(define (l1-label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define (l1c-label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define (t? expr)
  (or (num? expr) (x? expr)))

(define (s? expr)
  (or (x? expr) (num? expr) (l1c-label? expr)))

(define (aop? expr)
  (member expr (list '+= '-= '*= '&=)))

(define (sop? expr)
  (member expr (list '<<= '>>= )))

(define (cmp? expr)
  (member expr (list '< '<= '= )))

;;
;; Helper Functions
;;

(define (parse-l1-label label)
  (string->symbol (string-append "l1c_" (substring (symbol->string label) 1))))

(define (format-operand operand)
  (cond
    [(x? operand) (string-append "%" (symbol->string operand))]
    [(number? operand) (string-append "$" (number->string operand))]
    [(l1c-label? operand) (string-append "$" (symbol->string operand))]))

(define (format-label label)
  (symbol->string label))

(define (lowest-bits register)
  (cond
    [(eq? 'eax register) "%al"]
    [(eq? 'ebx register) "%bl"]
    [(eq? 'ecx register) "%cl"]
    [(eq? 'edx register) "%dl"]))

(define (print-line string)
  (begin
    (display string)
    (newline)))

;;
;; Compiler Routines for Individual Instructions
;;

(define (compile-assign-register dest source)
  (print-line (string-append "movl " (format-operand source) ", " (format-operand dest))))

(define (compile-read-memory dest source-base source-offset)
  (print-line (string-append "movl " (number->string source-offset) "(" (format-operand source-base) "), " (format-operand dest))))

(define (compile-update-memory dest-base dest-offset source)
  (print-line (string-append "movl " (format-operand source) ", " (number->string dest-offset) "(" (format-operand dest-base) ")")))

(define (boolean->numbered-string value)
  (if value "1" "0"))

(define (compile-update-aop lhs operation rhs)
  (print-line
   (cond
     [(eq? '+= operation)
      (string-append "addl " (format-operand rhs) ", " (format-operand lhs))]
     [(eq? '-= operation)
      (string-append "subl " (format-operand rhs) ", " (format-operand lhs))]
     [(eq? '*= operation)
      (string-append "imul " (format-operand rhs) ", " (format-operand lhs))]
     [(eq? '&= operation)
      (string-append "andl " (format-operand rhs) ", " (format-operand lhs))])))

(define (compile-update-sop-sx lhs operation rhs)
  (print-line
   (cond
     [(eq? '<<= operation)
      (string-append "sall %cl, " (format-operand lhs))]
     [(eq? '>>= operation)
      (string-append "sarl %cl, " (format-operand lhs))])))

(define (compile-update-sop-num lhs operation rhs)
  (print-line
   (cond
     [(eq? '<<= operation)
      (string-append "sall " (format-operand rhs) ", " (format-operand lhs))]
     [(eq? '>>= operation)
      (string-append "sarl " (format-operand rhs) ", " (format-operand lhs))])))

(define (compile-save-comparison dest lhs operation rhs)
  (cond 
    [(and (num? lhs) (num? rhs))
     (print-line
      (cond
        [(eq? '< operation)
         (string-append "movl $" (boolean->numbered-string (< lhs rhs) ) ", " (format-operand dest))]
        [(eq? '<= operation)
         (string-append "movl $" (boolean->numbered-string (<= lhs rhs) ) ", " (format-operand dest))]
        [(eq? '= operation)
         (string-append "movl $" (boolean->numbered-string (= lhs rhs) ) ", " (format-operand dest))]))]
    [(num? lhs)
     (begin 
       (print-line
        (string-append
         "cmp " (format-operand lhs) ", " (format-operand rhs)))
       (print-line 
        (cond
          [(eq? '< operation)
           (string-append "setg " (lowest-bits dest))]
          [(eq? '<= operation)
           (string-append "setge " (lowest-bits dest))]
          [(eq? '= operation)
           (string-append "sete " (lowest-bits dest))]))
       (print-line
        (string-append "movzbl " (lowest-bits dest) ", " (format-operand dest))))]
    [else
     (begin 
       (print-line
        (string-append
         "cmp " (format-operand rhs) ", " (format-operand lhs)))
       (print-line 
        (cond
          [(eq? '< operation)
           (string-append "setl " (lowest-bits dest))]
          [(eq? '<= operation)
           (string-append "setle " (lowest-bits dest))]
          [(eq? '= operation)
           (string-append "sete " (lowest-bits dest))]))
       (print-line
        (string-append "movzbl " (lowest-bits dest) ", " (format-operand dest))))]))

(define (compile-cond-jump cond-lhs cond-operation cond-rhs true-label false-label)
  (cond 
    [(and (num? cond-lhs) (num? cond-rhs))
     (print-line
      (string-append "jmp "
                     (cond
                       [(eq? '< cond-operation)
                        (if (< cond-lhs cond-rhs) (format-label true-label) (format-label false-label))]
                       [(eq? '<= cond-operation)
                        (if (<= cond-lhs cond-rhs) (format-label true-label) (format-label false-label))]
                       [(eq? '= cond-operation)
                        (if (= cond-lhs cond-rhs) (format-label true-label) (format-label false-label) )])))]
    [(num? cond-lhs)
     (begin 
       (print-line
        (string-append
         "cmp " (format-operand cond-lhs) ", " (format-operand cond-rhs)))
       (print-line 
        (cond
          [(eq? '< cond-operation)
           (string-append "jg " (format-label true-label))]
          [(eq? '<= cond-operation)
           (string-append "jge " (format-label true-label))]
          [(eq? '= cond-operation)
           (string-append "je " (format-label true-label))]))
       (print-line
        (string-append "jmp " (format-label false-label))))]
    [else
     (begin 
       (print-line
        (string-append
         "cmp " (format-operand cond-rhs) ", " (format-operand cond-lhs)))
       (print-line 
        (cond
          [(eq? '< cond-operation)
           (string-append "jl " (format-label true-label))]
          [(eq? '<= cond-operation)
           (string-append "jle " (format-label true-label))]
          [(eq? '= cond-operation)
           (string-append "je " (format-label true-label))]))
       (print-line
        (string-append "jmp " (format-label false-label))))]))

(define (compile-label name)
  (print-line (string-append (format-label name) ":")))

(define (compile-goto-label name)
  (print-line (string-append "jmp " (format-label name))))

(define unique-label-index 0)
(define (generate-unique-label)
  (begin 
    (set! unique-label-index (+ unique-label-index 1))
    (string-append "label_" (number->string unique-label-index))))

(define (compile-call-func-label func-ref)
  (let ([new-label (generate-unique-label)])
    (begin 
      (print-line (string-append "pushl $" new-label))
      (print-line "pushl %ebp")
      (print-line "movl %esp, %ebp")
      (print-line (string-append "jmp " (format-label func-ref)))
      (print-line (string-append new-label ":")))))

(define (compile-tail-call-func-label func-ref)
  (begin
    (print-line "movl %ebp, %esp")
    (print-line (string-append "jmp " (format-label func-ref)))))

(define (compile-call-func-x func-ref)
  (let ([new-label (generate-unique-label)])
    (begin 
      (print-line (string-append "pushl $" new-label))
      (print-line "pushl %ebp")
      (print-line "movl %esp, %ebp")
      (print-line (string-append "jmp *" (format-operand func-ref)))
      (print-line (string-append new-label ":")))))

(define (compile-tail-call-func-x func-ref)
  (begin
    (print-line "movl %ebp, %esp")
    (print-line (string-append "jmp *" (format-operand func-ref)))))

(define (compile-return-from-func)
  (begin
    (print-line "movl %ebp, %esp")
    (print-line "popl %ebp")
    (print-line "ret")))

(define (compile-print-t source)
  (begin
    (print-line (string-append "pushl " (format-operand source)))
    (print-line "call print")
    (print-line "addl $4,%esp")))

(define (compile-allocate-t size num)
  (begin
    (print-line (string-append "pushl " (format-operand num)))
    (print-line (string-append "pushl " (format-operand size)))
    (print-line "call allocate")
    (print-line "addl $8,%esp")))

(define (compile-array-error-t base index)
  (begin
    (print-line (string-append "pushl " (format-operand index)))
    (print-line (string-append "pushl " (format-operand base)))
    (print-line "call array_error")
    (print-line "addl $8,%esp")))

;;
;; Main Compiler Implementation
;;

(define (compile-instruction sexpr)
  (match sexpr
    ; (eax <- (print t))
    [(list 'eax '<- (list 'print source))
     (compile-print-t source)]
    
    ; (eax <- (allocate t t))
    [(list 'eax '<- (list 'allocate size num))
     (compile-allocate-t size num)]
    
    ; (eax <- (array-error t t))
    [(list 'eax '<- (list 'array-error base index))
     (compile-array-error-t base index)]
    
    ; (x <- (mem x n4)) ; read from memory @ x+n4
    [(list dest '<- (list 'mem source-base source-offset ) )
     (compile-read-memory dest source-base source-offset)]
    
    ; ((mem x n4) <- s) ;; update memory @ x+n4
    [(list (list 'mem (? x? dest-base) (? n4? dest-offset)) '<- (? l1-label? source))
     (compile-update-memory dest-base dest-offset (parse-l1-label source))]
    [(list (list 'mem (? x? dest-base) (? n4? dest-offset)) '<- source)
     (compile-update-memory dest-base dest-offset source)]
    
    ; (x <- s) ;; assign to a register
    [(list dest '<- (? l1-label? source))
     (compile-assign-register dest (parse-l1-label source))]
    [(list dest '<- source)
     (compile-assign-register dest source)]
    
    ; (x aop= t) ;; update x with an arith op and t.
    [(list lhs (? aop? aop) (? t? rhs))
     (compile-update-aop lhs aop rhs)]
    
    ; (x sop= sx) ;; update x with a shifting op and sx.
    [(list lhs (? sop? sop) (? sx? rhs))
     (compile-update-sop-sx lhs sop rhs)]
    
    ; (x sop= num) ;; update x with a shifting op and num.
    [(list lhs (? sop? sop) (? num? rhs))
     (compile-update-sop-num lhs sop rhs)]
    
    ; (cx <- t cmp t) ;; save result of a comparison
    [(list dest '<- (? t? lhs) (? cmp? cmp) (? t? rhs))
     (compile-save-comparison dest lhs cmp rhs)]
    
    ; (goto label) ;; unconditional jump
    [(list 'goto (? l1-label? label))
     (compile-goto-label (parse-l1-label label))]
    
    ; (cjump t cmp t label label) ;; conditional jump
    [(list 'cjump (? t? lhs) (? cmp? cmp) (? t? rhs) (? l1-label? true-label) (? l1-label? false-label))
     (compile-cond-jump lhs cmp rhs (parse-l1-label true-label) (parse-l1-label false-label))]
    
    ; (call u) ;; call a function
    [(list 'call (? l1-label? func-ref))
     (compile-call-func-label (parse-l1-label func-ref))]
    [(list 'call (? x? func-ref))
     (compile-call-func-x func-ref)]
    
    ; (tail-call u) ;; tail call a function
    [(list 'tail-call (? l1-label? func-ref))
     (compile-tail-call-func-label (parse-l1-label func-ref))]
    [(list 'tail-call (? x? func-ref))
     (compile-tail-call-func-x func-ref)]
    
    ; (return) ;; return from a function 
    [(list 'return)
     (compile-return-from-func)]
    
    ; label ;; target of a jump
    [(? l1-label? l)
     (compile-label (parse-l1-label l))]))

(define (compile-function function)
  (map (lambda (instruction)
         (compile-instruction instruction))
       function))
  
(define (compile-program sexpr)
  (begin
    (compile-function (first sexpr))
    (print-line "popl %ebp");
    (print-line "popl %edi");
    (print-line "popl %esi");
    (print-line "popl %ebx");
    (print-line "leave");
    (print-line "ret");
    (map (lambda (function)
           (compile-function function))
         (rest sexpr))
    (void)))

;;
;; Compile the file specified on the command line
;;

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(compile-program (call-with-input-file filename read))
