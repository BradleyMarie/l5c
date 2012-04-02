#lang plai

(print-only-errors #t)

(define (l1c-label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

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

(define (aop? expr)
  (member expr (list '+= '-= '*= '&=)))

(define (sop? expr)
  (member expr (list '<<= '>>= )))

(define (cmp? expr)
  (member expr (list '< '<= '= )))

(define (t? expr)
  (or (num? expr) (x? expr)))

(define (s? expr)
  (or (x? expr) (num? expr) (l1c-label? expr)))

(define (u? expr)
  (or (x? expr) (l1c-label? expr)))

(define (parse-l1-label label)
  (string->symbol (string-append "l1c_" (substring (symbol->string label) 1))))

(define (l1-label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define-type L1Instruction
  [assign-register (dest x?) (source s?)]
  [read-memory (dest x?) (source-base x?) (source-offset n4?)]
  [update-memory (dest-base x?) (dest-offset n4?) (source s?)]
  [update-aop (lhs x?) (operation aop?) (rhs s?)]
  [update-sop-sx (lhs x?) (operation sop?) (rhs sx?)]
  [update-sop-num (lhs x?) (operation sop?) (rhs num?)]
  [save-comparison (dest cx?) (lhs t?) (operation cmp?) (rhs t?)]
  [label (name l1c-label?)]
  [goto-label (label l1c-label?)]
  [cond-jump (cond-lhs t?) (cond-operation cmp?) (cond-rhs t?) (true-label l1c-label?) (false-label l1c-label?)]
  [call-func (func-ref u?)]
  [tail-call-func (func-ref u?)]
  [return-from-func]
  [print-t (source t?)]
  [allocate-t (size t?) (num t?)]
  [array-error-t (base t?) (index t?)])

(define (parse-instruction sexpr)
  (match sexpr
    ; (eax <- (print t))
    [(list 'eax '<- (list 'print source))
     (print-t source)]
    
    ; (eax <- (allocate t t))
    [(list 'eax '<- (list 'allocate size num))
     (allocate-t size num)]
    
    ; (eax <- (array-error t t))
    [(list 'eax '<- (list 'array-error base index))
     (array-error-t base index)]
    
    ; (x <- (mem x n4)) ; read from memory @ x+n4
    [(list dest '<- (list 'mem source-base source-offset ) )
     (read-memory dest source-base source-offset)]
    
    ; ((mem x n4) <- s) ;; update memory @ x+n4
    [(list (list 'mem dest-base dest-offset) '<- (? l1-label? source))
     (update-memory dest-base dest-offset (parse-l1-label source))]
    [(list (list 'mem dest-base dest-offset) '<- source)
     (update-memory dest-base dest-offset source)]
    
    ; (x <- s) ;; assign to a register
    [(list dest '<- (? l1-label? source))
     (assign-register dest (parse-l1-label source))]
    [(list dest '<- source)
     (assign-register dest source)]
    
    ; (x aop= t) ;; update x with an arith op and t.
    [(list lhs (? aop? aop) rhs)
     (update-aop lhs aop rhs)]
    
    ; (x sop= sx) ;; update x with a shifting op and sx.
    [(list lhs (? sop? sop) (? sx? rhs))
     (update-sop-sx lhs sop rhs)]
    
    ; (x sop= num) ;; update x with a shifting op and num.
    [(list lhs (? sop? sop) (? num? rhs))
     (update-sop-num lhs sop rhs)]
    
    ; (cx <- t cmp t) ;; save result of a comparison
    [(list dest '<- lhs cmp rhs)
     (save-comparison dest lhs cmp rhs)]
    
    ; (goto label) ;; unconditional jump
    [(list 'goto label)
     (goto-label (parse-l1-label label))]
    
    ; (cjump t cmp t label label) ;; conditional jump
    [(list 'cjump cond-lhs cond-operation cond-rhs true-label false-label)
     (cond-jump cond-lhs cond-operation cond-rhs (parse-l1-label true-label) (parse-l1-label false-label))]
    
    ; (call u) ;; call a function
    [(list 'call (? l1-label? func-ref))
     (call-func (parse-l1-label func-ref))]
    [(list 'call func-ref)
     (call-func func-ref)]
    
    ; (tail-call u) ;; tail call a function
    [(list 'tail-call (? l1-label? func-ref))
     (tail-call-func (parse-l1-label func-ref))]
    [(list 'tail-call func-ref)
     (tail-call-func func-ref)]
    
    ; (return) ;; return from a function 
    [(list 'return)
     (return-from-func)]
    
    ; label ;; target of a jump
    [(? l1-label? l)
     (label (parse-l1-label l))]))

(define (parse-function sexpr)
  (map (lambda (instruction)
         (parse-instruction instruction))
       sexpr))

(define (parse-program sexpr)
  (map (lambda (function)
         (parse-function function))
       sexpr))

;-------------------------parser tests----------------------------------------------

(test (parse-instruction '(eax <- ebx)) (assign-register 'eax 'ebx))
(test (parse-instruction '(eax <- 3)) (assign-register 'eax 3))

(test (parse-instruction '((mem eax 0) <- 3)) (update-memory 'eax 0 3))
(test (parse-instruction '(eax <- (mem eax 0))) (read-memory 'eax 'eax 0))

(test (parse-instruction '(eax += ebx)) (update-aop 'eax '+= 'ebx))
(test (parse-instruction '(eax <<= ecx)) (update-sop-sx 'eax '<<= 'ecx))
(test (parse-instruction '(eax <<= 2)) (update-sop-num 'eax '<<= 2))

(test (parse-instruction '(eax <- ebx < 2)) (save-comparison 'eax 'ebx '< 2))

;------------------------begin compiler code----------------------------------------

(define (format-operand operand)
  (cond
    [(x? operand) (string-append "%" (symbol->string operand))]
    [(l1c-label? operand) (string-append "$" (symbol->string operand))]
    [(number? operand) (string-append "$" (number->string operand))]))

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
     [(eq? '/= operation)
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
         (string-append "movl $" (boolean->numbered-string (= lhs rhs) ) ", " (format-operand dest))])
      )]
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
        (string-append "movzbl " (lowest-bits dest) ", " (format-operand dest))))
     ]
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

(define (compile-cond-jump cond-lhs cond-operation cond-rhs true-label false-label )
  (cond 
    [(and (num? cond-lhs) (num? cond-rhs))
     (print-line
      (string-append "jmp "
                     (cond
                       [(eq? '< cond-operation)
                        (if (< cond-lhs cond-rhs) (symbol->string true-label) (symbol->string false-label))]
                       [(eq? '<= cond-operation)
                        (if (<= cond-lhs cond-rhs) (symbol->string true-label) (symbol->string false-label))]
                       [(eq? '= cond-operation)
                        (if (= cond-lhs cond-rhs) (symbol->string true-label) (symbol->string false-label) )])))
     ]
    [(num? cond-lhs)
     (begin 
       (print-line
        (string-append
         "cmp " (format-operand cond-lhs) ", " (format-operand cond-rhs)))
       (print-line 
        (cond
          [(eq? '< cond-operation)
           (string-append "jg " (symbol->string true-label))]
          [(eq? '<= cond-operation)
           (string-append "jge " (symbol->string true-label))]
          [(eq? '= cond-operation)
           (string-append "je " (symbol->string true-label))]))
       (print-line
        (string-append "jmp " (symbol->string false-label))))]
    [else
     (begin 
       (print-line
        (string-append
         "cmp " (format-operand cond-rhs) ", " (format-operand cond-lhs)))
       (print-line 
        (cond
          [(eq? '< cond-operation)
           (string-append "jl " (symbol->string true-label))]
          [(eq? '<= cond-operation)
           (string-append "jle " (symbol->string true-label))]
          [(eq? '= cond-operation)
           (string-append "je " (symbol->string true-label))]))
       (print-line
        (string-append "jmp " (symbol->string false-label))))]))

(define (compile-label name)
  (print-line (string-append (symbol->string name) ":")))

(define (compile-goto-label name)
  (print-line (string-append "jmp " (symbol->string name))))

(define unique-label-index 0)
(define (generate-unique-label)
  (begin 
    (set! unique-label-index (+ unique-label-index 1))
    (string-append "label_" (number->string unique-label-index))))

(define (compile-call-func func-ref)
  (let ([new-label (generate-unique-label)])
    (begin 
      (print-line (string-append "pushl $" new-label))
      (print-line "pushl %ebp")
      (print-line "movl %esp, %ebp")
      (print-line (string-append "jmp " (symbol->string func-ref)))
      (print-line (string-append new-label ":")))))

(define (compile-return-from-func)
  (begin
    (print-line "movl %ebp, %esp")
    (print-line "popl %ebp")
    (print-line "ret")))

(define (compile-tail-call-func func-ref)
  (begin
    (print-line "movl %ebp, %esp")
    (print-line (string-append "jmp " (symbol->string func-ref)))))

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
    (print-line "call array-error")
    (print-line "addl $8,%esp")))

(define (compile-instruction instruction)
  (type-case L1Instruction instruction
    [assign-register (dest source)
                     (compile-assign-register dest source)]
    [read-memory (dest source-base source-offset)
                 (compile-read-memory dest source-base source-offset)]
    [update-memory (dest-base dest-offset source)
                 (compile-update-memory dest-base dest-offset source)]
    [update-aop (lhs operation rhs)
                (compile-update-aop lhs operation rhs)]
    [update-sop-sx (lhs operation rhs)
                   (compile-update-sop-sx lhs operation rhs)]
    [update-sop-num (lhs operation rhs)
                    (compile-update-sop-num lhs operation rhs)]
    [save-comparison (dest lhs operation rhs)
                     (compile-save-comparison dest lhs operation rhs)]
    [label (name)
           (compile-label name)]
    [goto-label (name)
                (compile-goto-label name)]
    [cond-jump (cond-lhs cond-operation cond-rhs true-label false-label)
               (compile-cond-jump cond-lhs cond-operation cond-rhs true-label false-label)]
    [call-func (func-ref)
               (compile-call-func func-ref)]
    [tail-call-func (func-ref)
                    (compile-tail-call-func func-ref)]
    [return-from-func ()
     (compile-return-from-func)]
    [print-t (source)
             (compile-print-t source)]
    [allocate-t (size num)
                (compile-allocate-t size num)]
    [array-error-t (base index)
                   (compile-array-error-t)]))

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

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(compile-program (parse-program (call-with-input-file filename read)))