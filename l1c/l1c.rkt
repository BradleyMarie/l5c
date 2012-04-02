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
    
    ; (eax <- (print t))
    [(list 'eax '<- (list 'print source))
     (print-t source)]
    
    ; (eax <- (allocate t t))
    [(list 'eax '<- (list 'allocate size num))
     (allocate-t size num)]
    
    ; (eax <- (array-error t t))
    [(list 'eax '<- (list 'array-error base index))
     (array-error-t base index)]
    
    ; label ;; target of a jump
    [(? l1-label? l)
     (label (parse-l1-label l))]))

(define (parse-function sexpr)
  (map (lambda (instruction)
         (parse-instruction instruction))
       sexpr))

(define (parse-program sexpr)
  (append* (map (lambda (function)
                  (parse-function function))
                sexpr)))

(test (parse-instruction '(eax <- ebx)) (assign-register 'eax 'ebx))
(test (parse-instruction '(eax <- 3)) (assign-register 'eax 3))

(test (parse-instruction '((mem eax 0) <- 3)) (update-memory 'eax 0 3))
(test (parse-instruction '(eax <- (mem eax 0))) (read-memory 'eax 'eax 0))

(test (parse-instruction '(eax += ebx)) (update-aop 'eax '+= 'ebx))
(test (parse-instruction '(eax <<= ecx)) (update-sop-sx 'eax '<<= 'ecx))
(test (parse-instruction '(eax <<= 2)) (update-sop-num 'eax '<<= 2))

(test (parse-instruction '(eax <- ebx < 2)) (save-comparison 'eax 'ebx '< 2))
