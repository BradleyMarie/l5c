#lang plai

(print-only-errors #t)

(define (label? expr)
  (symbol? expr))

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
  (or (x? expr) (num? expr) (label? expr)))

(define (u? expr)
  (or (x? expr) (label? expr)))


(define-type L1Instruction
  [assign-register (dest x?) (source s?)]
  [read-memory (dest x?) (source-base x?) (source-offset n4?)]
  [update-memory (dest-base x?) (dest-offset n4?) (source s?)]
  [update-aop (lhs x?) (operation aop?) (rhs s?)]
  [update-sop-sx (lhs x?) (operation sop?) (rhs sx?)]
  [update-sop-num (lhs x?) (operation sop?) (rhs num?)]
  [save-comparison (dest cx?) (lhs t?) (operation cmp?) (rhs t?)]
  [l1-label (name label?)]
  [goto-label (label label?)]
  [cond-jump (cond-lhs t?) (cond-operation cmp?) (cond-rhs t?) (true-label label?) (false-label label?)]
  [call-func (func-ref u?)]
  [tail-call-func (func-ref u?)]
  [return-from-func]
  [print-t (source t?)]
  [allocate-t (size t?) (num t?)]
  [array-error-t (base t?) (index t?)])

(define (parse-instruction sexpr)
  (match sexpr
    [(list dest '<- (list 'mem source-base source-offset ) )
     (read-memory dest source-base source-offset)]
    [(list (list 'mem dest-base dest-offset) '<- source)
     (update-memory dest-base dest-offset source)]
    [(list dest '<- source) (assign-register dest source)]
    [(list lhs (? aop? aop) rhs)
     (update-aop lhs aop rhs)]
    [(list lhs (? sop? sop) (? sx? rhs))
     (update-sop-sx lhs sop rhs)]
    [(list lhs (? sop? sop) (? num? rhs))
     (update-sop-num lhs sop rhs)]
    [(list dest '<- lhs cmp rhs)
     (save-comparison dest lhs cmp rhs)]
    [(list 'goto label)
     (goto-label label)]
    [(list 'cjump cond-lhs cond-operation cond-rhs true-label false-label)
     (cond-jump cond-lhs cond-operation cond-rhs true-label false-label)]
    [(list 'call func-ref)
     (call-func func-ref)]
    [(list 'tail-call func-ref)
     (tail-call-func func-ref)]
    [(list 'return)
     (return-from-func)]
    [(list 'eax '<- (list 'print source))
     (print-t source)]
    [(list 'eax '<- (list 'allocate size num))
     (allocate-t size num)]
    [(list 'eax '<- (list 'array-error base index))
     (array-error-t base index)]))

(test (parse-instruction '(eax <- ebx)) (assign-register 'eax 'ebx))
(test (parse-instruction '(eax <- 3)) (assign-register 'eax 3))

(test (parse-instruction '((mem eax 0) <- 3)) (update-memory 'eax 0 3))
(test (parse-instruction '(eax <- (mem eax 0))) (read-memory 'eax 'eax 0))

(test (parse-instruction '(eax += ebx)) (update-aop 'eax '+= 'ebx))
(test (parse-instruction '(eax <<= ecx)) (update-sop-sx 'eax '<<= 'ecx))
(test (parse-instruction '(eax <<= 2)) (update-sop-num 'eax '<<= 2))

(test (parse-instruction '(eax <- ebx < 2)) (save-comparison 'eax 'ebx '< 2))
