;; Northwestern University
;; EECS 322
;; L4c
;;
;; By Brad Weinberger & Ethan Romba
;; May 12, 2012

#lang plai

;;
;; Helpers
;;

(define reserved-words (set 
                        '+ '- '* '< '<= '=
                        'number? 'a?
                        'new-array 'new-tuple 'aref 'aset 'alen 
                        'begin 'print 
                        'make-closure 'closure-proc 'closure-vars 
                        'let 'if))

(define (reserved-word? sym) (set-member? reserved-words sym))

(define (label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define (variable? expr)
  (if (symbol? expr)
      (if (not (reserved-word? expr))
          (match (symbol->string expr)
            [(regexp #rx"^[a-zA-Z_-][a-zA-Z_0-9-]*$") #t]
            [_ #f])
          #f)
      #f))

(define (L3-v? sexpr)
  (or (variable? sexpr) (label? sexpr) (number? sexpr) (reserved-word? sexpr)))

(define (L4-e? sexpr) #t)

(define variable-suffix -1)
(define variable-prefix "newvar")

(define (new-variable)
  (begin
    (set! variable-suffix (+ variable-suffix 1))
    (string->symbol (string-append variable-prefix (number->string variable-suffix)))))

;;
;; Variable/Label Renaming
;;

(define (translate-l4-instruction sexpr)
  (match sexpr
    [`(begin ,e1 ,e2)
     `(let ((,(new-variable) ,e1) ,e2))]
    [_
     sexpr]))

(define (translate-l4-program sexpr)
  (cond
    [(label? sexpr) (string->symbol (string-append ":l4c_" (substring (symbol->string sexpr) 1)))]
    [(variable? sexpr) (string->symbol (string-append "l4c_" (symbol->string sexpr)))]
    [(list? sexpr) (map (lambda (e) (translate-l4-instruction (translate-l4-program e))) sexpr)]
    [else sexpr]))

;;
;; Type Definition
;;

(define-type context
  [let-ctxt (x variable?)
            (b L4-e?)
            (k context?)]
  [if-ctxt (t L4-e?)
           (e L4-e?)
           (k context?)]
  [fun-ctxt (a (listof L4-e?))
            (k context?)]
  [arg-ctxt (p (listof L3-v?))
            (a (listof L4-e?))
            (k context?)]
  [no-ctxt])

;;
;; Find/Fill
;;

(define (find e k)
  (match e
    [`(let ([,x ,r]) ,b)
     (find r (let-ctxt x b k))]
    [`(if ,c ,t ,e)
     (find c (if-ctxt t e k))]
    [(? L3-v?)
     (fill e k)]
    [`(,f ,a ...)
     (find f (fun-ctxt a k))]))

; maybe-let: L3-d (val → L3-e) → L3-e

(define (maybe-let d f)
  (if (L3-v? d)
      (f d)
      (let ([x (new-variable)])
        `(let ([,x ,d])
           ,(f x)))))

(define (fill d k)
  (type-case context k
    [let-ctxt (x b k)
              `(let ([,x ,d])
                 ,(find b k))]
    [if-ctxt (t e k)
             (maybe-let d
                        (λ (v)
                          `(if ,v
                               ,(find t k)
                               ,(find e k))))]
    [fun-ctxt (a k)
              (maybe-let d
                         (λ (d)
                           (find (first a) (arg-ctxt (list d) (rest a) k))))]
    
    [arg-ctxt (p a k)
              (if (empty? a)
                  (maybe-let d
                             (λ (arg)
                               (fill (append p (list arg)) k)))
                  (maybe-let d
                             (λ (arg)
                               (fill (first a) (arg-ctxt (append p (list arg)) (rest a) k)))))]
    [no-ctxt () d]))

; norm: L4-e → L3-e
(define (norm e)
  (find e (no-ctxt)))

(define (normalize-function sexpr)
  (list
   (first sexpr)
   (second sexpr)
   (norm (third sexpr))))

(define (normalize-program sexpr)
  (cons
   (norm (first sexpr))
   (map normalize-function (rest sexpr))))

;;
;; Compile the file specified on the command line
;;

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(display (normalize-program (call-with-input-file filename read)))
