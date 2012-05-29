;; Northwestern University
;; EECS 322
;; L5c
;;
;; By Brad Weinberger & Ethan Romba
;; May 28, 2012

#lang plai

;;
;; Helpers
;;

(define reserved-words (set 
                        '+ '- '* '< '<= '=
                        'number? 'a?
                        'lambda 'new-array 'aref 'aset 'alen 
                        'begin 'print 'new-tuple
                        'let 'letrec 'if))

(define (reserved-word? sym) (set-member? reserved-words sym))

(define (variable? expr)
  (if (symbol? expr)
      (if (not (reserved-word? expr))
          (match (symbol->string expr)
            [(regexp #rx"^[a-zA-Z_-][a-zA-Z_0-9-]*$") #t]
            [_ #f])
          #f)
      #f))

(define variable-suffix -1)
(define variable-prefix "newvar")

(define (new-variable)
  (begin
    (set! variable-suffix (+ variable-suffix 1))
    (string->symbol (string-append variable-prefix (number->string variable-suffix)))))

(define function-suffix -1)
(define function-prefix ":newfunc")

(define (new-function)
  (begin
    (set! function-suffix (+ function-suffix 1))
    (string->symbol (string-append function-prefix (number->string function-suffix)))))

;; Original Program -> Rename all variables and labels -> 
;; Rename let variables -> Remove letrecs
;;
;; Variable/Label Renaming
;;

(define (rename-variable sexpr original-name new-name)
  (cond
    [(and (variable? sexpr) (eq? sexpr original-name)) new-name]
    [(list? sexpr) (map (lambda (e) (rename-variable e original-name new-name)) sexpr)]
    [else sexpr]))

(define (rename-let-variables sexpr)
  (match sexpr
    [`(let ([,x ,r]) ,b)
     (let ((new-var (new-variable)))
       `(let ([,new-var ,(rename-let-variables r)])       
          ,(rename-let-variables
            (rename-variable b x new-var))))]
    
    [`(letrec ([,x ,r]) ,b)
     (let ((new-var (new-variable)))
       `(letrec ([,new-var ,(rename-variable (rename-let-variables r) x new-var)])       
          ,(rename-let-variables
            (rename-variable b x new-var))))]
    
    [`(lambda (,args ...) ,e)
     (let [(variable-names (map (lambda (x) (new-variable)) args))]
       `(lambda ,variable-names
          ,(foldl
           (lambda (old-name new-name expr)
             (rename-variable expr old-name new-name))
           e
           args
           variable-names)))]
    
    [else
     (if (list? sexpr)
         (map rename-let-variables sexpr)
         sexpr)]))

(define (remove-letrecs sexpr)
  (match sexpr
    [`(letrec ([,x ,e1]) ,e2)
     `(let ((,x (new-tuple 0))) 
        (begin (aset ,x 0 ,(rename-variable e1 x `(aref ,x 0))) 
               ,(rename-variable e2 x `(aref ,x 0))))]
    [_
     (if (list? sexpr)
         (map remove-letrecs sexpr)
         sexpr)]))
;    
;    [(or (number? sexpr) (variable? sexpr)) sexpr]
;    [(or (list? (first sexpr)) (variable? (first sexpr)))
;     (let [(new-func-var (new-variable))]
;       `(let ([,new-func-var ,(first sexpr)])
;          ((closure-proc ,new-func-var)
;           (closure-vars ,new-func-var)
;           ,(map remove-application-expressions (rest sexpr)))))]
;    [else (map remove-application-expressions sexpr)]))

(define (preprocess-l5-program sexpr)
  (remove-letrecs (rename-let-variables sexpr)))

;;
;; Helper functions
;;

(define (symbol-by-name<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (get-free-variables-rec sexpr bound-variables)
  (cond
    [(variable? sexpr) (if (set-member? bound-variables sexpr)
                           (set)
                           (set sexpr))]
    [(list? sexpr) (foldl (lambda (expr result) 
                            (set-union 
                             (get-free-variables-rec expr bound-variables) 
                             result)) 
                          (set) 
                          sexpr)]
    [else (set)]))

(define (variable-list-to-mapping-rec free-variable-list map)
  (if (empty? free-variable-list)
              map
              (variable-list-to-mapping-rec
               (rest free-variable-list)
               (hash-set map (car free-variable-list) (hash-count map)))))

(define (variable-list-to-mapping free-variable-list)
  (variable-list-to-mapping-rec free-variable-list (hash)))

(define (get-free-variables sexpr)
  (sort (set->list (get-free-variables-rec (third sexpr) (list->set (second sexpr)))) symbol-by-name<?))

(define (rewrite-lambda-expression-rec sexpr free-variable-map argument-variable-map)
  (cond
    [(hash-has-key? free-variable-map sexpr) `(aref closure ,(hash-ref free-variable-map sexpr))]
    [(hash-has-key? argument-variable-map sexpr) `(aref arguments ,(hash-ref argument-variable-map sexpr))]
    [(list? sexpr) (map (lambda (e) (rewrite-lambda-expression-rec e free-variable-map argument-variable-map)) sexpr)]
    [else sexpr]))

(define (rewrite-lambda-expression function-name sexpr free-variable-map)
  (let [(args (second sexpr))]
    (if (<= (length args) 2) ;; Yes free variable closure
        `(,function-name ,(cons 'closure args) ,(rewrite-lambda-expression-rec (third sexpr) free-variable-map (hash))) ;; No argument closure
        `(,function-name (closure arguments) ,(rewrite-lambda-expression-rec (third sexpr) free-variable-map (variable-list-to-mapping args)))))) ;; Yes argument closure

;    (if (= 0 (hash-count free-variable-map)) 
;        (if (<= (length arguments) 3) ;; No free variable closure
;            `(,function-name arguments ,(rewrite-lambda-expression-rec (third sexpr) free-variable-map (hash))) ;; No argument closure
;            `(,function-name '(arguments) ,(rewrite-lambda-expression-rec (third sexpr) free-variable-map (variable-list-to-mapping arguments)))) ;; Yes argument closure

;;
;; Compile Program
;;

(define (lift-lambdas sexpr)
  (match sexpr
    [`(lambda (,args ...) ,e)
     (let* [(new-function-name (new-function)) 
            (free-variable-list (get-free-variables sexpr))
            (free-variable-map (variable-list-to-mapping free-variable-list))]
           (list 
            `(make-closure ,new-function-name ,(cons 'new-tuple free-variable-list))
            (rewrite-lambda-expression new-function-name sexpr free-variable-map)))]
    
    [_ (if (list? sexpr)
             (foldl
              (lambda (list-of-expr result)
                (list
                 (append (first result) (list (first list-of-expr)))
                 (append (second result) (second list-of-expr))))
              (list (list) (list))
              (map lift-lambdas sexpr))
           (list sexpr (list)))]))

(define (lift-all-lambdas-rec first-order-functions lifted-expressions first-iteration)
  (if (empty? lifted-expressions)
      (if first-iteration
          (list first-order-functions)
          first-order-functions)
      (let [(result (lift-lambdas lifted-expressions))]
        (lift-all-lambdas-rec
         (append (list first-order-functions) (list (first result)))
         (second result)
         #f))))

(define (lift-all-lambdas sexpr)
  (let [(result (lift-lambdas sexpr))]
        (lift-all-lambdas-rec
         (first result)
         (second result)
         #t)))

(define (wrap-application-expression func args)
  (let [(new-func-var (new-variable))]
        `(let ([,new-func-var ,func])
           ,(list* 
             (list
              'closure-proc
              new-func-var)
             (list
              'closure-vars
              new-func-var)
             args))))

(define (remove-application-expressions-rec sexpr)
  (cond
    [(or (not (list? sexpr))
         (= 1 (length sexpr))) sexpr]
    [(match (first sexpr)
       ;[`(lambda (,args ...) ,e) #t]
       ;[`(letrec ([,x ,r]) ,b) #t]
       [`(make-closure ,x (new-tuple ,y ...)) #t]
       [`(let ([,x ,r]) ,b) #t]
       [(? variable?) #t]
       [_
        #f])
     (wrap-application-expression (first sexpr) (rest sexpr))]
    [else
     (cons (first sexpr) (map remove-application-expressions-rec (rest sexpr)))]))

(define (remove-application-expressions sexpr)
  (cons
   (remove-application-expressions-rec (first sexpr))
   (map (lambda (expr) (list (first expr) (second expr) (remove-application-expressions-rec (third expr)))) (rest sexpr))))

;;
;; Compile the file specified on the command line
;;

(define (compile-program sexpr)
 (remove-application-expressions (lift-all-lambdas (preprocess-l5-program sexpr))))

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))
 
(pretty-display 
 (compile-program (call-with-input-file filename read)))