;; Northwestern University
;; EECS 322
;; L3c
;;
;; By Brad Weinberger & Ethan Romba
;; May 6, 2012

#lang racket

;;
;; Helpers
;;

(define (label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

;;
;; L3 -> L3 Translation
;;

(define (translate-l3-instruction sexpr)
  (match sexpr
    [`(make-closure ,l ,v)
     `(new-tuple ,l ,v)]
    [`(closure-proc ,v)
     `(aref ,v 0)]
    [`(closure-vars ,v)
     `(aref ,v 1)]
    [_
     sexpr]))

(define (encode number)
  (+ 1 (* 2 number)))

(define (translate-l3-program sexpr)
  (cond
    [(number? sexpr) (encode sexpr)]
    [(label? sexpr) (string->symbol (string-append ":l3c_" (substring (symbol->string sexpr) 1)))]
    [(list? sexpr) (map (lambda (e) (translate-l3-instruction (translate-l3-program e))) sexpr)]
    [else sexpr]))

;;
;; L3 -> L2 Translation
;;

(define biops (set '+ '- '= '< '<= '=))
(define (biop? sym) (set-member? biops sym))
   
(define (l3-call-rec list-of-args list-of-regs output label)
  (if (empty? list-of-args)
      (append output (list `(call ,label)))
      (l3-call-rec
       (rest list-of-args)
       (rest list-of-regs)
       (append output (list `(,(first list-of-regs) <- ,(first list-of-args))))
       label)))

(define (l3-call list-of-args label)
  (l3-call-rec list-of-args '(eax edx ecx) (list) label))

(define (compile-biop op args)
  (match op
    ['+
     (l3-call args ':add)]
    ['-
     (l3-call args ':sub)]
    ['*
     (l3-call args ':mul)]
    ['<
     (l3-call args ':lt)]
    ['<=
     (l3-call args ':lte)]
    ['=
     (l3-call args ':eq)]))

(define (compile-new-tuple args)
  (cons
   `(eax <- (allocate ,(length args) 0))
   (foldl (lambda (x result)
            (append
             result
             (list `((mem eax ,(* 4 (length result))) <- ,x))))
            (list)
            args)))
  
(define (compile-l3d sexpr)
  (match sexpr
    [`(,(? biop? op) ,v1 ,v2)
     (compile-biop op (list v1 v2))]
    [`(number? ,v)
     (l3-call (list v) ':isNumber)]
    [`(a? ,v)
     (l3-call (list v) ':isArray)]
    [`(new-array ,v1 ,v2)
     (l3-call (list v1 v2) ':newarray)]
    [`(new-tuple ,v1 ...)
     (compile-new-tuple v1)]
    [`(aref ,v1 ,v2)
     (l3-call (list v1 v2) ':aref)]
    [`(aset ,v1 ,v2)
     (l3-call (list v1 v2) ':aset)]
    [`(alen ,v1)
     (l3-call (list v1) ':alen)]
    [`(print ,v1)
     (l3-call (list v1) ':print)]
    [`(,v1 ,v2 ...)
     (l3-call v2 v1)]
    [_
     (list `(eax <- ,sexpr))]))

(define label-suffix -1)
(define label-prefix ":iflabel")

(define (next-label)
  (begin
    (set! label-suffix (+ label-suffix 1))
    (string->symbol (string-append label-prefix (number->string label-suffix)))))

(define (compile-l3e sexpr)
  (match sexpr
    [`(if ,v ,e1 ,e2)
     (let ([true-label (next-label)]
           [false-label (next-label)])
       (append
        (list `(cjump ,v != 0 ,true-label ,false-label))
        (list true-label)
        (compile-l3e e1)
        (list false-label)
        (compile-l3e e2)))]
    [`(let ([,x ,d]) ,e)
     (append (compile-l3d d) (list `(,x <- eax)) (compile-l3e e))]
    [_
     (compile-l3d sexpr)]))

(define (l3-answer-rec list-of-args list-of-regs output)
  (if (empty? list-of-args)
      output
      (l3-answer-rec
       (rest list-of-args)
       (rest list-of-regs)
       (append output (list `(,(first list-of-args) <- ,(first list-of-regs)))))))

(define (l3-answer args)
  (l3-answer-rec args '(eax edx ecx) (list)))

(define (compile-l3f sexpr)
  (append
   (list (first sexpr))
   (l3-answer (second sexpr))
   (compile-l3e (third sexpr))))

(define (compile-l3p sexpr)
  (let ([translated-program (translate-l3-program sexpr)])
    (cons
     (compile-l3e (first translated-program))
     (map compile-l3f (rest translated-program)))))

;;
;; Compile the file specified on the command line
;;

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(compile-l3p (call-with-input-file filename read))
   