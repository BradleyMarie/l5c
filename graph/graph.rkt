;; Northwestern University
;; EECS 322
;; Graph
;;
;; By Brad Weinberger & Ethan Romba
;; April 21, 2012

#lang plai
(require racket/set)

;;
;; Helpers
;;

(define registers (set 'eax 'ebx 'ecx 'edi 'edx 'esi))

(define (register? sym) (set-member? registers sym))

(define (variable? sym) (not (register? sym)))

(define (symbol-by-name<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

;;
;; Graph to Register Assignments
;;

(define (variable-vertex? graph-vertex)
  (variable? (first graph-vertex)))

(define (vertex-register-mapping graph-vertex)
  (let ([mapped-register 
         (sort (set->list (set-subtract registers (list->set (rest graph-vertex))))
               symbol-by-name<?)])
    (if (empty? mapped-register)
        (void)
        (list (first graph-vertex) (first mapped-register)))))

(define (graph-register-mapping list-of-vertices)
  (let ([register-assignments 
         (map vertex-register-mapping 
              (filter variable-vertex? list-of-vertices))])
    (if (member (void) register-assignments)
        #f
        register-assignments)))