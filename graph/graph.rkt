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

(define (list-by-first<? l1 l2)
  (string<? (symbol->string (first l1)) (symbol->string (first l2))))

;;
;; Graph Generation
;;

(define base-graph
  (make-immutable-hash
   (list
    (cons 'eax (set-subtract registers (set 'eax)))
    (cons 'ebx (set-subtract registers (set 'ebx)))
    (cons 'ecx (set-subtract registers (set 'ecx)))
    (cons 'edx (set-subtract registers (set 'edx)))
    (cons 'edi (set-subtract registers (set 'edi)))
    (cons 'esi (set-subtract registers (set 'esi))))))

(define (list->list-of-pairs list-of-symbols)
  (if (empty? list-of-symbols)
      (list)
      (append (map (lambda (element) 
                     (cons (first list-of-symbols) element)) 
                   (rest list-of-symbols))
              (list->list-of-pairs (rest list-of-symbols)))))

(define (add-pair-to-graph pair-of-symbols graph)
  (let ([car-value (hash-ref graph (car pair-of-symbols) (set))]
        [cdr-value (hash-ref graph (cdr pair-of-symbols) (set))])
    (hash-set
     (hash-set graph 
               (cdr pair-of-symbols) 
               (set-add cdr-value (car pair-of-symbols)))
     (car pair-of-symbols)
     (set-add car-value (cdr pair-of-symbols)))))
  
(define (add-symbols-to-graph symbols graph)
  (foldl (lambda (pair modified-graph) 
           (add-pair-to-graph pair modified-graph))
           graph
           (list->list-of-pairs symbols)))

(define (function-graph first-instruction-ins outs)
  (foldl (lambda (instruction-outs modified-graph)
           (add-symbols-to-graph instruction-outs modified-graph))
         (add-symbols-to-graph first-instruction-ins base-graph)
         outs))

;;
;; Graph to Register Assignments
;;
;
;(define (variable-vertex? graph-vertex)
;  (variable? (first graph-vertex)))
;
;(define (vertex-register-mapping graph-vertex)
;  (let ([mapped-register 
;         (sort (set->list (set-subtract registers (list->set (rest graph-vertex))))
;               symbol-by-name<?)])
;    (if (empty? mapped-register)
;        (void)
;        (list (first graph-vertex) (first mapped-register)))))
;
;(define (graph-register-mapping list-of-vertices)
;  (let ([register-assignments 
;         (map vertex-register-mapping 
;              (filter variable-vertex? list-of-vertices))])
;    (if (member (void) register-assignments)
;        #f
;        register-assignments)))

;;
;; Output formatting Code
;;

(define (format-graph graph)
  (sort
   (hash-map
    graph
    (lambda (key value) (list* key (sort (set->list value) symbol-by-name<?))))
   list-by-first<?))

;;
;; Kick off the graph coloring
;;

(define (start-graph-coloring sexpr)
  (display (format-graph (function-graph (second (first sexpr)) (rest (second sexpr))))))

(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(start-graph-coloring (call-with-input-file filename read))