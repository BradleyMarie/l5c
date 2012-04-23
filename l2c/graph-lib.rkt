;; Northwestern University
;; EECS 322
;; Graph
;;
;; By Brad Weinberger & Ethan Romba
;; April 21, 2012

#lang racket
(require racket/set)
(require "liveness-lib.rkt")

;;
;; Helpers
;;

(define registers (set 'eax 'ebx 'ecx 'edi 'edx 'esi))

(define (register? sym) (set-member? registers sym))

(define (variable? sym) 
  (if (and (not (register? sym)) (symbol? sym))
      (match (symbol->string sym)
        [(regexp #rx"^[a-zA-Z_-][a-zA-Z_0-9-]*$") #t]
        [_ #f])
      #f))

(define (symbol-by-name<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (list-by-first<? l1 l2)
  (string<? (symbol->string (first l1)) (symbol->string (first l2))))

(define (list-first-variable? l1)
  (variable? (first l1)))

(define (sop? expr)
  (set-member? (set '<<= '>>=) expr))

;;
;; Interference Graph Generation
;;

(define base-interference-graph
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

(define (add-pair-to-interference-graph pair-of-symbols graph)
  (let ([car-value (hash-ref graph (car pair-of-symbols) (set))]
        [cdr-value (hash-ref graph (cdr pair-of-symbols) (set))])
    (hash-set
     (hash-set graph 
               (cdr pair-of-symbols) 
               (set-add cdr-value (car pair-of-symbols)))
     (car pair-of-symbols)
     (set-add car-value (cdr pair-of-symbols)))))
  
(define (add-symbols-to-interference-graph symbols graph)
  (foldl (lambda (pair modified-graph) 
           (add-pair-to-interference-graph pair modified-graph))
           graph
           (list->list-of-pairs symbols)))

(define (function-interference-graph first-instruction-ins outs graph)
  (foldl (lambda (instruction-outs modified-graph)
           (add-symbols-to-interference-graph instruction-outs modified-graph))
         (add-symbols-to-interference-graph first-instruction-ins graph)
         outs))

(define (instruction-interference-graph list-of-instructions)
  (foldl (lambda (instruction modified-graph)
           (match instruction
             ; (x sop= sx) ;; update x with a shifting op and sx.
             [`(,ignore ,(? sop?) ,(? variable? read))
              (add-symbols-to-interference-graph (list* read (set->list (set-subtract registers (set 'ecx)))) modified-graph)]
             
             [_ modified-graph]))
         base-interference-graph
         list-of-instructions))
;;
;; Graph to Register Assignments
;;

(define base-coloring-graph
  (make-immutable-hash
   (list
    (cons 'eax  (cons 'eax (set-subtract registers (set 'eax))))
    (cons 'ebx  (cons 'ebx (set-subtract registers (set 'ebx))))
    (cons 'ecx  (cons 'ecx (set-subtract registers (set 'ecx))))
    (cons 'edx  (cons 'edx (set-subtract registers (set 'edx))))
    (cons 'edi  (cons 'edi (set-subtract registers (set 'edi))))
    (cons 'esi  (cons 'esi (set-subtract registers (set 'esi)))))))

(define (get-vertex-color vertex-name graph)
  (car (hash-ref graph vertex-name)))

(define (color-vertex register-adjacency-set)
  (let ([available-colors 
         (sort (set->list (set-subtract registers register-adjacency-set))
               symbol-by-name<?)])
    (if (empty? available-colors)
        (void)
        (first available-colors))))
  
(define (find-vertex-color adjacency-set coloring-graph)
  (color-vertex (foldl
                 (lambda (entry output)
                   (if (register? entry)
                       (set-add output entry)
                       (set-add output (get-vertex-color entry coloring-graph))))
                 (set)
                 (set->list adjacency-set))))

(define (add-colored-vertex-to-graph variable adjacency-set old-coloring-graph)
  (let ([vertex-color (find-vertex-color adjacency-set old-coloring-graph)])
    (if (eq? vertex-color (void))
        #f
        (foldl (lambda (element modified-coloring-graph)
                 (hash-update modified-coloring-graph element
                              (lambda (value) (cons (car value) (set-add (cdr value) variable)))))
               (hash-set old-coloring-graph variable (cons vertex-color adjacency-set))
               (set->list adjacency-set)))))

(define (remove-vertex-from-interference-graph variable interference-graph)
  (foldl (lambda (element modified-coloring-graph)
           (hash-update modified-coloring-graph element
                        (lambda (value) (set-remove value variable))))
         (hash-remove interference-graph variable)
         (set->list (hash-ref interference-graph variable))))

(define (rank-vertices v1 v2)
  (cond
    [(register? (car v1)) #f]
    [(register? (car v2)) #t]
    [(and (<= (cdr v1) 5) (<= (cdr v2) 5)) (>= (cdr v1) (cdr v2))]
    [(and (> (cdr v1) 5) (> (cdr v2) 5)) (< (cdr v1) (cdr v2))]
    [(<= (cdr v1) 5) #t]
    [else #f]))
    

(define (select-vertex-to-remove interference-graph)
  (car (first (sort (hash-map interference-graph (lambda (key value) 
                                     (cons key (set-count value)))) rank-vertices))))

(define (sorted-variable-list-rec interference-graph output)
  (if (equal? base-interference-graph interference-graph)
      output
      (let ([to-remove (select-vertex-to-remove interference-graph)])
         (sorted-variable-list-rec
          (remove-vertex-from-interference-graph to-remove interference-graph)
          (append output (list to-remove))))))

(define (sorted-variable-list interference-graph)
  (sorted-variable-list-rec interference-graph (list)))

(define (get-sorted-vertex-list-rec sorted-variables-in-list-form interference-graph output-list)
  (if (empty? sorted-variables-in-list-form)
      output-list
      (get-sorted-vertex-list-rec
       (rest sorted-variables-in-list-form)
       (remove-vertex-from-interference-graph (first sorted-variables-in-list-form) interference-graph)
       (append (list (cons (first sorted-variables-in-list-form) 
                                       (hash-ref interference-graph (first sorted-variables-in-list-form)))) output-list))))

(define (get-sorted-vertex-list sorted-variables-in-list-form interference-graph)
  (get-sorted-vertex-list-rec sorted-variables-in-list-form interference-graph (list)))
  
(define (color-interference-graph-rec interference-graph sorted-vertex-list colored-graph)
  (if (empty? sorted-vertex-list)
      colored-graph
      (let ([modified-graph (add-colored-vertex-to-graph (car (first sorted-vertex-list))
                                                         (cdr (first sorted-vertex-list))
                                                         colored-graph)])
        (if (eq? modified-graph #f)
                 #f
                 (color-interference-graph-rec (remove-vertex-from-interference-graph (car (first sorted-vertex-list)) interference-graph)
                                               (rest sorted-vertex-list)
                                               modified-graph)))))
                                               
(define (color-interference-graph interference-graph)
  (color-interference-graph-rec
   interference-graph
   (get-sorted-vertex-list (sorted-variable-list interference-graph) interference-graph)
   base-coloring-graph))

;;
;; Output formatting Code
;;

(define (format-interference-graph graph)
  (sort
   (hash-map
    graph
    (lambda (key value) (list* key (sort (set->list value) symbol-by-name<?))))
   list-by-first<?))

(define (format-colored-graph graph)
  (if (eq? #f graph)
      #f
      (sort 
       (filter
        list-first-variable?
        (hash-map
         graph
         (lambda (key value) (list key (car value)))))
       list-by-first<?)))

;;
;; Kick off the graph coloring
;;

(define (generate-interference-graph sexpr)
  (let ([liveness-results (liveness-analysis sexpr)])
    (format-interference-graph (function-interference-graph (second (first liveness-results)) (rest (second liveness-results))
                           (instruction-interference-graph sexpr)))))

(provide generate-interference-graph)

(define (translate-external-interference-graph sexpr)
  (make-immutable-hash (map (lambda (vertex) (cons (first vertex) (list->set (rest vertex)))) sexpr)))

(define (generate-colored-graph sexpr)
  (format-colored-graph 
   (color-interference-graph
    (translate-external-interference-graph sexpr))))

(provide generate-colored-graph)