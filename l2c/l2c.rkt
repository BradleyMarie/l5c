;; Northwestern University
;; EECS 322
;; L2c
;;
;; By Brad Weinberger & Ethan Romba
;; April 30, 2012

#lang racket

(define DEVELOPMENT #f)

(require "spill-lib.rkt")
(require "liveness-lib.rkt")
(require "graph-lib.rkt")
(require racket/cmdline)
(require test-engine/racket-tests)

(define registers '(eax ebx ecx edi edx esi))

(define (max-live-simultaneously liveness)
  (let ([maximum (length (second (first liveness)))])
    (for ([outs (rest (second liveness))])
      (set! maximum (max maximum (length outs))))
    maximum))

(check-expect (max-live-simultaneously '((in (eax ebx x) (eax)) (out (eax x) ()))) 3)
(check-expect (max-live-simultaneously '((in (eax) (eax ebx x)) (out (eax x) ()))) 2)
(check-expect (max-live-simultaneously '((in (eax) ()) (out (eax x) () (eax ebx x)))) 3)

(define (is-spilled? variable)
  (regexp-match? "s_" (symbol->string variable)))

(check-expect (is-spilled? 'test) #f)
(check-expect (is-spilled? 's_test) #t)

;; Retrieves a list of variables that have not yet been spilled
(define (get-variables liveness)
  (filter-not is-spilled?
              (remove-duplicates (remove* registers
                                          (flatten (map rest
                                                        liveness))))))

(check-expect (get-variables '((in (eax ebx s_x) (eax)) (out (eax x) ()))) '(x))
(check-expect (get-variables '((in (eax ebx x) (eax)) (out (eax x) ()))) '(x))
(check-expect (get-variables '((in (eax ebx x) (eax y)) (out (eax x) (a b)))) '(x y a b))

(define (choose-var-to-spill variables)
  (list-ref variables (random (length variables))))

(check-member-of (choose-var-to-spill '(a b c d)) 'a 'b 'c 'd)

(define (rewrite-variables function coloring)
  (if (empty? coloring)
      function
      (let ([variable (first (first coloring))]
            [register (second (first coloring))])
        (rewrite-variables (replace-list-elements function variable register) (rest coloring)))))

(check-expect (rewrite-variables '((x <- 1) (eax += x) (return)) '((x ebx)))
                 '((ebx <- 1) (eax += ebx) (return)))

(define (insert-esp-adjustment function num-spills)
  (if (zero? num-spills)
      function
      (list* (list 'esp '-= (* -4 num-spills)) function)))

(check-expect (insert-esp-adjustment '((ebx <- 1) (eax += ebx) (return)) 0)
                 '((ebx <- 1) (eax += ebx) (return)))
(check-expect (insert-esp-adjustment '((ebx <- 1) (eax += ebx) (return)) 2)
                 '((esp -= -8) (ebx <- 1) (eax += ebx) (return)))

(define (spill function num-spills liveness)
  (let ([variables (get-variables liveness)])
    (if (empty? variables)
        #f
        (let* ([var-to-spill (choose-var-to-spill variables)]
               [offset (* -4 (+ num-spills 1))]
               [spilled-function (spill-function var-to-spill offset 's_)])
          (compile-function-rec spilled-function (+ num-spills 1))))))

(define (compile-function-rec function num-spills)
  (let* ([liveness (liveness-analysis function)]
        [variables (get-variables liveness)])
    (if (> (max-live-simultaneously liveness) 6)
        ;; Too many variables live at the same time -- Spill
        (spill function num-spills liveness variables)
        (let ([coloring (generate-colored-graph (generate-interference-graph function))])
          (if (false? coloring)
              ;; Bad coloring -- Spill
              (spill function num-spills liveness)
              ;; Good coloring -- Finish the translation
              (insert-esp-adjustment (rewrite-variables function coloring) num-spills))))))

(define (compile-function function)
  (compile-function-rec function 0))
                                
(define (compile-program program)
  (map compile-function program))

(if DEVELOPMENT
    (test)
    (let ([filename
        (command-line
         #:args (filename) filename)])
      (display (compile-program (call-with-input-file filename read)))))