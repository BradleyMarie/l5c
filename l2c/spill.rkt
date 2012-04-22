;; Northwestern University
;; EECS 322
;; Spill
;;
;; By Brad Weinberger & Ethan Romba
;; April 21, 2012

#lang racket

;;
;; Compile the file specified on the command line
;;

(require "spill-lib.rkt")
(require racket/cmdline)
(define filename
  (command-line
   #:args (filename) filename))

(call-with-input-file filename
  (lambda (p)
    (display (spill-function (read p) (read p) (read p) (read p)))))
