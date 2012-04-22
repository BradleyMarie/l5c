;; Northwestern University
;; EECS 322
;; Graph
;;
;; By Brad Weinberger & Ethan Romba
;; April 21, 2012

#lang racket
(require "graph-lib.rkt")
(require racket/cmdline)

(define filename
  (command-line
   #:args (filename) filename))

(generate-graph (call-with-input-file filename read))