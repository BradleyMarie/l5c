;; Northwestern University
;; EECS 322
;; Liveness
;;
;; By Brad Weinberger & Ethan Romba
;; April 21, 2012

#lang racket

;;
;; Kick off the liveness analysis
;;

(require "liveness-lib.rkt")
(require racket/cmdline)

(define filename
  (command-line
   #:args (filename) filename))

(display (liveness-analysis (call-with-input-file filename read)))