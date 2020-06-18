#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")

;; Experimenting a bit
(define (integer-log n) (inexact->exact (floor (log n 2))))
(define (nroot x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (integer-log n))
                            1.0))
(nroot 2 101)
