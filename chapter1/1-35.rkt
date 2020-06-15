#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.5))

(check-= 1.6180339887 phi 1e-10)
