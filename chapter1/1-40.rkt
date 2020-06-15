#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")

(define (cubic a b c) ;; Takes 3 numbers, returns a function that computes the
                      ;; 3rd grade polynomial defined as x³+ax²+bx+c.
  (lambda (x) (+ (expt x 3) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 -3) 1)
