#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")

(define dx 0.1)


(define (smooth f)
  (lambda (x) (average (f (+ x dx))
                       (f x)
                       (f (- x dx)))))

(define (supersmooth f n)
  (lambda (x) (((repeated smooth n) f) x)))
