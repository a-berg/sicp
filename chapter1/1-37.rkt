#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")


(define (cont-frac n d k)
  (define (f i result)
    (/ (n i) (+ result (d i))))
  (define (iter i result)
    (if (> 0 i)
        result
        (iter (-- i) (f i result))))
  (iter k (/ (n k) (d k))))


(define phi-exact (halve (+ 1 (sqrt 5))))
(define phi (+ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

(check-= phi-exact phi 1e-50)
