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

(define (tan-cf x k)
  (define (d i) (+ 1 (* 2 i)))
  (define (n i)
    (if (= i 0)
        x
        (- 0 (square x))))
  (cont-frac n d k))
