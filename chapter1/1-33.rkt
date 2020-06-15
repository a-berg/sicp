#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")

(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter? a)
                           (combiner result (term a))
                           (combiner result null-value)))))
  (iter a null-value))

(filtered-accumulate + 0 identity 0 ++ 10 odd?)

(define (relative-prime? i n)
  (= (gcd i n) 1))

(filtered-accumulate + 0 identity 0 ++ 10 (lambda (i) (relative-prime? i 10)))
