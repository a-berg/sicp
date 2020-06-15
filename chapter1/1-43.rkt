#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")

(define (compose f g)
  (lambda (x) (f (g x))))

(define (power-op b n op e)
  (define (square x) (op x x))
  (define (halve x) (/ x 2))
  (define (even? x) (= (remainder x 2) 0))

  (define (power-iter b n result)
    (if (= n 0)
        result
        (if (even? n)
            (power-iter (square b) (halve n) result)
            (power-iter (square b) (halve (- n 1)) (op result b)))))

  (power-iter b n e))

(define (repeated f n)
  ;; I can't believe its not a monad!
  (power-op f n compose identity))
