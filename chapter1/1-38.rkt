#lang sicp
(#%require rackunit)
(#%require "common_sicp_funcs.rkt")

(define (pseudo-list i)
  (cond ((= i 0)  1)
        ((= i 1)  2)
        ((= i 2)  1)
        ((= i 3)  1)
        ((= i 4)  4)
        ((= i 5)  1)
        ((= i 6)  1)
        ((= i 7)  6)
        ((= i 8)  1)
        ((= i 9)  1)
        ((= i 10) 8)))

(define (cont-frac n d k)
  (define (f i result)
    (/ (n i) (+ result (d i))))
  (define (iter i result)
    (if (> 0 i)
        result
        (iter (-- i) (f i result))))
  (iter k (/ (n k) (d k))))

(define e (+ 2 (cont-frac (lambda (i) 1.0)
                          (lambda (i) (pseudo-list i))
                                  10)))

(define e-exact (exp 1))
(check-= e-exact e 1e-8)

(define (list-iterator L)
  (lambda (i) (list-ref L i)))

(define ee (+ 2 (cont-frac (lambda (i) 1.0)
                           (list-iterator (list 1 2 1 1 4 1 1 6 1 1 8))
                           10)))
