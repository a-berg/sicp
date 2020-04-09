#lang sicp
(#%require rackunit)

(define (expt b n)
  (define (square x) (* x x))
  (define (halve x) (/ x 2))
  (define (even? x) (= (remainder x 2) 0))

  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((= n 1) (* a b))
          ((even? n) (expt-iter (square b) (halve n) a))
          (else (expt-iter (square b) (halve (- n 1)) (* a b)))))
  (expt-iter b n 1))

(check-equal? (expt 300 0) 1)
(check-equal? (expt 2 10) 1024)
(check-equal? (expt 10 10) 10000000000)
