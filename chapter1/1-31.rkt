#lang sicp
(#%require rackunit)

(define (square x) (* x x))

(define (accum term a next b op neutral)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (op result (term a)))))
  (iter a neutral))

(define (prod term a next b)
  (accum term a next b * 1))

(define (compute-pi n-iter)
  (define (pi-next n) (+ 1 n))
  (define (pi-term n)
    (/ (* 2 2 n (+ n 1))
       (square (+ 1 (* 2 n)))))
  (* 4 (prod pi-term 1 pi-next n-iter)))

(check-= 3.1415926535 (exact->inexact (compute-pi 1000)) 1e-3)
