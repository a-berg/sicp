#lang sicp
(#%require rackunit)
(#%require "chapter2_functions.rkt")

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p-start p-end)
  (cons p-start p-end))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (let ((midx (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2))
        (midy (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))
    (make-point midx midy)))

(#%provide print-point
           make-point x-point y-point
           make-segment start-segment end-segment
           midpoint-segment)
