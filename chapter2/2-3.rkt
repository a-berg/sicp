#lang sicp
(#%require rackunit)
(#%require "chapter2_functions.rkt")
(#%require "2-2.rkt")

;; No, I don't think I will
(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left rectangle)
  (car rectangle))

(define (top-right rectangle)
  (make-point (x-point bottom-right) (y-point top-left)))

(define (bottom-right rectangle)
  (cdr rectangle))

(define (bottom-left rectangle)
  (make-point (x-point top-left) (y-point bottom-right)))

(define (rectangle-width rectangle)
  (abs (- (x-point (top-left rectangle))
          (x-point (bottom-right rectangle)))))

(define (rectangle-height rectangle)
  (abs (- (y-point (top-left rectangle))
          (y-point (bottom-right rectangle)))))

(define (rectangle-perimeter rectangle)
  (* 2 (+ (rectangle-width rectangle) (rectangle-height rectangle))))

(define a (make-point 0.0 1.0))
(define b (make-point 1.0 0.0))
(define R (make-rectangle a b))

(rectangle-perimeter R) ;; 4 as expected
