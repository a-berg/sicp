#lang sicp
(#%require rackunit)

(define (square x) (* x x))

(define (sqrt-iter guess x prev-guess)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) x guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
  (< (abs (- (/ prev-guess guess) 1)) 0.001))

(define (sqrt x)
  (sqrt-iter (improve 1.0 x) x 1.0))

; Test
(check-= 3 (sqrt 9) 1e-4)
(check-= 0.1 (sqrt 0.01) 1e-4)
(check-= 35136418411178.91 (sqrt 1234567898765432101010101010) 1e-4)
(check-= (/ 35136418411178.91 (sqrt 1234567898765432101010101010)) 1.0 1e-4)
