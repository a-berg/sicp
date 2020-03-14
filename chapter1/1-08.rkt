#lang sicp
(#%require rackunit)

(define (cube x) (* x x x))

(define (cubrt-iter guess x prev-guess)
  (if (good-enough? guess prev-guess)
      guess
      (cubrt-iter (improve guess x) x guess)))

;; improved-guess = (x / guess**2 + 2*guess)/3
(define (improve guess x)
  (average (* 2 guess) (/ x (* guess guess))))

(define (average x y)
  (/ (+ x y) 3))

(define (good-enough? guess prev-guess)
  (< (abs (- (/ prev-guess guess) 1)) 0.001))

(define (cubrt x)
  (cubrt-iter (improve 1.0 x) x 1.0))

; Test
(check-= 3 (cubrt 27) 1e-4)
(check-= 0.1 (cubrt 1e-3) 1e-4)
