#lang sicp
(#%require rackunit)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (+ a b) n))
  (define (simpson-next x) (+ x (* 2 h)))
  (* (+ (* 2 (sum f (+ a (* 2 h)) simpson-next b)) ;; y even
        (* 4 (sum f (+ a h) simpson-next b)) ;; y odd
        (f a);; y0
        (f b);; yn
        )
     (/ h 3)))
