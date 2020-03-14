#lang sicp

; Exercise 1.6
;; Using cond in this particular function causes an infinite loop because it
;; gets expanded due to applicative-order evaulation, and the expansion has a
;; call to sqrt-iter itself. On the contrary, if doesn't fully expand, as it
;; "normally evaluates" because it's special. (or this is what I understood)
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; Original
;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))
; Exercise 1.7
(define (good-enough? guess prev-guess)
  (< (abs (- (/ prev-guess guess) 1)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Test
(define (similar? x y) (< (abs (- x y)) 1e-4))

(similar? 3 (sqrt 9))
(similar? 0.1 (sqrt 0.01))
