#lang racket ;; doesn't work with #lang sicp

(provide
   square
   ++
   --
   halve
   fl-eq?
   average
   fixed-point
   average-damp
   newtons-method
   fixed-point-of-transform
   repeated)

;;; ???
(define (square x) (* x x))
(define (halve x) (/ x 2))
(define (++ n) (+ 1 n))
(define (-- n) (- n 1))
(define (fl-eq? x y eps)
  (< (abs (- x y)) eps))
(define (average . args) (/ (apply + args) (length args)))

;;; 1.3.3 Procedures as general Methods
;;;; Search / half interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if ((lambda (x y) (fl-eq? x y 1e-3)) neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;;;; Fixed point of function
(define tolerance 1e-10)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (fl-eq? v1 v2 tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;; 1.3.4 Procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))

;;;; Newton's method
(define dx 1e-5)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;;; Abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; This be useful on exercises 44-46 so am pasting it there yo.
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
  (power-op f n compose identity))
