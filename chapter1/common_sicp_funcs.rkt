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
   fixed-point-of-transform)

;;; ???
(define (square x) (* x x))
(define (halve x) (/ x 2))
(define (++ n) (+ 1 n))
(define (-- n) (- n 1))
(define (fl-eq? x y eps)
  (< (abs (- x y)) eps))
(define (average x y) (/ (+ x y) 2))

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

;;; Exercise 1.37
;; (define (cont-frac n d k)
;;   (define (f i result)
;;     (/ (n i) (+ result (d i))))
;;   (define (iter i result)
;;     (if (> 0 i)
;;         result
;;         (iter (-- i) (f i result))))
;;   (iter k (/ (n k) (d k))))
