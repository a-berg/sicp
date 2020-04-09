#lang sicp
(#%require rackunit)

(define (power-op b n op e)
  (define (square x) (op x x))
  (define (halve x) (/ x 2))
  (define (even? x) (= (remainder x 2) 0))

  (define (power-iter b n a)
    (if (= n 0)
        a
        (if (even? n)
            (power-iter (square b) (halve n) a)
            (power-iter (square b) (halve (- n 1)) (op a b)))))

  (power-iter b n e))

(check-equal? (power-op 3 2 + 0) 6) ;; 3*2 = 6
(check-equal? (power-op 3 2 * 1) 9) ;; 3^2 = 9

(define (expt b n) (power-op b n * 1))
(define (mult a b) (power-op a b + 0))

(power-op 3 2 * 4) ;; 36 = 3^2*4
(power-op 3 2 + 4) ;; 10 = 3*2+4

(define (↑↑ base exponent height) (power-op exponent height expt base))
