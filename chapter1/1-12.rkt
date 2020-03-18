#lang sicp
(#%require rackunit)

;; Given an n (level) and an m (position in that level), compute Pascal's
;; triangle.
;; I don't check for bounds.
(define (PT n m)
  (cond ((< n 2) 1)
        ((= m 0) 1)
        ((= m n) 1)
        (else (+ (PT (- n 1) m)
                 (PT (- n 1) (- m 1))))))

(check-equal? (PT 3 2) 3)
(check-equal? (PT 5 2) 10)
(check-equal? (PT 5 4) 5)

;; Improvement here:

