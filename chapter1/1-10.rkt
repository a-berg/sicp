#lang sicp
(#%require rackunit)


;; Definition of a modification of the Ackermann function.
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; Some values of the fonction
(check-equal? (A 1 10) (expt 2 10))
(A 2 4)  ;; 65536 = 2^(2^(2^2)) = 2^16
(A 3 3)  ;; 65536 = ?? (hyperoperation)

;; Therefore, when we define these functions:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
;; We are defining in fact:
;; f(n) = 2*n
;; g(n) = 2^n
;; h(n) = 2^...^2 (n times)
;; We could name h(n) as "tower exponentiation" and be done with it, maybe
;; assigning the ^^ symbol to denote this operation: 2^^3 = 8
;; As for the hypothetical case of A(3, n), this operation would possibly be
;; akin to 2^^...^^2 n times. In fact, let us define:
(define (^^ b n)
  (cond ((= n 0) 1)
        ((= n 1) b)
        (else (expt b (^^ b (- n 1))))))

(check-equal? (^^ 2 4) (A 2 4))
;; Now, the new operation (let's call it hyper) would be exactly the same, but
;; with `expt` substituted by `^^` and `^^` by `hyper`
(define (hyper b n)
  (cond ((= n 0) 1)
        ((= n 1) b)
        (else (^^ b (hyper b (- n 1))))))

(check-equal? (A 3 3) (hyper 2 3))
;; So, (A 3 n) = (hyper 2 n). One can presumably generalize this pattern,
;; although the `A` function already does this (but only for base 2). For
;; further information, read about Ackermann function (also the Ackermann phi
;; function, which is exactly what the generalization of these ideas is).
