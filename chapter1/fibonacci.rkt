#lang sicp
(#%require rackunit)


(define (fib-naive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-naive (- n 1))
                 (fib-naive (- n 2))))))

(check-equal? 0 (fib-naive 0))
(check-equal? 1 (fib-naive 1))
(check-equal? 21 (fib-naive 8))

(define (fib n)
  (define (iter a b counter)
    (if (= counter 0)
        b
        (iter (+ a b) a (- counter 1))))
  (iter 1 0 n))

(check-equal? 0 (fib 0))
(check-equal? 1 (fib 1))
(check-equal? 21 (fib 8))

;; "tribonacci" sequence.
(define (trib-naive n)
  (if (< n 3)
      n
      (+ (trib-naive (- n 1))
         (trib-naive (- n 2))
         (trib-naive (- n 3)))))

(trib-naive 8)

(define (trib n)
  (define (iter a b c counter)
    (if (= counter 0)
        c
        (iter (+ a b c) a b (- counter 1))))
  (iter 2 1 0 n))

(trib 8)
