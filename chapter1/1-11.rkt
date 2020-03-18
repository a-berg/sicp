#lang sicp
(#%require rackunit)


(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f-impr n)
  (define (iter a b c counter)
    (if (= counter 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- counter 1))))
  (iter 2 1 0 n))

(check-equal? (f 20) (f-impr 20))
