#lang sicp
(#%require "chapter2_functions.rkt")

;; (define (make-rat n d)
;;   (let ((g (gcd n d))
    ;;     (sign (/ (* n d) (abs (* n d)))))
    ;; (cons (* sign (abs (/ n g) )) (abs (/ d g)))))

(print-rat (make-rat -5 -10))
(print-rat (make-rat -5 10))
(print-rat (make-rat 5 -10))
(print-rat (make-rat 5 10))
