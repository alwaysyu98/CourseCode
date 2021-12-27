#lang planet neil/sicp

(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x)
        (f ((repeated f (- n 1)) x)))))

(define dx 0.000001)
(define (smooth f x)
  (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))

(define (n-fold-smooth f x n)
  ((repeated smooth n) f x))