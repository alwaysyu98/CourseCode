#lang planet neil/sicp

(define (cont-frac n d k)
  (define (iter n d k acc)
    (if (= k 0) acc
        (iter n d (- k 1) (/ (n k) (+ (d k) acc)))))
  (iter n d k 0))

(define (tan x)
  (cont-frac (lambda (i)
               (cond ((= i 1) x)
                     (else (- (* x x)))))
             (lambda (i) (- (* 2 i) 1))
             100))
(define pi 3.1415926)
(tan (/ pi 4))