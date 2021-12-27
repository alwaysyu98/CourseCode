#lang planet neil/sicp

(define (double f)
  (lambda (x)
    (f (f x))))

; 21
(((double (double double)) inc) 5)
; double(double(double)) (inc) = double(double(double(double(inc))))

; 13
((double (double (double inc))) 5)
; double(double(double(inc))) = double(double(double(inc)))

; above two evaluations are different.