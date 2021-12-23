#lang planet neil/sicp

(define (cbrt x)
  (define (square x) (* x x))
  (define (good-enough? guess lastguess)
    (< (abs (- guess lastguess)) 0.001))
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cbrt-iter guess lastguess x)
    (if (good-enough? guess lastguess)
        guess
        (cbrt-iter (improve guess x) guess
                   x)))
  (cbrt-iter (improve 1.0 x) 1.0 x))


; let's try
(cbrt 8)
(cbrt 1e18)
(cbrt 1e-18)
