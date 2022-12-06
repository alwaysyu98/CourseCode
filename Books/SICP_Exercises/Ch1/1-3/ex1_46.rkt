#lang planet neil/sicp

(define (average x y)
  (/ (+ x y) 2))
(define (square x)
  (* x x))

(define (iterative-imporve good-enough? imporve)
  (define (iter-impove guess)
    (let ((next (imporve guess)))
          (if (good-enough? guess)
              next
              (iter-impove next))))
  iter-impove)

(define (sqrt x)
  ((iterative-imporve
   (lambda (guess)
     (< (abs (- (square guess) x)) 0.001))
   (lambda (y)
     (average y (/ x y))))
  1.0))

; test case
(sqrt 16)

(define (fixed-point f first-guess)
  ((iterative-imporve
   (lambda (x)
     (< (abs (- (f x) x)) 0.001))
   f)
  first-guess))

; test case
(fixed-point cos 1.0)