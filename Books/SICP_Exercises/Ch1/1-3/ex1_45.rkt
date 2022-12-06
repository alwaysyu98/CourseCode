#lang planet neil/sicp

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (fast-pow b p)
  (define (iter b p acc)
    (cond ((= p 0) acc)
      ((odd? p) (iter (* b b) (/ (- p 1) 2) (* acc b)))
      (else (iter (* b b) (/ p 2) acc))))
  (iter b p 1))


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x)
        (f ((repeated f (- n 1)) x)))))

(define (n-th-roots x n)
  (fixed-point ((repeated average-damp (- n 1)) (lambda (y) (/ x (fast-pow y (- n 1))))) 1.0))

; I'm not sure how many average-damps I should use.

; test cases 
(n-th-roots 16 4)
(n-th-roots 64 4)
(n-th-roots (fast-pow 2 6) 6)
(n-th-roots (fast-pow 2 6) 6)
(n-th-roots (fast-pow 2 8) 8)