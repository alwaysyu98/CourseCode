#lang planet neil/sicp

(define (square x) (* x x ))

(define (good-enough? guess x)
   (< (abs (- (square guess) x)) 0.001))

(define (sqrt x good-enough?)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
  (sqrt-iter 1.0 x))

; see the following test cases
; 1) should be #f, but #t
; because the large number' representation in computer will neglect very little number
(good-enough? 1e32 (+ (square 1e32) 0.01))

; 2) shoule be 1e-16, but 0.03125 in my computer
; because the iter will terminate at first several iterations.
(sqrt 1e-32 good-enough?)

; 3) shoule be 1e16, seems good
(sqrt 1e32 good-enough?)

; good-enough?-pro1
(define (good-enough?-pro1 guess lastguess)
   (< (abs (- guess lastguess)) 0.001))

; good-enough?-pro2
(define (good-enough?-pro2 guess lastguess)
   (= (abs (- guess lastguess)) 0))

(define (sqrt-pro x good-enough?)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (sqrt-iter guess lastguess x)
    (if (good-enough? guess lastguess)
        guess
        (sqrt-iter (improve guess x) guess
                   x)))
  (sqrt-iter (improve 1.0 x) 1.0 x))

; use good-enough?-pro1
(sqrt-pro 1e-16 good-enough?-pro1)
(sqrt-pro 1e16 good-enough?-pro1)

; use good-enough?-pro2
(sqrt-pro 1e-16 good-enough?-pro2)
(sqrt-pro 1e16 good-enough?-pro2)