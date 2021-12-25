#lang planet neil/sicp

; smallest-divisor
(define (square n) (* n n))

; expmod
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; fermat-test
(define (fermat-test n a)
  (cond ((= a n) true)
        ((= (expmod a n n) a) (fermat-test n (+ a 1)))
        (else false)))

; fast-prime?
(define (fast-prime? n)
  (fermat-test n 2))
  

; test case
(fast-prime? 561)
(fast-prime? 1105)
(fast-prime? 1729)
(fast-prime? 2465)
(fast-prime? 2821)
(fast-prime? 6601)