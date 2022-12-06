#lang planet neil/sicp

; smallest-divisor
(define (square n) (* n n))

; expmod
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((root (expmod base (/ exp 2) m)))
           (if (and (= (square root) 1) (not (= root 1)) (not (= root (- m 1))))
               0
               (remainder (square root) m))))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

; fermat-test
(define (fermat-test n a)
  (cond ((= a n) true)
        ((= (expmod a (- n 1) n) 1) (fermat-test n (+ a 1)))
        (else false)))

; fast-prime?
(define (fast-prime? n)
  (fermat-test n 2))

; match or not
(define (test-fast-prime? n x)
  (newline)
  (if (eq? (fast-prime? n) x)
      (display "right")
      (display "wrong")))
  

; test case
(test-fast-prime? 3 true)
(test-fast-prime? 4 false)
(test-fast-prime? 5 true)
(test-fast-prime? 6 false)
(test-fast-prime? 7 true)
(test-fast-prime? 561 false)
(test-fast-prime? 1105 false)
(test-fast-prime? 1729 false)
(test-fast-prime? 2465 false)
(test-fast-prime? 2821 false)
(test-fast-prime? 6601 false)