#lang planet neil/sicp

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; F(base, exp) = 2*F(base, exp/2) + O(1)
; use Master's Theorem, logb(a) = log2(2) = 1
; O(n^logb(a)) = O(n) > O(1)
; then is should be O(n) time complexity