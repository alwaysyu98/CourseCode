#lang planet neil/sicp

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; it is needed to compute large intermediate number