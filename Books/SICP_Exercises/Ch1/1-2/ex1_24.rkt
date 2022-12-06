#lang planet neil/sicp

; smallest-divisor
(define (next n)
  (if (= n 2) 3
      (+ n 2)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (square n) (* n n))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

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
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; fast-prime?
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; prime?
(define (prime? n)
  (fast-prime? n 100))

; timed-prime-test
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))


; https://docs.racket-lang.org/reference/if.html
; For cond and if details, please refer to above official website.
; The then-bodys are evaluated in order, and the
; results from all but the last then-body are ignored.

; use if and use cond to implement
(define (search-for-primes start end)
  (define (search-for-primes-iter start end)
    ;(cond ((<= start end) (timed-prime-test start) (search-for-primes-iter (+ start 2) end))))
    (if (<= start end) ((lambda () (timed-prime-test start) (search-for-primes-iter (+ start 2) end)))))
  (search-for-primes-iter
   (if (odd? start) start (+ start 1))
   end))
      

; test case
(search-for-primes 1000      1020)
(search-for-primes 10000     10038)
(search-for-primes 100000    100045)
(search-for-primes 1000000   1000030)

; 1009 *** 125
; 1013 *** 127
; 1019 *** 132
; 10007 *** 160
; 10009 *** 156
; 10037 *** 157
; 100003 *** 183
; 100019 *** 186
; 100043 *** 186
; 1000003 *** 205
; 1000033 *** 207
; 1000037 *** 256
; 1000039 *** 217

