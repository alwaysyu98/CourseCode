#lang planet neil/sicp

(define (cont-frac n d k)
  (define (iter n d k acc)
    (if (= k 0) acc
        (iter n d (- k 1) (/ (n k) (+ (d k) acc)))))
  (iter n d k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           5)