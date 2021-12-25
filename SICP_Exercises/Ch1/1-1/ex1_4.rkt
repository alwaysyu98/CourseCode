#lang planet neil/sicp
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; it will use the result from (> b 0) to determine which operator (+ or -) will be used.
