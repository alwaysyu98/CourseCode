#lang planet neil/sicp
(define (new-if1 predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-if2 predicate then-clause else-clause)
  (if predicate then-clause else-clause))



; as it is introduced in ex1.5, if will calculate predicate expression first, then choose whether
; then clause or else clause to execute. But if you redefine your onw "new-if", the computer will
; execute both then clause and else clause. So, the answer is the progress will not stop because
; its infinitely self-invoke.

; Let's see some examples, they show the differences.

(display "new-if1 start\n")
(new-if1 (< 1 2) (display 9) (display 10))
(display "new-if1 end\n")

(display "new-if2 start\n")
(new-if2 (< 1 2) (display 9) (display 10))
(display "new-if2 end\n")

(display "origin if start\n")
(if (< 1 2) (display 9) (display 10))
(display "origin if end\n")
