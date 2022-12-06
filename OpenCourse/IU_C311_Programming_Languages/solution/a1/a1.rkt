#lang racket

; C311 - 2016?
; Date: 7th, Dec, 2022
; Writer Jingyi Yu

; test with test tool
; (require "a1-student-tests.rkt")
; (test-file #:file-name "a1.rkt")

; 1. Define and test a procedure countdown that takes a natural number and returns a list of the natural numbers less than or equal to that number, in descending order.

(define countdown
  (lambda (x)
  (cond
    ((zero? x) '(0))
    ((cons x (countdown (- x 1)))))))
(countdown 5)

; 2. Define and test a procedure insertR that takes two symbols and a list and returns a new list with the second symbol inserted after each occurrence of the first symbol. For this and later questions, these functions need only hold over eqv?-comparable structures.

(define insertR
  (lambda (a b c)
       (cond
         ((eqv? c null) '())
         ((eqv? (car c) a) (cons a (cons b (insertR a b (cdr c)))))
         (else (cons (car c) (insertR a b (cdr c)))))))
(insertR 'x 'y '(x z z x y x))

; 3. Define and test a procedure remv-1st that takes a a symbol and a list and returns a new list with the first occurrence of the symbol removed.

(define remv-1st
  (lambda (x y)
    (cond
      ((eqv? y null) '())
      ((eqv? (car y) x) (cdr y))
      (else (cons (car y) (remv-1st x (cdr y)))))))
(remv-1st 'x '(x y z x))
(remv-1st 'y '(x y z y x))

; 4. Define and test a procedure list-index-ofv? that takes an element and a list and returns the (base 0) index of that element in the list. A list missing that element will be considered bad data.

(define list-index-ofv?
  (lambda (a b)
    (letrec
        ((helper (lambda (a b index)
                   (cond
                     ((eqv? b null) -1)
                     ((eqv? (car b) a) index)
                     (else (helper a (cdr b) (add1 index)))))))
      (helper a b 0))))
(list-index-ofv? 'x '(x y z x x))
(list-index-ofv? 'x '(y z x x))


; 5. Define and test a procedure filter that takes a predicate and a list and returns a new list containing the elements that satisfy the predicate. A predicate is a procedure that takes a single argument and returns either #t or #f. The number? predicate, for example, returns #t if its argument is a number and #f otherwise. The argument satisfies the predicate, then, if the predicate returns #t for that argument.

(define filter
  (lambda (a b)
    (cond
      ((null? b) '())
      ((a (car b)) (cons (car b) (filter a (cdr b))))
      (else (filter a (cdr b))))))
(filter even? '(1 2 3 4 5 6))

; 6. Define and test a procedure zip that takes two lists and forms a new list, each element of which is a pair formed by combining the corresponding elements of the two input lists. If the two lists are of uneven length, then drop the tail of the longer one.
(define zip
  (lambda (a b)
    (cond
      ((null? a) '())
      ((null? b) '())
      (else (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))))
(zip '(1 2 3) '(a b c))
(zip '(1 2 3 4 5 6) '(a b c))
(zip '(1 2 3) '(a b c d e f))

; https://stackoverflow.com/questions/6006671/are-pair-and-list-different-in-scheme
; pair and list are different
; '(1 . 2) = (cons 1 2)
; '(1 2)   = (cons 1 (cons 2 nil)) 

; 7. Define and test a procedure map that takes a procedure p of one argument and a list ls and returns a new list containing the results of applying p to the elements of ls. Do not use Racket's built-in map in your definition.
(define map
  (lambda (a b)
    (cond
      ((null? b) '())
      (else (cons (a (car b)) (map a (cdr b)))))))
(map add1 '(1 2 3 4))

; 8. Define and test a procedure append that takes two lists, ls1 and ls2, and appends ls1 to ls2.
(define append
  (lambda (a b)
    (cond
      ((null? a) b)
      (else (cons (car a) (append (cdr a) b))))))
(append '(a b c) '(1 2 3))

; 9. Define and test a procedure reverse that takes a list and returns the reverse of that list.
(define reverse
  (lambda (a)
    (letrec
        ((helper (lambda (a stack)
                   (cond
                     ((null? a) stack)
                     (else (helper (cdr a) (cons (car a) stack)))))))
      (helper a '()))))
(reverse '(a 3 x))

; 10. Define and test a procedure fact that takes a natural number and computes the factorial of that number. The factorial of a number is computed by multiplying it by the factorial of its predecessor. The factorial of 0 is defined to be 1.
(define fact
  (lambda (n)
    (letrec
        ((helper (lambda (n result)       
                   (cond
                     ((zero? n) result)
                     (else (helper (- n 1) (* n result)))))))
      (helper n 1))))
(fact 0)
(fact 5)