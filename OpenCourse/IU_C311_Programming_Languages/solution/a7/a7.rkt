#lang racket

(require "pmatch.rkt")

; recap CPS -> k: continuation
; (recur-func a b (lambda (x) (k (* x x))))
; recur-func will be computed later, but (k (* x x)) actually means (* x x) will computed first

; Part 1
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
               ((empty? ls) '())
               ((not (= (car ls) 0)) (cons (car ls) (last-non-zero (cdr ls))))
               (else (k (last-non-zero (cdr ls))))
  	       ))))
	(last-non-zero ls)))))

; Part 2
(define lex
  (lambda (a acc)
    (pmatch a
            [`,x (guard (number? x)) `(const ,x)]
            [`,var (guard (symbol? var)) `(var ,(index-of acc var))]
            [`(zero? ,nexp) `(zero ,(lex nexp acc))]
            [`(sub1 ,x) `(sub1 ,(lex x acc))]
            [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
            [`(if ,test ,x ,y) `(if ,(lex test acc) ,(lex x acc) ,(lex y acc))]
            [`(let ((,id ,arg)) ,body) `(let ,(lex arg acc) ,(lex body (cons id acc)))]
            [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]
            [`(lambda (,x) ,y) `(lambda ,(lex y (cons x acc)))]
            [`(let/cc ,k ,body) `(letcc ,(lex body (cons k acc)))]
            [`(throw ,e1, e2) `(throw ,(lex e1 acc) ,(lex e2 acc))]
            [`(quote ,x) `x]
            )))

; Part 3
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env-cps (lambda (x) (value-of-cps x2 env-cps (lambda (y) (k (* x y))))))]
      [`(sub1 ,x) (value-of-cps x env-cps (lambda (x) (k (sub1 x))))]
      [`(zero ,x) (value-of-cps x env-cps (lambda (x) (k (zero? x))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (lambda (x) (if x
                                                                       (value-of-cps conseq env-cps (lambda (y) (k y)))
                                                                       (value-of-cps alt env-cps (lambda (y) (k y))))))]
      ; let/cc is automatically implemented in CPS
      ; let/cc -> (let/cc k body), here k is excatly the continuation
      ; https://stackoverflow.com/a/40142198
      [`(letcc ,body) (value-of-cps body (extend-env k env-cps) k)]
      [`(throw ,k-exp, v-exp) (value-of-cps k-exp env-cps (lambda (x) (value-of-cps v-exp env-cps (lambda (y) (x y)))))]
      ; let 
      [`(let ,e ,body) (value-of-cps e env-cps (lambda (x) (apply-closure (closure body env-cps) x k)))]
      [`(var ,expr) (apply-env env-cps expr k)]
      ; interesting here
      [`(lambda ,body) (apply-k k (closure body env-cps))]
      [`(app ,rator ,rand) (value-of-cps rator env-cps (lambda (x) (value-of-cps rand env-cps (lambda (y) (apply-closure x y k)))))]
      )))

(define empty-env
  (lambda ()
    (lambda (y k)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define extend-env
  (lambda (a env)
    (lambda (y k)
      (if (zero? y)
          (apply-k k a)
          (env (sub1 y) k)))))

(define apply-env
  (lambda (env y k)
    (env y k)))

(define closure
  (lambda (body env)
    (lambda (a k)
      (value-of-cps body (extend-env a env) k))))

(define apply-closure
  (lambda (closure x k)
    (closure x k)))

(define apply-k
  (lambda (k x)
    (k x)))