#lang racket

(require "pmatch.rkt")

; Part 1: Interpreters and Environments
; 1
(define value-of
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) `,n] ;numbers
            [`,boolean (guard (boolean? boolean)) `,boolean] ;booleans
            [`zero? `,(lambda (x) (zero? x))] ;zero?
            [`sub1 `,(lambda (x) (sub1 x))]
            [`(* ,x ,y) `,(* (value-of x env) (value-of y env))] ; *
            [`,var (guard (symbol? var)) `,(env var)] ;variables
            [`(if ,test ,res1 ,res2) `,(cond
                                         ((value-of test env) (value-of res1 env))
                                         (else (value-of res2 env)))] ;if
            [`(lambda (,id) ,body) `,(lambda (arg) (value-of body (lambda (var)
                                                                    (cond
                                                                      ((eq? id var) arg)
                                                                      (else (env var))))))] ;lambda
            [`(,id ,body) `,((value-of id env) (value-of body env))] ;application
            [`(let ([,arg ,def]) ,body) `,(value-of body (lambda (var)
                                                           (cond
                                                             ((eq? arg var) (value-of def env))
                                                             (else (env var)))))] ; let
            )))

; interpreter2 and interpreter3 are similiar
; not diffcult and easy to understand, so I skipped :-)

; Part2
; 4
(define empty-env
  (lambda ()
    `(empty-env)))
(define reverse-string
  (lambda x
    (list->string (reverse (string->list x)))))
(define fo-eulav
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) `,n]
            [`?orez `,(lambda (x) (zero? x))]
            [`1bus `,(lambda (x) (sub1 x))]
            [`,var (guard (symbol? var)) `,(env var)]
            [`(,body ,id) `,((fo-eulav id env) (fo-eulav body env))]
            [`(,x ,y *) `,(* (fo-eulav x env) (fo-eulav y env))]
            [`(,res2 ,res1 ,test fi) `,(cond
                                         ((fo-eulav test env) (fo-eulav res1 env))
                                         (else (fo-eulav res2 env)))]
            [`(,body (,id) adbmal) `,(lambda (arg)
                                       (fo-eulav body
                                                 (lambda (var)
                                                   (cond
                                                     ((eq? id var) arg)
                                                     (else (env var))))))]
            )))

; 6
(define value-of-lex
  (lambda (exp env)
    (match exp
      (`,b #:when (boolean? b) exp)
      (`,n #:when (number? n) exp)
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(zero? ,body) (zero? (value-of-lex body env)))
      (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))
(define apply-env-lex list-ref)
(define extend-env-lex cons)

; 7
(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
(define c+ (lambda (m) 
               (lambda (n) 
                 (lambda (a) (lambda (b) ((m a) ((n a) b)))))))

(define csub1
  (lambda (base)
    (lambda (f)
      (lambda (zero)
        (let
            ([x2 (cdr ((base (lambda (x)
                               (let
                                   ([x1 (car x)]
                                    [x2 (cdr x)])
                                 (cons (f x1) x1))))
                       (cons zero zero)))])
          x2)))))
(((csub1 c5) add1) 0)
(((csub1 c0) add1) 0)