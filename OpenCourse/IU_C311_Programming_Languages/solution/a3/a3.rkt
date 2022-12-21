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
            [`,var (guard (symbol? var)) `,(unbox (env var))] ;variables
            [`(* ,x ,y) `,(* (value-of x env) (value-of y env))] ; *
            [`(if ,test ,res1 ,res2) `,(cond
                                         ((value-of test env) (value-of res1 env))
                                         (else (value-of res2 env)))] ;if
            [`(lambda (,id) ,body) `,(lambda (arg) (value-of body
                                                             (let ((bx (box arg)))
                                                               (lambda (var)
                                                                 (cond
                                                                   ((eq? id var) bx)
                                                                   (else (env var)))))))] ;lambda
            [`(,id ,body) `,((value-of id env) (value-of body env))] ;application
            [`(let ([,arg ,def]) ,body) `,(value-of body
                                                    (let ((bx (box (value-of def env))))
                                                      (lambda (var)
                                                        (cond
                                                          ((eq? arg var) bx)
                                                          (else (env var))))))] ; let
            [`(set! ,x ,def) `,(set-box! (env x) (value-of def env))] ;set!
            [`(begin2 ,x ,y) `,(begin (value-of x env) (value-of y env))] ;begin2
            )))

(value-of
 '((lambda (x)
     (begin2 (set! x 5) x))
   6)
 (lambda (y) (error 'value-of "unbound variable ~s" y)))

; value-of-fn
; treat env as a function
(define empty-env-fn
  (lambda ()
    (lambda (var) (error "no var"))))
(define extend-env-fn
  (lambda (id arg env)
    (lambda (var)
      (cond
        ((eq? id var) arg)
        (else (env var))))))
(define apply-env-fn
  (lambda (env var)
    (env var)))

(define value-of-fn
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) `,n] ;numbers
            [`,boolean (guard (boolean? boolean)) `,boolean] ;booleans
            [`zero? `,(lambda (x) (zero? x))] ;zero?
            [`sub1 `,(lambda (x) (sub1 x))] ;sub1
            [`(* ,x ,y) `,(* (value-of-fn x env) (value-of-fn y env))] ; *
            [`,var (guard (symbol? var)) `,(apply-env-fn env var)] ;variables
            [`(if ,test ,res1 ,res2) `,(cond
                                         ((value-of-fn test env) (value-of-fn res1 env))
                                         (else (value-of-fn res2 env)))] ;if
            [`(lambda (,id) ,body) `,(lambda (arg) (value-of-fn body (extend-env-fn id arg env)))] ;lambda
            [`(,id ,body) `,((value-of-fn id env) (value-of-fn body env))] ;application
            [`(let ([,id ,arg]) ,body) `,(value-of-fn body (extend-env-fn id (value-of-fn arg env) env))] ; let
            )))

; value-of-ds
; treat env as a list
(define empty-env-ds
  (lambda ()
    `(empty-env)))
(define extend-env-ds
  (lambda (id arg env)
    `(extend-env-ds ,id ,arg, env)))
(define apply-env-ds
  (lambda (env var)
    (pmatch env
     [`(empty-env) `,(error "no var")]
     [`(extend-env-ds ,id ,arg, env) (cond
                                       ((eq? id var) arg)
                                       (else (apply-env-ds env var)))])))

(define value-of-ds
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (integer? n)) `,n] ;numbers
            [`,boolean (guard (boolean? boolean)) `,boolean] ;booleans
            [`zero? `,(lambda (x) (zero? x))] ;zero?
            [`sub1 `,(lambda (x) (sub1 x))]
            [`(* ,x ,y) `,(* (value-of-ds x env) (value-of-ds y env))] ; *
            [`,var (guard (symbol? var)) `,(apply-env-ds env var)] ;variables
            [`(if ,test ,res1 ,res2) `,(cond
                                         ((value-of-ds test env) (value-of-ds res1 env))
                                         (else (value-of-ds res2 env)))] ;if
            [`(lambda (,id) ,body) `,(lambda (arg) (value-of-ds body (extend-env-ds id arg env)))] ;lambda
            [`(,id ,body) `,((value-of-ds id env) (value-of-ds body env))] ;application
            [`(let ([,id ,arg]) ,body) `,(value-of-ds body (extend-env-ds id (value-of-ds arg env) env))] ; let
            )))

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
; there are some differences between 2013ver and 2022ver.
; const and zero
; but it doesn't matter, apply-env-lex and exten-env-lex are not changed 
(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(zero ,x) (zero? (value-of-lex x env))]
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