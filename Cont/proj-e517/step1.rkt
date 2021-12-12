#lang racket
(require "parenthec.rkt")

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))


(define value-of-cps
  (lambda (exp env-cps k)
    (union-case exp expr
                [(const cexp) (apply-k k cexp)]
                [(var n) (apply-env env-cps n k)]
                [(mult nexp1 nexp2)
                 (value-of-cps nexp1 env-cps
                               (make-k-mult₁ nexp2 env-cps k))]
                [(sub1 nexp)
                 (value-of-cps nexp env-cps
                               (make-k-sub1 k))]
                [(zero nexp)
                 (value-of-cps nexp env-cps
                               (make-k-zero? k))]
                [(if test conseq alt)
                 (value-of-cps test env-cps
                               (make-k-if conseq alt env-cps k))]
                [(letcc body)
                 (extend-env k env-cps
                             (make-k-letcc body k))]
                [(throw kexp vexp)
                 (value-of-cps kexp env-cps
                               (make-k-throw vexp env-cps))]
                [(let exp body)
                 (value-of-cps exp env-cps
                               (make-k-let₁ env-cps body k))]
                [(lambda body)
                 (make-closure body env-cps k)]
                [(app rator rand)
                 (value-of-cps rator env-cps
                               (make-k-clos rand env-cps k))])))

;; environment
 
(define empty-env
  (lambda ()
    `()))

(define extend-env
  (λ (v^ env-cps^ k^)
    (apply-k k^ `(,v^ . ,env-cps^))))

(define apply-env
  (λ (env-cps y k)
    (match env-cps
      ['() (error 'value-of "unbound identifier")]
      [`(,v . ,env₁)
       (if (zero? y)
           (apply-k k v)
           (apply-env env₁ (sub1 y) k))])))

;; closure

(define make-closure
  (λ (body env-cps k)
    (apply-k k `(Closure ,body ,env-cps))))

(define apply-closure
  (λ (c-cps a k)
    (match c-cps
      [`(Closure ,body ,env-cps)
       (extend-env a env-cps
                   (make-k-let₂ body k))])))

;; continuation

(define empty-k
  (lambda ()
    `(init)))

(define make-k-mult₁
  (λ (x^ env-cps^ k^)
    `(mult₁ ,x^ ,env-cps^ ,k^)))

(define make-k-mult₂
  (λ (v^ k^)
    `(mult₂ ,v^ ,k^)))

(define make-k-zero?
  (λ (k^)
    `(zero? ,k^)))

(define make-k-sub1
  (λ (k^)
    `(sub1 ,k^)))

(define make-k-throw
  (λ (v-exp^ env-cps^)
    `(throw ,v-exp^ ,env-cps^)))

(define make-k-clos
  (λ (rand^ env-cps^ k^)
    `(clos ,rand^ ,env-cps^ ,k^)))

(define make-k-arg
  (λ (clos^ k^)
    `(arg ,clos^ ,k^)))

(define make-k-if
  (λ (conseq^ alt^ env-cps^ k^)
    `(if ,conseq^ ,alt^ ,env-cps^ ,k^)))

(define make-k-let₁
  (λ (env-cps^ body^ k^)
    `(let₁ ,env-cps^ ,body^ ,k^)))

(define make-k-let₂
  (λ (body^ k^)
    `(let₂ ,body^ ,k^)))

(define make-k-letcc
  (λ (body^ k^)
    `(letcc ,body^ ,k^)))


(define apply-k
  (λ (k v)
    (match k
      [`(init) v]
      [`(mult₁ ,x^ ,env-cps^ ,k^)
       (value-of-cps x^ env-cps^
                     (make-k-mult₂ v k^))]
      [`(mult₂ ,v^ ,k^)
       (apply-k k^ (* v^ v))]
      [`(zero? ,k^)
       (apply-k k^ (zero? v))]
      [`(sub1 ,k^)
       (apply-k k^ (sub1 v))]
      [`(throw ,v-exp^ ,env-cps^)
       (value-of-cps v-exp^ env-cps^ v)]
      [`(clos ,rand^ ,env-cps^ ,k^)
       (value-of-cps rand^ env-cps^
                     (make-k-arg v k^))]
      [`(arg ,clos^ ,k^)
       (apply-closure clos^ v k^)]
      [`(if ,conseq^ ,alt^ ,env-cps^ ,k^)
       (if v
           (value-of-cps conseq^ env-cps^ k^)
           (value-of-cps alt^ env-cps^ k^))]
      [`(let₁ ,env-cps^ ,body^ ,k^)
       (extend-env v env-cps^
                   (make-k-let₂ body^ k^))]
      [`(let₂ ,body^ ,k^)
       (value-of-cps body^ v k^)]
      [`(letcc ,body^ ,k^)
       (value-of-cps body^ v k^)])))


(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

(main)


