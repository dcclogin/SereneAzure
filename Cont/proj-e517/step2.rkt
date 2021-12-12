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

(define-union closure
  (clos body env))

(define-union env
  (mt)
  (pr v cdrenv))

(define-union cont
  (init)
  (mult₁ x env k)
  (mult₂ v k)
  (zero k)
  (sub1 k)
  (throw v env)
  (clos rand env k)
  (arg c k)
  (if conseq alt env k)
  (let₁ env body k)
  (let₂ body k)
  (letcc body k))

(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
                [(const cexp) (let* ([k₀ k]
                                     [v₀ cexp])
                                (apply-k k₀ v₀))]
                [(var n) (let* ([envr env]
                                [y n]
                                [k k])
                           (apply-env envr y k))]
                [(mult nexp1 nexp2)  
                 (let* ([exp nexp1]
                        [env env]
                        [k (make-k-mult₁ nexp2 env k)])
                   (value-of-cps exp env k))]
                [(sub1 nexp)
                 (let* ([exp nexp]
                        [env env]
                        [k (make-k-sub1 k)])
                   (value-of-cps exp env k))]
                [(zero nexp)
                 (let* ([exp nexp]
                        [env env]
                        [k (make-k-zero? k)])
                   (value-of-cps exp env k))]
                [(if test conseq alt)
                 (let* ([exp test]
                        [env env]
                        [k (make-k-if conseq alt env k)])
                   (value-of-cps exp env k))]
                [(letcc body)
                 (let* ([v k]
                        [envr env]
                        [k (make-k-letcc body k)])
                   (extend-env v envr k))]
                [(throw kexp vexp)
                 (let* ([exp kexp]
                        [env env]
                        [k (make-k-throw vexp env)])
                   (value-of-cps exp env k))]
                [(let exp body)
                 (let* ([exp exp]
                        [env env]
                        [k (make-k-let₁ env body k)])
                   (value-of-cps exp env k))]
                [(lambda body)
                 (let* ([body body]
                        [env env]
                        [k k])
                   (make-closure body env k))]
                [(app rator rand)
                 (let* ([exp rator]
                        [env env]
                        [k (make-k-clos rand env k)])
                   (value-of-cps exp env k))])))

;; closure

(define make-closure
  (λ (body env k)
    (let* ([k₀ k]
           [v₀ (closure_clos body env)])
      (apply-k k₀ v₀))))

(define apply-closure
  (λ (c a k)
    (union-case c closure
                [(clos body env)
                 (let* ([v a]
                        [envr env]
                        [k (make-k-let₂ body k)])
                   (extend-env v envr k))])))


;; environment

(define empty-env
  (lambda ()
    (env_mt)))

(define extend-env
  (λ (v envr k)
    (let* ([k₀ k]
           [v₀ (env_pr v envr)])
      (apply-k k₀ v₀))))

(define apply-env
  (λ (envr y k)
    (union-case envr env
                [(mt) (error 'value-of "unbound identifier")]
                [(pr v cdrenv)
                 (if (zero? y)
                     (let* ([k₀ k]
                            [v₀ v])
                       (apply-k k₀ v₀))
                     (let* ([envr cdrenv]
                            [y (sub1 y)]
                            [k k])
                       (apply-env envr y k)))])))


;; continuation

(define empty-k
  (lambda ()
    (cont_init)))

(define make-k-mult₁
  (λ (x^ env-cps^ k^)
    (cont_mult₁ x^ env-cps^ k^)))

(define make-k-mult₂
  (λ (v^ k^)
    (cont_mult₂ v^ k^)))

(define make-k-zero?
  (λ (k^)
    (cont_zero k^)))

(define make-k-sub1
  (λ (k^)
    (cont_sub1 k^)))

(define make-k-throw
  (λ (v-exp^ env-cps^)
    (cont_throw v-exp^ env-cps^)))

(define make-k-clos
  (λ (rand^ env-cps^ k^)
    (cont_clos rand^ env-cps^ k^)))

(define make-k-arg
  (λ (clos^ k^)
    (cont_arg clos^ k^)))

(define make-k-if
  (λ (conseq^ alt^ env-cps^ k^)
    (cont_if conseq^ alt^ env-cps^ k^)))

(define make-k-let₁
  (λ (env-cps^ body^ k^)
    (cont_let₁ env-cps^ body^ k^)))

(define make-k-let₂
  (λ (body^ k^)
    (cont_let₂ body^ k^)))

(define make-k-letcc
  (λ (body^ k^)
    (cont_letcc body^ k^)))

(define apply-k
  (λ (k₀ v₀)
    (union-case k₀ cont
                [(init) v₀]
                [(mult₁ x env k)
                 (let* ([exp x]
                        [env env]
                        [k (make-k-mult₂ v₀ k)])
                   (value-of-cps exp env k))]
                [(mult₂ v k)
                 (let* ([k₀ k]
                        [v₀ (* v v₀)])
                   (apply-k k₀ v₀))]
                [(zero k)
                 (let* ([k₀ k]
                        [v₀ (zero? v₀)])
                   (apply-k k₀ v₀))]
                [(sub1 k)
                 (let* ([k₀ k]
                        [v₀ (sub1 v₀)])
                   (apply-k k₀ v₀))]
                [(throw v env)
                 (let* ([exp v]
                        [env env]
                        [k v₀])
                   (value-of-cps exp env k))]
                [(clos rand env k)
                 (let* ([exp rand]
                        [env env]
                        [k (make-k-arg v₀ k)])
                   (value-of-cps exp env k))]
                [(arg c k)
                 (let* ([c c]
                        [a v₀]
                        [k k])
                   (apply-closure c a k))]
                [(if conseq alt env k)
                 (if v₀
                     (let* ([exp conseq]
                            [env env]
                            [k k])
                       (value-of-cps exp env k))
                     (let* ([exp alt]
                            [env env]
                            [k k])
                       (value-of-cps exp env k)))]
                [(let₁ env body k)
                 (let* ([v v₀]
                        [envr env]
                        [k (make-k-let₂ body k)])
                   (extend-env v envr k))]
                [(let₂ body k)
                 (let* ([exp body]
                        [env v₀]
                        [k k])
                   (value-of-cps exp env k))]
                [(letcc body k)
                 (let* ([exp body]
                        [env v₀]
                        [k k])
                   (value-of-cps exp env k))])))



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


