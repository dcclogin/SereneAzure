(define-program-counter pc1)

(define-registers
  seq1-exp
  seq1-env
  seq1-k
  app1-k-k₀
  app1-k-v₀
  mk1-clos-body
  mk1-clos-env
  mk1-clos-k
  app1-clos-c
  app1-clos-a
  app1-clos-k
  ext1-env-v
  ext1-env-envr
  ext1-env-k
  app1-env-envr
  app1-env-y
  app1-env-k)

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
  (init jump-out)
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

(define-label value-of-seq1
  (union-case seq1-exp expr
              [(const cexp)
               (begin
                 (set! app1-k-k₀ seq1-k)
                 (set! app1-k-v₀ cexp)
                 (set! pc1 apply1-k))]
              [(var n)
               (begin
                 (set! app1-env-envr seq1-env)
                 (set! app1-env-y n)
                 (set! app1-env-k seq1-k)
                 (set! pc1 apply1-env))]
              [(mult nexp1 nexp2)
               (begin
                 (set! seq1-exp nexp1)
                 #;(set! seq1-env seq1-env)
                 (set! seq1-k (cont_mult₁ nexp2 seq1-env seq1-k))
                 (set! pc1 value-of-seq1))]
              [(sub1 nexp)
               (begin
                 (set! seq1-exp nexp)
                 #;(set! seq1-env seq1-env)
                 (set! seq1-k (cont_sub1 seq1-k))
                 (set! pc1 value-of-seq1))]
              [(zero nexp)
               (begin
                 (set! seq1-exp nexp)
                 #;(set! seq1-env seq1-env)
                 (set! seq1-k (cont_zero seq1-k))
                 (set! pc1 value-of-seq1))]
              [(if test conseq alt)
               (begin
                 (set! seq1-exp test)
                 #;(set! seq1-env seq1-env)
                 (set! seq1-k (cont_if conseq alt seq1-env seq1-k))
                 (set! pc1 value-of-seq1))]
              [(letcc body)
               (begin
                 (set! ext1-env-v seq1-k)
                 (set! ext1-env-envr seq1-env)
                 (set! ext1-env-k (cont_letcc body seq1-k))
                 (set! pc1 extend1-env))]
              [(throw kexp vexp)
               (begin
                 (set! seq1-exp kexp)
                 #;(set! seq1-env seq1-env)
                 (set! seq1-k (cont_throw vexp seq1-env))
                 (set! pc1 value-of-seq1))]
              [(let exp body)
               (begin
                 (set! seq1-exp exp)
                 #;(set! seq1-env seq1-env)
                 (set! seq1-k (cont_let₁ seq1-env body seq1-k))
                 (set! pc1 value-of-seq1))]
              [(lambda body)
               (begin
                 (set! mk1-clos-body body)
                 (set! mk1-clos-env seq1-env)
                 (set! mk1-clos-k seq1-k)
                 (set! pc1 make1-closure))]
              [(app rator rand)
               (begin
                 (set! seq1-exp rator)
                 #;(set! seq1-env seq1-env)
                 (set! seq1-k (cont_clos rand seq1-env seq1-k))
                 (set! pc1 value-of-seq1))]))

;; closure

(define-label make1-closure
  (begin
    (set! app1-k-k₀ mk1-clos-k)
    (set! app1-k-v₀ (closure_clos mk1-clos-body mk1-clos-env))
    (set! pc1 apply1-k)))

(define-label apply1-closure
  (union-case app1-clos-c closure
              [(clos body env)
               (begin
                 (set! ext1-env-v app1-clos-a)
                 (set! ext1-env-envr env)
                 (set! ext1-env-k (cont_let₂ body app1-clos-k))
                 (set! pc1 extend1-env))]))


;; environment

(define-label extend1-env
  (begin
    (set! app1-k-k₀ ext1-env-k)
    (set! app1-k-v₀ (env_pr ext1-env-v ext1-env-envr))
    (set! pc1 apply1-k)))

(define-label apply1-env
  (union-case app1-env-envr env
              [(mt) (error 'value-of "unbound identifier")]
              [(pr v cdrenv)
               (if (zero? app1-env-y)
                   (begin
                     (set! app1-k-k₀ app1-env-k)
                     (set! app1-k-v₀ v)
                     (set! pc1 apply1-k))
                   (begin
                     (set! app1-env-envr cdrenv)
                     (set! app1-env-y (sub1 app1-env-y))
                     #;(set! app1-env-k app1-env-k)
                     (set! pc1 apply1-env)))]))


;; continuation

(define-label apply1-k
  (union-case app1-k-k₀ cont
              [(init jump-out) (dismount-trampoline jump-out)]
              [(mult₁ x env k)
               (begin
                 (set! seq1-exp x)
                 (set! seq1-env env)
                 (set! seq1-k (cont_mult₂ app1-k-v₀ k))
                 (set! pc1 value-of-seq1))]
              [(mult₂ v k)
               (begin
                 (set! app1-k-k₀ k)
                 (set! app1-k-v₀ (* v app1-k-v₀))
                 (set! pc1 apply1-k))]
              [(zero k)
               (begin
                 (set! app1-k-k₀ k)
                 (set! app1-k-v₀ (zero? app1-k-v₀))
                 (set! pc1 apply1-k))]
              [(sub1 k)
               (begin
                 (set! app1-k-k₀ k)
                 (set! app1-k-v₀ (sub1 app1-k-v₀))
                 (set! pc1 apply1-k))]
              [(throw v env)
               (begin
                 (set! seq1-exp v)
                 (set! seq1-env env)
                 (set! seq1-k app1-k-v₀)
                 (set! pc1 value-of-seq1))]
              [(clos rand env k)
               (begin
                 (set! seq1-exp rand)
                 (set! seq1-env env)
                 (set! seq1-k (cont_arg app1-k-v₀ k))
                 (set! pc1 value-of-seq1))]
              [(arg c k)
               (begin
                 (set! app1-clos-c c)
                 (set! app1-clos-a app1-k-v₀)
                 (set! app1-clos-k k)
                 (set! pc1 apply1-closure))]
              [(if conseq alt env k)
               (if app1-k-v₀
                   (begin
                     (set! seq1-exp conseq)
                     (set! seq1-env env)
                     (set! seq1-k k)
                     (set! pc1 value-of-seq1))
                   (begin
                     (set! seq1-exp alt)
                     (set! seq1-env env)
                     (set! seq1-k k)
                     (set! pc1 value-of-seq1)))]
              [(let₁ env body k)
               (begin
                 (set! ext1-env-v app1-k-v₀)
                 (set! ext1-env-envr env)
                 (set! ext1-env-k (cont_let₂ body k))
                 (set! pc1 extend1-env))]
              [(let₂ body k)
               (begin
                 (set! seq1-exp body)
                 (set! seq1-env app1-k-v₀)
                 (set! seq1-k k)
                 (set! pc1 value-of-seq1))]
              [(letcc body k)
               (begin
                 (set! seq1-exp body)
                 (set! seq1-env app1-k-v₀)
                 (set! seq1-k k)
                 (set! pc1 value-of-seq1))]))



(define-label main 
  (begin
    (set! seq1-exp
          (expr_let
           (expr_lambda
            (expr_lambda
             (expr_if
              (expr_zero (expr_var 0))
              (expr_const 1)
              (expr_mult (expr_var 0)
                         (expr_app
                          (expr_app (expr_var 1) (expr_var 1))
                          (expr_sub1 (expr_var 0)))))))
           (expr_mult
            (expr_letcc
             (expr_app
              (expr_app (expr_var 1) (expr_var 1))
              (expr_throw
               (expr_var 0)
               (expr_app
                (expr_app (expr_var 1) (expr_var 1))
                (expr_const 4)))))
            (expr_const 5))))
    (set! seq1-env (env_mt))
    (set! pc1 value-of-seq1)
    (mount-trampoline cont_init seq1-k pc1)
    (printf "The value is ~s\n" app1-k-v₀)))


