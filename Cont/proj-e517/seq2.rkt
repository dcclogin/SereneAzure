(define-program-counter pc2)

(define-registers
  seq2-exp
  seq2-env
  seq2-k
  app2-k-k₀
  app2-k-v₀
  mk2-clos-body
  mk2-clos-env
  mk2-clos-k
  app2-clos-c
  app2-clos-a
  app2-clos-k
  ext2-env-v
  ext2-env-envr
  ext2-env-k
  app2-env-envr
  app2-env-y
  app2-env-k)

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

(define-label value-of-seq2
  (union-case seq2-exp expr
              [(const cexp)
               (begin
                 (set! app2-k-k₀ seq2-k)
                 (set! app2-k-v₀ cexp)
                 (set! pc2 apply2-k))]
              [(var n)
               (begin
                 (set! app2-env-envr seq2-env)
                 (set! app2-env-y n)
                 (set! app2-env-k seq2-k)
                 (set! pc2 apply2-env))]
              [(mult nexp1 nexp2)
               (begin
                 (set! seq2-exp nexp1)
                 #;(set! seq2-env seq2-env)
                 (set! seq2-k (cont_mult₁ nexp2 seq2-env seq2-k))
                 (set! pc2 value-of-seq2))]
              [(sub1 nexp)
               (begin
                 (set! seq2-exp nexp)
                 #;(set! seq2-env seq2-env)
                 (set! seq2-k (cont_sub1 seq2-k))
                 (set! pc2 value-of-seq2))]
              [(zero nexp)
               (begin
                 (set! seq2-exp nexp)
                 #;(set! seq2-env seq2-env)
                 (set! seq2-k (cont_zero seq2-k))
                 (set! pc2 value-of-seq2))]
              [(if test conseq alt)
               (begin
                 (set! seq2-exp test)
                 #;(set! seq2-env seq2-env)
                 (set! seq2-k (cont_if conseq alt seq2-env seq2-k))
                 (set! pc2 value-of-seq2))]
              [(letcc body)
               (begin
                 (set! ext2-env-v seq2-k)
                 (set! ext2-env-envr seq2-env)
                 (set! ext2-env-k (cont_letcc body seq2-k))
                 (set! pc2 extend2-env))]
              [(throw kexp vexp)
               (begin
                 (set! seq2-exp kexp)
                 #;(set! seq2-env seq2-env)
                 (set! seq2-k (cont_throw vexp seq2-env))
                 (set! pc2 value-of-seq2))]
              [(let exp body)
               (begin
                 (set! seq2-exp exp)
                 #;(set! seq2-env seq2-env)
                 (set! seq2-k (cont_let₁ seq2-env body seq2-k))
                 (set! pc2 value-of-seq2))]
              [(lambda body)
               (begin
                 (set! mk2-clos-body body)
                 (set! mk2-clos-env seq2-env)
                 (set! mk2-clos-k seq2-k)
                 (set! pc2 make2-closure))]
              [(app rator rand)
               (begin
                 (set! seq2-exp rator)
                 #;(set! seq2-env seq2-env)
                 (set! seq2-k (cont_clos rand seq2-env seq2-k))
                 (set! pc2 value-of-seq2))]))

;; closure

(define-label make2-closure
  (begin
    (set! app2-k-k₀ mk2-clos-k)
    (set! app2-k-v₀ (closure_clos mk2-clos-body mk2-clos-env))
    (set! pc2 apply2-k)))

(define-label apply2-closure
  (union-case app2-clos-c closure
              [(clos body env)
               (begin
                 (set! ext2-env-v app2-clos-a)
                 (set! ext2-env-envr env)
                 (set! ext2-env-k (cont_let₂ body app2-clos-k))
                 (set! pc2 extend2-env))]))


;; environment

(define-label extend2-env
  (begin
    (set! app2-k-k₀ ext2-env-k)
    (set! app2-k-v₀ (env_pr ext2-env-v ext2-env-envr))
    (set! pc2 apply2-k)))

(define-label apply2-env
  (union-case app2-env-envr env
              [(mt) (error 'value-of "unbound identifier")]
              [(pr v cdrenv)
               (if (zero? app2-env-y)
                   (begin
                     (set! app2-k-k₀ app2-env-k)
                     (set! app2-k-v₀ v)
                     (set! pc2 apply2-k))
                   (begin
                     (set! app2-env-envr cdrenv)
                     (set! app2-env-y (sub1 app2-env-y))
                     #;(set! app2-env-k app2-env-k)
                     (set! pc2 apply2-env)))]))


;; continuation

(define-label apply2-k
  (union-case app2-k-k₀ cont
              [(init jump-out) (dismount-trampoline jump-out)]
              [(mult₁ x env k)
               (begin
                 (set! seq2-exp x)
                 (set! seq2-env env)
                 (set! seq2-k (cont_mult₂ app2-k-v₀ k))
                 (set! pc2 value-of-seq2))]
              [(mult₂ v k)
               (begin
                 (set! app2-k-k₀ k)
                 (set! app2-k-v₀ (* v app2-k-v₀))
                 (set! pc2 apply2-k))]
              [(zero k)
               (begin
                 (set! app2-k-k₀ k)
                 (set! app2-k-v₀ (zero? app2-k-v₀))
                 (set! pc2 apply2-k))]
              [(sub1 k)
               (begin
                 (set! app2-k-k₀ k)
                 (set! app2-k-v₀ (sub1 app2-k-v₀))
                 (set! pc2 apply2-k))]
              [(throw v env)
               (begin
                 (set! seq2-exp v)
                 (set! seq2-env env)
                 (set! seq2-k app2-k-v₀)
                 (set! pc2 value-of-seq2))]
              [(clos rand env k)
               (begin
                 (set! seq2-exp rand)
                 (set! seq2-env env)
                 (set! seq2-k (cont_arg app2-k-v₀ k))
                 (set! pc2 value-of-seq2))]
              [(arg c k)
               (begin
                 (set! app2-clos-c c)
                 (set! app2-clos-a app2-k-v₀)
                 (set! app2-clos-k k)
                 (set! pc2 apply2-closure))]
              [(if conseq alt env k)
               (if app2-k-v₀
                   (begin
                     (set! seq2-exp conseq)
                     (set! seq2-env env)
                     (set! seq2-k k)
                     (set! pc2 value-of-seq2))
                   (begin
                     (set! seq2-exp alt)
                     (set! seq2-env env)
                     (set! seq2-k k)
                     (set! pc2 value-of-seq2)))]
              [(let₁ env body k)
               (begin
                 (set! ext2-env-v app2-k-v₀)
                 (set! ext2-env-envr env)
                 (set! ext2-env-k (cont_let₂ body k))
                 (set! pc2 extend2-env))]
              [(let₂ body k)
               (begin
                 (set! seq2-exp body)
                 (set! seq2-env app2-k-v₀)
                 (set! seq2-k k)
                 (set! pc2 value-of-seq2))]
              [(letcc body k)
               (begin
                 (set! seq2-exp body)
                 (set! seq2-env app2-k-v₀)
                 (set! seq2-k k)
                 (set! pc2 value-of-seq2))]))



(define-label main 
  (begin
    (set! seq2-exp
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
    (set! seq2-env (env_mt))
    (set! pc2 value-of-seq2)
    (mount-trampoline cont_init seq2-k pc2)
    (printf "The value is ~s\n" app2-k-v₀)))


