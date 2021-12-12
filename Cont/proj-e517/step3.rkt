(define-program-counter pc)
(define-program-counter pc1)
(define-program-counter pc2)

(define-registers
  valof-exp
  valof-env
  valof-k
  app-k-k₀
  app-k-v₀
  mk-clos-body
  mk-clos-env
  mk-clos-k
  app-clos-c
  app-clos-a
  app-clos-k
  ext-env-v
  ext-env-envr
  ext-env-k
  app-env-envr
  app-env-y
  app-env-k
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
  app1-env-k
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

(define-label value-of-cps
  (union-case valof-exp expr
              [(const cexp)
               (begin
                 (set! app-k-k₀ valof-k)
                 (set! app-k-v₀ cexp)
                 (set! pc apply-k))]
              [(var n)
               (begin
                 (set! app-env-envr valof-env)
                 (set! app-env-y n)
                 (set! app-env-k valof-k)
                 (set! pc apply-env))]
              [(mult nexp1 nexp2)
               (begin
                 (set! seq1-exp nexp1)
                 (set! seq2-exp nexp2)
                 (set! seq1-env valof-env)
                 (set! seq2-env valof-env)
                 (set! pc1 value-of-seq1)
                 (set! pc2 value-of-seq2)
                 (mount-trampoline cont_init seq1-k pc1)
                 (mount-trampoline cont_init seq2-k pc2)
                 (set! app-k-k₀ valof-k)
                 (set! app-k-v₀ (* app1-k-v₀ app2-k-v₀))
                 (set! pc apply-k))]
              [(sub1 nexp)
               (begin
                 (set! valof-exp nexp)
                 #;(set! valof-env valof-env)
                 (set! valof-k (cont_sub1 valof-k))
                 (set! pc value-of-cps))]
              [(zero nexp)
               (begin
                 (set! valof-exp nexp)
                 #;(set! valof-env valof-env)
                 (set! valof-k (cont_zero valof-k))
                 (set! pc value-of-cps))]
              [(if test conseq alt)
               (begin
                 (set! valof-exp test)
                 #;(set! valof-env valof-env)
                 (set! valof-k (cont_if conseq alt valof-env valof-k))
                 (set! pc value-of-cps))]
              [(letcc body)
               (begin
                 (set! ext-env-v valof-k)
                 (set! ext-env-envr valof-env)
                 (set! ext-env-k (cont_letcc body valof-k))
                 (set! pc extend-env))]
              [(throw kexp vexp)
               (begin
                 (set! valof-exp kexp)
                 #;(set! valof-env valof-env)
                 (set! valof-k (cont_throw vexp valof-env))
                 (set! pc value-of-cps))]
              [(let exp body)
               (begin
                 (set! valof-exp exp)
                 #;(set! valof-env valof-env)
                 (set! valof-k (cont_let₁ valof-env body valof-k))
                 (set! pc value-of-cps))]
              [(lambda body)
               (begin
                 (set! mk-clos-body body)
                 (set! mk-clos-env valof-env)
                 (set! mk-clos-k valof-k)
                 (set! pc make-closure))]
              [(app rator rand)
               (begin
                 (set! valof-exp rator)
                 #;(set! valof-env valof-env)
                 (set! valof-k (cont_clos rand valof-env valof-k))
                 (set! pc value-of-cps))]))

;; closure

(define-label make-closure
  (begin
    (set! app-k-k₀ mk-clos-k)
    (set! app-k-v₀ (closure_clos mk-clos-body mk-clos-env))
    (set! pc apply-k)))

(define-label apply-closure
  (union-case app-clos-c closure
              [(clos body env)
               (begin
                 (set! ext-env-v app-clos-a)
                 (set! ext-env-envr env)
                 (set! ext-env-k (cont_let₂ body app-clos-k))
                 (set! pc extend-env))]))


;; environment

(define-label extend-env
  (begin
    (set! app-k-k₀ ext-env-k)
    (set! app-k-v₀ (env_pr ext-env-v ext-env-envr))
    (set! pc apply-k)))

(define-label apply-env
  (union-case app-env-envr env
              [(mt) (error 'value-of "unbound identifier")]
              [(pr v cdrenv)
               (if (zero? app-env-y)
                   (begin
                     (set! app-k-k₀ app-env-k)
                     (set! app-k-v₀ v)
                     (set! pc apply-k))
                   (begin
                     (set! app-env-envr cdrenv)
                     (set! app-env-y (sub1 app-env-y))
                     #;(set! app-env-k app-env-k)
                     (set! pc apply-env)))]))


;; continuation

(define-label apply-k
  (union-case app-k-k₀ cont
              [(init jump-out) (dismount-trampoline jump-out)]
              [(mult₁ x env k)
               (begin
                 (set! valof-exp x)
                 (set! valof-env env)
                 (set! valof-k (cont_mult₂ app-k-v₀ k))
                 (set! pc value-of-cps))]
              [(mult₂ v k)
               (begin
                 (set! app-k-k₀ k)
                 (set! app-k-v₀ (* v app-k-v₀))
                 (set! pc apply-k))]
              [(zero k)
               (begin
                 (set! app-k-k₀ k)
                 (set! app-k-v₀ (zero? app-k-v₀))
                 (set! pc apply-k))]
              [(sub1 k)
               (begin
                 (set! app-k-k₀ k)
                 (set! app-k-v₀ (sub1 app-k-v₀))
                 (set! pc apply-k))]
              [(throw v env)
               (begin
                 (set! valof-exp v)
                 (set! valof-env env)
                 (set! valof-k app-k-v₀)
                 (set! pc value-of-cps))]
              [(clos rand env k)
               (begin
                 (set! valof-exp rand)
                 (set! valof-env env)
                 (set! valof-k (cont_arg app-k-v₀ k))
                 (set! pc value-of-cps))]
              [(arg c k)
               (begin
                 (set! app-clos-c c)
                 (set! app-clos-a app-k-v₀)
                 (set! app-clos-k k)
                 (set! pc apply-closure))]
              [(if conseq alt env k)
               (if app-k-v₀
                   (begin
                     (set! valof-exp conseq)
                     (set! valof-env env)
                     (set! valof-k k)
                     (set! pc value-of-cps))
                   (begin
                     (set! valof-exp alt)
                     (set! valof-env env)
                     (set! valof-k k)
                     (set! pc value-of-cps)))]
              [(let₁ env body k)
               (begin
                 (set! ext-env-v app-k-v₀)
                 (set! ext-env-envr env)
                 (set! ext-env-k (cont_let₂ body k))
                 (set! pc extend-env))]
              [(let₂ body k)
               (begin
                 (set! valof-exp body)
                 (set! valof-env app-k-v₀)
                 (set! valof-k k)
                 (set! pc value-of-cps))]
              [(letcc body k)
               (begin
                 (set! valof-exp body)
                 (set! valof-env app-k-v₀)
                 (set! valof-k k)
                 (set! pc value-of-cps))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    sequential interpreter 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    sequential interpreter 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
    (set! valof-exp
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
           (expr_mult (expr_app
                       (expr_app (expr_var 0) (expr_var 0))
                       (expr_const 5))
                      (expr_app
                       (expr_app (expr_var 0) (expr_var 0))
                       (expr_const 5)))))
    (set! valof-env (env_mt))
    (set! pc value-of-cps)
    (mount-trampoline cont_init valof-k pc)
    (printf "The value is ~s\n" app-k-v₀)))


