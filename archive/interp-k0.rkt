#lang racket
(require racket/trace)

(define mt-env '())

(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))
       
(struct ClosV (f env) #:transparent)

(define interp
  (lambda (exp env k)
    (match exp
      [`(let-cc ,c ,e)
       (interp e (ext-env c k env) k)]
      [(? symbol? x)
       (let ([v (lookup x env)])
         (k v))]      
      [(? number? x)
       (k x)] 
      [`(lambda (,x) ,e)                               
       (k (ClosV exp env))]   
      [`(let ([,x ,e1]) ,e2)
       (interp e1 env
               (lambda (v)
                 (interp e2 (ext-env x v env) k)))]  
      [`(,e1 ,e2)
       (interp e1 env
               (lambda (v1)
                 (interp e2 env
                         (lambda (v2)
                           (match v1
                             [(ClosV `(lambda (,x) ,e) env-save)
                              (interp e (ext-env x v2 env-save) k)]
                             [(? procedure? x) (v1 v2)])))))]
      [`(,op ,e1 ,e2)
       (interp e1 env
               (lambda (v1)
                 (interp e2 env
                         (lambda (v2)
                           (match op
                             ['+ (k (+ v1 v2))]
                             ['* (k (* v1 v2))])))))])))

(define interp-driver
  (lambda (exp)
    (interp exp mt-env (lambda (v) v))))


;(trace interp)
(interp-driver '(+ 1 2))
;; => 3

(interp-driver '(* 2 3))
;; => 6

(interp-driver '(* 2 (+ 3 4)))
;; => 14

(interp-driver '(* (+ 1 2) (+ 3 4)))
;; => 21

(interp-driver '((lambda (x) (* 2 x)) 3))
;; => 6

(interp-driver
 '(let ([x 2])
    (let ([f (lambda (y) (* x y))])
      (f 3))))
;; => 6

(interp-driver
 '(let ([x 2])
    (let ([f (lambda (y) (* x y))])
      (let ([x 4])
        (f 3)))))
;; => 6

(interp-driver
 '(+ 1
     (let-cc k
             (+ 3 (k (let ([x (+ 2 3)])
                       (+ x 2)))))))
;; => 8
