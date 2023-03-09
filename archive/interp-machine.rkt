#lang racket

(define C* #f) ; the control expression
(define E* #f) ; the environment register
(define V* #f) ; the value register
(define K* #f) ; the continuation register
(define P* #f) ; the procedure register
(define F* #t) ; the flag register

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

(define done-k
  '(done-k))

(define let-k
  (lambda (var body next-k)
    `(let-k ,var ,body ,next-k)))

(define apply-arg-k
  (lambda (arg next-k)
    `(apply-arg-k ,arg ,next-k)))

(define do-apply-k
  (lambda (fv next-k)
    `(do-apply-k ,fv ,next-k)))

(define op-rhs-k
  (lambda (op rhs next-k)
    `(op-rhs-k ,op ,rhs ,next-k)))

(define do-op-k
  (lambda (op v-lhs next-k)
    `(do-op-k ,op ,v-lhs ,next-k)))

(define continue
  (lambda ()
    (match K*
      [`(done-k) (set! F* #t)]

      [`(let-k ,var ,body ,next-k)
       (begin
         (set! C* body)
         (set! E* (ext-env var V* E*))
         (set! K* next-k)
         (set! P* interp))]
      
      [`(apply-arg-k ,arg ,next-k)
       (begin
         (set! C* arg)
         (set! K* (do-apply-k V* next-k))
         (set! P* interp))]
      
      [`(do-apply-k ,fv ,next-k)
       (match fv
         [(ClosV `(lambda (,x) ,e) env-save)
          (begin
            (set! C* e)
            (set! E* (ext-env x V* env-save))
            (set! K* next-k)
            (set! P* interp))])]
      
      [`(op-rhs-k ,op ,rhs ,next-k)
       (begin
         (set! C* rhs)
         (set! K* (do-op-k op V* next-k))
         (set! P* interp))]
      
      [`(do-op-k ,op ,v-lhs ,next-k)
       (match op
         ['+ (begin
               (set! K* next-k)
               (set! V* (+ v-lhs V*))
               (set! P* continue))]
         ['* (begin
               (set! K* next-k)
               (set! V* (* v-lhs V*))
               (set! P* continue))])])))

(define interp
  (lambda ()
    (match C*                                          
      [(? symbol? x)
       (begin
         (set! V* (lookup x E*))
         (set! P* continue))]
      
      [(? number? x)
       (begin
         (set! V* C*)
         (set! P* continue))]
      
      [`(lambda (,x) ,e)                               
       (begin
         (set! V* (ClosV C* E*))
         (set! P* continue))]
      
      [`(let ([,x ,e1]) ,e2)
       (begin
         (set! C* e1)
         (set! K* (let-k x e2 K*))
         (set! P* interp))]
      
      [`(,e1 ,e2)
       (begin
         (set! C* e1)
         (set! K* (apply-arg-k e2 K*))
         (set! P* interp))]
      
      [`(,op ,e1 ,e2)
       (begin
         (set! C* e1)
         (set! K* (op-rhs-k op e2 K*))
         (set! P* interp))])))

(define trampoline
  (lambda ()
    (if F*
        V*
        (begin
          (P*)
          (trampoline)))))

(define interp-driver
  (lambda (exp)
    (begin
      (set! C* exp)
      (set! E* mt-env)
      (set! K* done-k)
      (set! P* interp)
      (set! F* #f)
      (trampoline))))



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
