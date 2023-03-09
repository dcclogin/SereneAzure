#lang racket
;; redefine gensym
(define gensym
  (let ([n -1])
    (λ (sym)
      (set! n (add1 n))
      (string->symbol
       (string-append (symbol->string sym) "."
                      (number->string n))))))

(define lookup
  (λ (x alist)
    (let ([pr (assv x alist)])
      (cond
        [(not pr)
         (error 'lookup "cannot find ~a" x)]
        [else
         (cdr pr)]))))

(define ext
  (λ (x v alist)
    `((,x . ,v) . ,alist)))


(struct VS (value store) #:transparent)
;; for values
(struct Fun (f env) #:transparent)
(struct Loc (l) #:transparent)
(struct Unit ())

;; interp-!: interpreter for an explicit reference language

;; exp := ... |
;; | (newref exp)
;; | (setref exp exp)
;; | (deref exp)

;; value = Unit | Num | Loc | Fun
;; VS = (value * store)
(define interp-!
  (λ (exp env store)
    (match exp
      [(? number? x) (VS x store)]
      [(? symbol? x) (VS (lookup x env) store)]
      ;; new-ref
      [`(: ,e)
       (let* ([v*s (interp-! e env store)]
              [v (VS-value v*s)]
              [s (VS-store v*s)]
              [l (gensym 'loc)]
              [s@ (ext l (box v) s)])
         (VS (Loc l) s@))]
      ;; setref
      [`(:= ,e1 ,e2)
       (let* ([v*s-1 (interp-! e1 env store)]
              [v1 (VS-value v*s-1)]
              [s1 (VS-store v*s-1)])
         (match v1
           [(Loc l)
            (let* ([v*s-2 (interp-! e2 env s1)]
                   [v2 (VS-value v*s-2)]
                   [s2 (VS-store v*s-2)]
                   [b (lookup l s2)])
              (set-box! b v2)
              (VS (Unit) s2))]
           [else (error 'interp-! "not a location.")]))]
      ;; deref
      [`(! ,e)
       (let* ([v*s (interp-! e env store)]
              [v (VS-value v*s)]
              [s (VS-store v*s)])
         (match v
           [(Loc l)
            (let* ([b (lookup l s)])
              (VS (unbox b) s))]
           [else (error 'interp-! "not a location.")]))]
      ;; function definition
      [`(lambda (,x) ,e)
       (VS (Fun exp env) store)]
      ;; function application
      [`(,f ,a)
       (let* ([v*s-f (interp-! f env store)]
              [v-f (VS-value v*s-f)]
              [s-f (VS-store v*s-f)])
         (match v-f
           [(Fun `(lambda (,x) ,e) env-save)
            (let* ([v*s-a (interp-! a env s-f)]
                   [v-a (VS-value v*s-a)]
                   [s-a (VS-store v*s-a)]
                   [env@ (ext x v-a env-save)])
              (interp-! e env@ s-a))]
           [else (error 'interp-! "not a function.")]))]
      ;; let binding
      [`(let ([,x ,e]) ,b)
       (let* ([v*s-e (interp-! e env store)]
              [v-e (VS-value v*s-e)]
              [s-e (VS-store v*s-e)]
              [env@ (ext x v-e env)])
         (interp-! b env@ s-e))]
      ;; sequential
      [`(begin2 ,e1 ,e2)
       ((λ (v*s)
          (interp-! e2 env (VS-store v*s)))
        (interp-! e1 env store))]
      ;; arith
      [`(,op ,e1 ,e2)
       (match op
         [(or '+ '- '* '/)
          (let* ([v*s-1 (interp-! e1 env store)]
                 [v1 (VS-value v*s-1)]
                 [s1 (VS-store v*s-1)]
                 [v*s-2 (interp-! e2 env s1)]
                 [v2 (VS-value v*s-2)]
                 [s2 (VS-store v*s-2)]
                 [v (+ v1 v2)])
            (VS v s2))])])))


(define eval-exp
  (λ (exp)
    (interp-! exp '() '())))


;; => 23
(eval-exp '(let ([x (: 42)])
             (begin2
               (:= x 23)
               (! x))))

;; => 12
(eval-exp '(let ([x (: 5)])
             (let ([y (: 6)])
               (begin2
                 (:= x 6)
                 (+ (! x) (! y))))))

;; => 8
(eval-exp '(let ([x (: 5)])
             (let ([f (lambda (y)
                        (begin2
                          (:= x (+ (! x) (! y)))
                          (! x)))])
               (let ([x 6])
                 (f (: 3))))))