#lang racket
; occurs-free? algorithm
(define occurs-free?
  (lambda (x lc)
    (match lc
      [(? number? i) #f]
      [(? symbol? i) (eq? x i)]
      [`(lambda (,i) ,e) (and (not (eq? x i))
                              (occurs-free? x e))]
      [`(,f ,a) (or (occurs-free? x f)
                    (occurs-free? x a))]
      [`(,bop ,e1 ,e2) (or (occurs-free? x e1)
                           (occurs-free? x e2))])))

; redefine the 'generate symbol' algorithm
(define gensym
  (let ([n -1])
    (lambda (x)
      (set! n (+ 1 n))
      (string->symbol
       (string-append (symbol->string x) "." (number->string n))))))

; substitution algotithm M[y<-N]
(define subst
  (lambda (y N M)
    (cond
      [(occurs-free? y M)
       (match M
         [(? symbol? i) N]
         [`(,e1 ,e2)
          `(,(subst y N e1) ,(subst y N e2))]
         [`(,bop ,e1 ,e2)
          `(,bop ,(subst y N e1)
                 ,(subst y N e2))]
         [`(lambda (,x) ,e)
          (cond
            [(occurs-free? x N)
             (let* ([x* (gensym x)]
                    [e* (subst x x* e)])
               (if (eq? x* y)
                   (let ([x* (gensym x)])
                     `(lambda (,x*) ,(subst y N e*)))
                   `(lambda (,x*) ,(subst y N e*))))]
            [else `(lambda (,x) ,(subst y N e))])])]
      [else M])))

(define value?
  (lambda (e)
    (match e
      [(? number? x) #t]
      [(? symbol? x) #t]
      [`(lambda (,x) ,b) #t]
      [else #f])))

; Machine State
(struct St (exp ctx) #:transparent)

; CC Machine
(define cc
  (lambda (s)
    (match s
      [(St `(,M ,N) E)
       (if (value? M)
           (if (value? N)
               (match M
                 [`(lambda (,x) ,e)
                  (St (subst x N e) E)])
               (St N `((,M _) . ,E)))
           (St M `((_ ,N) . ,E)))]
      [(St `(,bop ,e1 ,e2) E)
       (if (value? e1)
           (if (value? e2)
               (match bop
                 ['+ (St (+ e1 e2) E)]
                 ['* (St (* e1 e2) E)])
               (St e2 `((,bop ,e1 _) . ,E)))
           (St e1 `((,bop _ ,e2) . ,E)))]
      [(St (? value? V) `((,U _) . ,E)) (St `(,U ,V) E)]
      [(St (? value? V) `((_ ,N) . ,E)) (St `(,V ,N) E)]
      [(St (? value? V) `((,bop ,e1 _) . ,E)) (St `(,bop ,e1 ,V) E)]
      [(St (? value? V) `((,bop _ ,e2) . ,E)) (St `(,bop ,V ,e2) E)])))

; Simpified CC Machine
(define scc
  (lambda (s)
    (match s
      [(St (? value? V) `(((lambda (,x) ,e) _) . ,E)) (St (subst x V e) E)]
      [(St (? value? V) `((_ ,N) . ,E)) (St N `((,V _) . ,E))]
      [(St (? value? V) `((,bop _ ,e2) . ,E)) (St e2 `((,bop ,V _) . ,E))]
      [(St (? value? V) `((,bop ,e1 _) . ,E)) (match bop
                                                ['+ (St (+ e1 V) E)]
                                                ['* (St (* e1 V) E)])]
      [(St `((lambda (,x) ,e) ,N) E) (if (value? N)
                                         (St (subst x N e) E)
                                         (St N `(((lambda (,x) ,e) _) . ,E)))]
      [(St `(,M ,N) E) (St M `((_ ,N) . ,E))] ; if M is not an λ-abstraction
      [(St `(,bop ,e1 ,e2) E) (if (value? e1)
                                  (if (value? e2)
                                      (match bop
                                        ['+ (St (+ e1 e2) E)]
                                        ['* (St (* e1 e2) E)])
                                      (St e2 `((,bop ,e1 _) . ,E)))
                                  (St e1 `((,bop _ ,e2) . ,E)))])))



(define run-machine
  (lambda (m s c)
    (display c)
    (display " -> ")
    (writeln s)
    (match s
      [(St V '(mt))
       (match V
         [(? number? x) V]
         [`(lambda (,x) ,e) V]
         [o (run-machine m (m s) (add1 c))])]
      [else (run-machine m (m s) (add1 c))])))

(run-machine scc (St '((lambda (x) x) ((lambda (x) x) 5)) '(mt)) 0)
(run-machine cc (St '((lambda (x) x) ((lambda (x) x) 5)) '(mt)) 0)
(run-machine scc (St '(* (+ (* 2 3) 3) (* 4 5)) '(mt)) 0)
(run-machine cc (St '(* (+ (* 2 3) 3) (* 4 5)) '(mt)) 0)