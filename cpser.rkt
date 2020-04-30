#lang racket

(define I (lambda (v) v))

; redefine the gensym function in Racket
(define gensym
  (let ([n -1])
    (lambda (s)
      (set! n (add1 n))
      (string->symbol
       (string-append (symbol->string s) (number->string n))))))

; some trivial instances of CPS transformation
(define tree-sum
  (lambda (exp C)
    (match exp
      [(? number? x) (C x)]
      [`(,e1 ,e2)
       (tree-sum e1
                 (lambda (v1)
                   (tree-sum e2
                             (lambda (v2)
                               (C (+ v1 v2))))))])))
(define calc
  (lambda (exp C)
    (match exp
      [(? number? x) (C x)]
      [(? symbol? x) (C x)]
      [`(,op ,e1 ,e2)
       (calc op
             (lambda (v0)
               (calc e1
                     (lambda (v1)
                       (calc e2
                             (lambda (v2)
                               (match v0
                                 ['+ (C (+ v1 v2))]
                                 ['* (C (* v1 v2))])))))))])))
(define fact
  (lambda (n C)
    (cond
      [(= n 0) (C 1)]
      [else (fact (sub1 n)
                  (lambda (v)
                    (* (C v) n)))]))) ; a weired place for C


; the core of CPS transformation, and it's much like flatten pass!
(define cps
  (lambda (exp C)
    (match exp
      [(? symbol? x) (C x)]
      [`(,e1 ,e2)
       (cps e1
            (lambda (v1)
              (cps e2
                   (lambda (v2)
                     (let ([v* (gensym 'v)])
                       `(,v1 ,v2
                             (λ (,v*) ,(C v*))))))))])))

; the core of ANF transformation. Still like the flatten pass!
(define anf
  (lambda (exp C)
    (match exp
      [(? symbol? x) (C x)]
      [`(,e1 ,e2)
       (anf e1
            (lambda (v1)
              (anf e2
                   (lambda (v2)
                     (let ([v* (gensym 'v)])
                       `(let ([,v* (,v1 ,v2)])
                          ,(C v*)))))))])))

; use the same technique to find the left-most redex in a reduction sequence
(define left-most
  (lambda (exp C)
    (match exp
      [(? symbol? x) (C x)]
      [`(,e1 ,e2)
       (left-most e1
                  (lambda (v1)
                    (left-most e2
                               (lambda (v2)
                                 (let ([rdx `(,v1 ,v2)])
                                   rdx)))))])))

; and the right-most redex ...
(define right-most
  (lambda (exp C)
    (match exp
      [(? symbol? x) (C x)]
      [`(,e1 ,e2)
       (right-most e2
                   (lambda (v2)
                     (right-most e1
                                 (lambda (v1)
                                   (let ([rdx `(,v1 ,v2)])
                                     rdx)))))])))

; get the 'context' of the left-most redex in one pass
(define unwrap1
  (lambda (exp ctx C)
    (match exp
      [(? symbol? x) (C x)]
      [`(,e1 ,e2)
       (unwrap1 e1
                (lambda (v) (ctx `(,v ,e2)))
                (lambda (v1)
                  (unwrap1 e2
                           (lambda (v) (ctx `(,e1 ,v)))
                           (lambda (v2)
                             (let ([rdx `(,v1 ,v2)]
                                   [r* (gensym 'r)])
                               `((:= ,r* ,rdx)
                                 ,(ctx r*)))))))])))

; like the 'small-step' flatten pass in compiler building
(define unwrap-arith
  (lambda (exp ctx C)
    (match exp
      [(? symbol? x) (C x)]
      [(? number? x) (C x)]
      [`(,e1 ,e2)
       (unwrap-arith
        e1
        (lambda (v)
          (ctx `(,v ,e2)))
        (lambda (v1)
          (unwrap-arith
           e2
           (lambda (v)
             (ctx `(,e1 ,v)))
           (lambda (v2)
             (let ([rdx `(,v1 ,v2)]
                   [r* (gensym 'r)])
               `((:= ,r* ,rdx)
                 ,(ctx r*)))))))]
      [`(,op ,e1 ,e2)
       (unwrap-arith
        op
        (lambda (v)
          (ctx `(,v ,e1 ,e2)))
        (lambda (v0)
          (unwrap-arith
           e1
           (lambda (v)
             (ctx `(,op ,v ,e2)))
           (lambda (v1)
             (unwrap-arith
              e2
              (lambda (v)
                (ctx `(,op ,e1 ,v)))
              (lambda (v2)
                (let ([rdx `(,op ,v1 ,v2)]
                      [r* (gensym 'r)])
                  `((:= ,r* ,rdx)
                    ,(ctx r*)))))))))])))

(define flatten
  (lambda (exp)
    (match exp
      [(? symbol? x) x]
      [`(,e1 ,e2)
       (let ([res (unwrap1 exp I I)])
         (cons (car res) (flatten (car (cdr res)))))])))

(define flatten-arith
  (lambda (exp)
    (match exp
      [(? symbol? x) x]
      [o
       (let ([res (unwrap-arith exp I I)])
         (cons (car res) (flatten-arith (car (cdr res)))))])))

; find all redexes and their 'contexts' in a reduction sequence without CPS
(define find
  (lambda (exp C)
    (match exp
      [(? symbol? x) '()]
      [`(,e1 ,e2)
       (if (and (symbol? e1)
                (symbol? e2))
           `(((:= rdx ,exp) . ((:= ctx ,(C 'rdx)))))
           (append (find e1 (lambda (v) (C `(,v ,e2))))
                   (find e2 (lambda (v) (C `(,e1 ,v))))))])))

; Yin Wang's original 'find-redexes' algorithm for λ-Calculus
(define find-redexes
  (lambda (exp)
    (letrec ([find
              (lambda (exp C)
                (match exp
                  [`(lambda (,x) ,e)
                   (find e (lambda (v) (C `(lambda (,x) ,v))))]
                  [`((lambda (,x) ,e1) ,e2)
                   (append `((,exp . ,C))
                           (find e1 (lambda (v) (C `((lambda (,x) ,v) ,e2))))
                           (find e2 (lambda (v) (C `((lambda (,x) ,e1) ,v)))))]
                  [`(,e1 ,e2)
                   (append (find e1 (lambda (v) (C `(,v ,e2))))
                           (find e2 (lambda (v) (C `(,e1 ,v)))))]
                  [exp '()]))])
      (find exp (lambda (v) v)))))

(tree-sum '(3 ((4 5) 6)) I)
(calc '(+ (* 2 3) (+ 1 (* 4 5))) I)

(cps '((f (f x)) ((g x) x)) I)
(anf '((f (f x)) ((g x) x)) I)

(left-most '((f (f x)) (g (z (t b)))) I)
(right-most '((f (f x)) (g (z (t b)))) I)

(unwrap1 '((g (z (t b))) (f (f x))) I I)
(unwrap-arith '(* (+ (* (g x) (f 2)) (h 3)) (i 5)) I I)
(flatten '((g (z (t b))) (f (f x))))
(flatten-arith '(* (+ (* (g x) (f 2)) (h 3)) (i 5)))

(find '((f (f x)) ((g x) (t b))) I)