#lang racket
(require racket/trace)

(define gensym
  (lambda (split)
    (let ([n -1])
      (lambda (s)
        (set! n (add1 n))
        (string->symbol
         (string-append
          (symbol->string s) split (number->string n)))))))

(define new-var (gensym "."))

;; semantic object def
(struct Lam (F) #:transparent)
(struct Syn (s) #:transparent)
;; type def
(struct Type (t) #:transparent)
(struct -> (St Tt) #:transparent)

(define refl
  (lambda (tm Ty)
    (match* (tm Ty)
      [(v (-> S T))
       (Lam (lambda (x)
              (refl `(,v ,(reify x S)) T)))]
      [(_ (Type t)) (Syn tm)])))

(define reify
  (lambda (stm Ty)
    (match* (stm Ty)
      [((Syn s) (Type t)) s]
      [((Lam F) (-> S T))
       (define x (new-var 'v))
       `(λ (,x) ,(reify (F (refl x S)) T))])))

#;(trace refl)
#;(trace reify)

(define NbE
  (lambda (tm Ty)
    (reify (refl tm Ty) Ty)))

(NbE 'f (-> (Type 'a) (Type 'b)))
(NbE 'f (-> (-> (Type 'a) (Type 'b))
            (-> (Type 'a) (Type 'b))))

;; Kleene's Smn Theorem: S11
(define s11
  (lambda (f x)
    (let ([y (new-var 'y)])
      `(λ (,y) (,f ,x ,y)))))
;; pretty much like PE/NbE
(s11 '(λ (x y) (+ x y)) 3)


