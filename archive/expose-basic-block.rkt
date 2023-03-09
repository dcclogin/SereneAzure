#lang racket
(require racket/trace)

(define expose-basic-block
  (lambda (exp)
    (define !
      (lambda (exp ctx C)
        (match exp
          [`(begin . ,s)
           (! s ctx
              (lambda (v)
                (cond
                  [(null? v) (C v)]
                  [(eqv? C id) `([start (lambda () (begin ,@v))] ,@(C `()))]
                  ;; if s = (b$?), then leap forward.
                  [(jmp? (car v)) (C (car v))]
                  [else (match ctx
                          [`(,b jmp2 ,b1 ,b2)
                           `([,b (lambda () (begin ,@v))] ,@(C `(,b)))]
                          [c (let ([b (new-block 'b)])
                               `([,b (lambda () (begin ,@v))] ,@(C `(,b))))])])))]
          ;; nested if: ctx = (jmp2 th$ el$)
          [`(if ,icnd ,ith ,iel)
           (match ctx
             [`(jmp2 ,b1 ,b2)
              (! ith ctx
                 (lambda (ith$)
                   (! iel ctx
                      (lambda (iel$)
                        (! icnd `(jmp2 ,ith$ ,iel$) C)))))])]
          ;; processed by previous passes, cnd won't be #t/#f
          [`(,s1 ... (if ,cnd ,th ,el) ,s2 ...)
           #:when (seq? s1)
           (define ctx* (match ctx
                          [`(,b jmp2 ,b1 ,b2) (cdr ctx)]
                          [ctx ctx]))
           (! `(begin . ,s2) ctx*
              (lambda (s$2)
                (define ctx@ (if (null? s$2) ctx* `(jmp ,s$2)))
                (! th ctx@
                   (lambda (th$)
                     (! el ctx@
                        (lambda (el$)
                          (! cnd `(jmp2 ,th$ ,el$)
                             (lambda (cnd$) (C `(,@s1 ,cnd$))))))))))]
          ;; s2 must not be empty
          [`(,s1 ... (while ,c ,s0) ,s2 ...)
           #:when (seq? s1)
           (define ctx* (match ctx
                          [`(,b jmp2 ,b1 ,b2) (cdr ctx)]
                          [ctx ctx]))
           (! `(begin . ,s2) ctx*
              (lambda (s$2) (define b (new-block 'b))
                (! s0 `(jmp (,b))
                   (lambda (s$0) (define ctx@
                                   (match* (ctx* s$2)
                                     [(`(jmp ,o$) '())
                                      `(,b jmp2 ,s$0 ,o$)]
                                     [(_ _) `(,b jmp2 ,s$0 ,s$2)]))
                     (! c ctx@
                        (lambda (c$)
                          (if (null? s1)
                              (C `(,c$))
                              (! s1 `(jmp ,c$) C))))))))]
          ;; compare position
          [`(,op . ,as)
           #:when (memq op ops)
           (match ctx
             [`(jmp2 ,th$ ,el$)
              (cond
                [(equal? th$ el$) (C th$)]
                [else (C `(if ,exp ,th$ ,el$))])])]
          ;; a 'straight' line ended with 0~2 jumps 
          [`(,h ... ,t)
           (match ctx
             ;; no jump
             ['tail (C exp)]
             ;; single jump
             [`(jmp ,b) (C `(,@(shortcut1 exp) ,b))]
             ;; double jumps
             [`(jmp2 ,b1 ,b2)
              (C `(,@(shortcut1 h) ,(shortcut `(if ,t ,b1 ,b2))))]
             ;; named double jumps
             [`(,b jmp2 ,b1 ,b2)
              (C `(,@(shortcut1 h) ,(shortcut `(if ,t ,b1 ,b2))))])]
          ['() (C '())])))
    (define id (lambda (x) x))
    (define enter
      (lambda (x)
        `(letrec ,x (start))))
    (define seq?
      (lambda (ls)
        (match ls
          ['() #t]
          [`(,a . ,d)
           (match a
             [`(if . ,s) #f]
             [`(while . ,s) #f]
             [else (seq? d)])])))
    (define shortcut
      (lambda (exp)
        (match exp
          [`(if #t ,e1 ,e2) e1]
          [`(if #f ,e1 ,e2) e2]
          [else exp])))
    (define (shortcut1 ls)
      (match ls
        ['() '()]
        [`((set! ,x ,x) . ,ls*) (shortcut1 ls*)]
        [`(,a . ,ls*) `(,a . ,(shortcut1 ls*))]))
    ;; need modification
    (define jmp?
      (lambda (s)
        (match s
          [`(,(? symbol? x)) #:when (not (eqv? x 'read)) #t]
          [else #f])))
    (define ops '(eq? < > <= >= not))
    (define gensym
      (lambda (split)
        (let ([n -1])
          (lambda (s)
            (set! n (add1 n))
            (string->symbol
             (string-append
              (symbol->string s) split (number->string n)))))))
    (define new-var (gensym "."))
    (define new-block (gensym "$"))
    #;(trace !)
    (enter (! exp 'tail id))))

(define ! (lambda (exp) (pretty-print (expose-basic-block exp))))


(! '(begin
      (set! x 5)
      (set! y 6)
      (if (eq? x 4)
          (begin (set! x 1) (set! y 2))
          (begin (set! x 3)
                 (if (if (if (if (eq? y 5)
                                 (begin #t)
                                 (begin #f))
                             (begin #t)
                             (begin #f))
                         (begin #f)
                         (begin #t))
                     (begin (set! y 3))
                     (begin (set! y 4)))
                 (set! z 9)))
      5))
(! '(begin
      (set! x 5)
      (set! y 0)
      (while (begin
               (if (< x 10)
                   (begin (eq? x 5))
                   (begin #f)))
             (begin
               (if (> x 6)
                   (begin (set! y (+ y x)))
                   (begin (set! y (+ y 1))))
               (set! x (+ x 1))))
      (if (eq? x 0)
          (begin (set! y 0))
          (begin (set! x 0)))
      y))

(! '(begin
      (while (begin
               (< x 10))
             (begin
               (while (begin
                        (< y 20))
                      (begin
                        (set! z (+ x y))
                        (set1 y (+ y 1))))
               (set! x (+ x 1))))
      z))