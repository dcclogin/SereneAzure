#lang racket
(require graph)
(require racket/trace)
(require "utilities.rkt")


(define binop '(+ - eq? < > <= >=))
(define R '(rcx rdx rsi rdi r8 r9 r10 rbx r12 r13 r14))
(define R-er '(rcx rdx rsi rdi r8 r9 r10))
(define R-ee '(rbx r12 r13 r14))
(define conventions '(rdi rsi rdx rcx r8 r9))

;; make begin at the top
(define (make-begin exp)
  (cond
    [(eqv? 'begin (car exp)) exp]
    [else `(begin ,@exp)]))


;; pre-process function arguments
(define (impose-conventions-fun fun)
  (define (! exp)
    (match exp
      [`(begin . ,s*) (make-begin (! s*))]
      [`(if ,cnd ,thn ,els)
       (list `(if ,@(! cnd) ,(! thn) ,(! els)))]
      [`(set! ,x (fun-app ,f . ,args))
       (define mvs (mk-moves conventions args))
       `(,@mvs (set! ,x (fun-app ,f)))]
      [`(set! ,x ,e) (list exp)]
      [`(,bop ,a ,b)  #:when (memq bop binop) (list exp)]
      [(? symbol?) (list exp)]
      [(? fixnum?) (list exp)]
      [(? boolean?) (list exp)]
      [`(fun-app ,f . ,args)
       (define mvs (mk-moves conventions args))
       `(,@mvs (fun-app ,f))]
      [`(,s . ,s*) `(,@(! s) . ,(! s*))]
      ['() exp]))
  (define (mk-moves to from)
    (cond
      [(or (null? to) (null? from)) '()]
      [else (let ([mv `(set! ,(car to) ,(car from))])
              `(,mv ,@(mk-moves (cdr to) (cdr from))))]))
  (match fun
    [(Def f args T info exp)
     (let ([mvs (mk-moves (map car args) conventions)])
       (Def f args T info
            (make-begin `(,@mvs ,@(cdr (! exp))))))]))

(define (impose-conventions p)
  (match p
    [(ProgramDefs info funs)
     (ProgramDefs info (map impose-conventions-fun funs))]))

;; flip the controw flow
;; ~ transpose the CFG

;; make all symbols in ls a set

;; structure of a expression with its live after set
(struct LA (exp after) #:transparent)
;; A: live-after set
(define (uncover-live-fun fun)
  (define (transpose exp)
    (match exp
      [`(begin . ,s*) `(begin ,@(transpose s*))]
      [`(if ,cnd ,s1 ,s2)
       (match cnd
         [`(if . ,ss)
          `(if ,(transpose cnd) ,(transpose s1) ,(transpose s2))]
         [else `(if ,cnd ,(transpose s1) ,(transpose s2))])]
      [`(while ,c ,e)
       `(while ,(transpose c) ,(transpose e))]
      [`(,s . ,s*)
       (match s
         [`(if . ,ss) `(,@(transpose s*) ,(transpose s))]
         [`(while . ,ss) `(,@(transpose s*) ,(transpose s))]
         [else `(,@(transpose s*) ,s)])]
      ['() '()]))
  (define (set-symbol ls)
    (cond
      [(null? ls) (set)]
      [(symbol? (car ls))
       (set-union (set (car ls)) (set-symbol (cdr ls)))]
      [else (set-symbol (cdr ls))]))
  (define (! exp A)
    (match exp
      [`(begin . ,s*)
       (define-values (e* A*) (! s* A))
       (values (make-begin e*) A*)]
      [`(if ,cnd ,thn ,els)
       (define-values (thn* A-thn) (! thn A))
       (define-values (els* A-els) (! els A))
       (define-values (cnd* A-cnd) (! cnd (set-union A-thn A-els)))
       (values `(if ,cnd* ,thn* ,els*) A-cnd)]
      [`(,bop ,a ,b)
       #:when (memq bop binop)
       (values (LA exp A) (set-union A (set-symbol (list a b))))]
      [(? fixnum? n) (values (LA exp A) A)]
      [(? boolean? b) (values (LA exp A) A)]
      [(? symbol? x) (values (LA exp A) (set-union A (set x)))]
      [`(set! ,x ,y)
       #:when (symbol? y)
       (define A* (set-union (set y) (set-subtract A (set x))))
       (values (LA exp A) A*)]
      [`(set! ,x ,y)
       #:when (or (fixnum? y) (boolean? y))
       (define A* (set-subtract A (set x)))
       (values (LA exp A) A*)]
      [`(set! ,x (,bop ,a ,b))
       #:when (memq bop binop)
       (define A* (set-union (set-symbol (list a b)) (set-subtract A (set x))))
       (values (LA exp A) A*)]
      [`(set! ,x (fun-app ,f))
       (define A* (set-union (set f) (set-subtract A (set x))))
       (values (LA exp A) A*)]
      [`(set! ,x (fun-ref ,f))
       (define A* (set-subtract A (set x)))
       (values (LA exp A) A*)]
      [`(fun-app ,f) (values (LA exp A) (set-union A (set f)))]
      [`(,s . ,s*)
       (define-values (e A*)(! s A))
       (define-values (e* A**) (! s* A*))
       (values `(,e . ,e*) A**)]
      ['() (values '() A)]))
  (match fun
    [(Def f args T info exp)
     (define-values (exp* A*) (! (transpose exp) (set)))
     (Def f args T info (transpose exp*))]))

(define (uncover-live p)
  (match p
    [(ProgramDefs info funs)
     (ProgramDefs info (map uncover-live-fun funs))]))



;; build interference graph
(define (build-interference-fun fun)
  (define G (undirected-graph
             '([rcx rdx]
               [rcx rsi]
               [rcx rdi]
               [rcx r8]
               [rcx r9]
               [rcx r10]
               [rdx rsi]
               [rdx rdi]
               [rdx r8]
               [rdx r9]
               [rdx r10]
               [rsi rdi]
               [rsi r8]
               [rsi r9]
               [rsi r10]
               [rdi r8]
               [rdi r9]
               [rdi r10]
               [r8 r9]
               [r8 r10]
               [r9 r10])))
  (define (confl ls)
    (lambda (v)
      (cond
        [(null? ls) v]
        [(eqv? v (car ls)) ((confl (cdr ls)) v)]
        [else (add-edge! G (car ls) v)
              ((confl (cdr ls)) v)])))
  (define (! exp ctx)
    (match* (exp ctx)
      [((LA (? symbol? x) A) _) 'do-nothing]
      [((LA (? fixnum? x) A) _) 'do-nothing]
      [((LA (? boolean? x) A) _) 'do-nothing]
      [((LA `(set! ,x (,bop ,a ,b)) A) _)
       #:when (memq bop binop)
       (map (lambda (v)
              (if (not (eqv? v x))
                  (begin (add-edge! G x v) v)
                  v))
            (set->list A))]
      [((LA `(set! ,x (fun-ref ,f)) A) _)
       (map (lambda (v)
              (if (not (eqv? v x))
                  (begin (add-edge! G x v) v)
                  v))
            (set->list A))]
      ;; y: int, bool, symbol, fun-ref
      [((LA `(set! ,x ,y) A) _)
       #:when (or (fixnum? y)
                  (symbol? y)
                  (boolean? y))
       (map (lambda (v)
              (if (and (not (eqv? v x)) (not (eqv? v y)))
                  (begin (add-edge! G x v) v)
                  v))
            (set->list A))]
      ;; all caller-saved be written to
      [((LA `(set! ,x (fun-app ,f)) A) _)
       (map (confl (cons x R-er)) (set->list A))]
      ;; all caller-saved be written to
      [((LA `(fun-app ,f) A) _)
       (map (confl R-er) (set->list A))]
      [((LA `(,bop ,a ,b) A) _)
       #:when (memq bop binop) 'do-nothing]
      [(`(if ,cnd ,thn ,els) _)
       (begin
         (! cnd 'cnd)
         (! thn ctx)
         (! els ctx))]
      [(`(begin . ,s*) _) (! s* ctx)]
      [(`(,a . ,d) _)
       (begin (! a ctx)
              (! d ctx))]
      [('() _) 'end]))
  #;(trace !)
  (match fun
    [(Def f args T info exp)
     (! exp 'tail)
     (define info* (cons `(interference . ,G) info))
     (Def f args T info* exp)]))

(define (build-interference p)
  (match p
    [(ProgramDefs info funs)
     (ProgramDefs info (map build-interference-fun funs))]))


;; allocate registers via Chaitin-Briggs algorithm
(define (graph-coloring-fun fun)
  (define g #f)
  (define s #f)
  (define k (length R))
  (define color-table '())
  (define spill-table '())  ;; never set 0
  ;; first step
  (define (!-1 G)
    (define ls (order-smallest-last G))
    (cond
      [(null? ls) (!-3)]
      [else
       (define lst (last ls))
       (define k* (length (get-neighbors G lst)))
       (if (< k* k)
           (begin
             (set! s (cons lst s))
             (remove-vertex! G lst)
             (!-1 (graph-copy G)))
           (begin
             (!-2 (graph-copy G))))]))
  ;; second step
  (define (!-2 G)
    (define ls (order-smallest-last G))
    (define lst (last ls))
    (set! s (cons lst s))
    (remove-vertex! G lst)
    (define ls* (order-smallest-last G))
    (define lst* (last ls*))
    (define k* (length (get-neighbors G lst*)))
    (if (< k* k)
        (!-1 (graph-copy G))
        (!-2 (graph-copy G))))
  ;; third step
  (define (!-3)
    (cond
      [(null? s) 'the-end]
      [else (define v (car s))
            (set! s (cdr s))
            (define nc (map get-color (get-neighbors g v)))
            (define color (get-least-free nc))
            (if (<= color k)
                (begin
                  (set! color-table (cons `(,v . ,color) color-table))
                  (!-3))
                (let* ([sp (select-spill `(,v . ,s))]
                       [fv (new-fvar 'fv)])
                  (set! spill-table (cons `(,sp . ,fv) spill-table))
                  (remove-vertex! g sp)
                  (set! s '())
                  (set! color-table '())
                  (!-1 (graph-copy g))))]))
  ;; get least free color with neighbors' colors
  (define (get-least-free ncolor)
    (define (! ls c)
      (cond
        [(memq c ls) (! ls (add1 c))]
        [else c]))
    (! ncolor 1))
  ;; min(cost/degree)
  (define (select-spill ls)
    (define ds (map get-degree ls))
    (define maxd (apply max ds))
    (define (! ls ds)
      (cond
        [(= (car ds) maxd) (car ls)]
        [else (! (cdr ls) (cdr ds))]))
    (! ls ds))
  ;; compute degree of a node
  (define (get-degree node)
    (length (get-neighbors g node)))
  ;;
  (define (get-color node)
    (cond
      [(assv node color-table)
       => (lambda (pr) (cdr pr))]
      [else 0]))
  ;; frame variables
  (define offset/fv 0)
  (define (new-fvar s)
    (set! offset/fv (add1 offset/fv))
    (string->symbol
     (string-append
      (symbol->string s) "." (number->string offset/fv))))
  (match fun
    [(Def f args T info exp)
     ;; initialize the global graph, stack, and color table
     (set! g (cdr (assv 'interference info)))
     (set! s '())
     (set! color-table '())
     (!-1 (graph-copy g))
     (define info* `((frame-variables . ,offset/fv)
                     (colors . ,color-table)
                     (spills . ,spill-table) . ,info))
     (Def f args T info* exp)]))

(define (graph-coloring p)
  (match p
    [(ProgramDefs info funs)
     (ProgramDefs info (map graph-coloring-fun funs))]))




;; write-back allocated registers to the original expression
(define (allocate-registers-fun fun)
  (define rmap '())
  (define vmap '())
  (define amap '())
  (define ree R-ee)
  (define callee-used '())
  (define (divide-map! ct)
    (cond
      [(null? ct) 'the-end]
      [(memq (caar ct) R-er)
       (set! rmap (cons `(,(cdar ct) . ,(caar ct)) rmap))
       (divide-map! (cdr ct))]
      [else (set! vmap (cons (car ct) vmap))
            (divide-map! (cdr ct))]))
  (define (get-allocate-map! vmap)
    (cond
      [(null? vmap) 'the-end]
      [(assv (cdar vmap) rmap)
       => (lambda (pr)
            (set! amap (cons `(,(caar vmap) . ,(cdr pr)) amap))
            (get-allocate-map! (cdr vmap)))]
      [else (define r (pick!))
            (set! rmap (cons `(,(cdar vmap) . ,r) rmap))
            (set! amap (cons `(,(caar vmap) . ,r) amap))
            (get-allocate-map! (cdr vmap))]))
  (define (pick!)
    (let ([r (car ree)])
      (set! callee-used (cons r callee-used))
      (set! ree (cdr ree))
      r))
  (define (! exp ctx)
    (match* (exp ctx)
      [((LA (? symbol? x) A) _) (rwt x)]
      [((LA (? fixnum? x) A) _) x]
      [((LA (? boolean? x) A) _) x]
      [((LA `(set! ,x (,bop ,a ,b)) A) _)
       #:when (memq bop binop)
       `(set! ,(rwt x) (,bop ,(rwt a) ,(rwt b)))]
      [((LA `(set! ,x (fun-ref ,f)) A) _)
       `(set! ,(rwt x) (fun-ref ,f))]
      [((LA `(set! ,x ,y) A) _)
       #:when (or (fixnum? y)
                  (symbol? y)
                  (boolean? y))
       `(set! ,(rwt x) ,(rwt y))]
      [((LA `(set! ,x (fun-app ,f)) A) _)
       `(set! ,(rwt x) (call ,(rwt f)))]
      [((LA `(fun-app ,f) A) _)
       `(tail-call ,(rwt f))]
      [((LA `(,bop ,a ,b) A) _)
       #:when (memq bop binop)
       `(,bop ,(rwt a) ,(rwt b))]
      [(`(if ,cnd ,thn ,els) _)
       `(if ,(! cnd 'cnd)
            ,(! thn ctx)
            ,(! els ctx))]
      [(`(begin . ,s*) _) (make-begin (! s* ctx))]
      [(`(,a . ,d) _)
       `(,(! a ctx) . ,(! d ctx))]
      [('() _) '()]))
  (define (rwt x)
    (cond
      [(assv x amap)
       => (lambda (pr) (cdr pr))]
      [else x]))
  (match fun
    [(Def f args T info exp)
     (divide-map! (cdr (assv 'colors info)))
     (set! amap (cdr (assv 'spills info)))
     (get-allocate-map! vmap)
     (define info* 
       `((num-root-spills . 0)
         (num-params      . ,(length args))
         (callee-saved    . ,callee-used)
         ,(assv 'frame-variables info)))
     (Def f args T info* (! exp 'tail))]))


(define (allocate-registers p)
  (match p
    [(ProgramDefs info funs)
     (ProgramDefs info (map allocate-registers-fun funs))]))




(define (sum1 fun)
  (pretty-print (uncover-live-fun (impose-conventions-fun fun))))
(define (sum2 fun)
  (pretty-print (graph-coloring-fun (build-interference-fun (uncover-live-fun (impose-conventions-fun fun))))))
(define (sum3 fun)
  (pretty-print (allocate-registers-fun (graph-coloring-fun (build-interference-fun (uncover-live-fun (impose-conventions-fun fun)))))))
(define (sumg fun)
  (build-interference-fun (uncover-live-fun (impose-conventions-fun fun))))