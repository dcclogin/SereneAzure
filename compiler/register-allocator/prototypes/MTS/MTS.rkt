#lang racket
(require "utilities.rkt")


(define op-prims '(+ - eq? < > <= >= not and or))
;; structure for ends
(struct Exp-Ln (exp mark) #:transparent)

(define mark-deaths
  (lambda (exp)
    (define cc 1)   ;; call-live flag
    (define incc! (lambda () (set! cc (add1 cc))))
    ;; union of two alists wrt keys
    (define (U ls1 ls2)
      (cond
        [(null? ls1) ls2]
        [(assv (caar ls1) ls2) (U (cdr ls1) ls2)]
        [else (cons (car ls1) (U (cdr ls1) ls2))]))
    ;; l - al
    (define (X l al)
      (match l
        ['() '()]
        [`(,(? symbol? a) . ,d)
         (cond
           [(assv a al)
            => (lambda (pr)
                 (cond
                   [(not (unbox (cdr pr))) (X d al)]
                   [(= cc (unbox (cdr pr))) (X d al)]
                   [else (set-box! (cdr pr) #f) (X d al)]))]
           [else (cons `(,a . ,(box cc)) (X d al))])]
        [`(,a . ,d) (X d al)]))
    ;; collect keys
    (define (K al)
      (cond
        [(null? al) '()]
        [else (cons (caar al) (K (cdr al)))]))
    ;; Exp * DeathList -> Exp-Ln * DeathList
    (define (mark exp ds)
      (match exp
        [`(begin . ,s*)
         (define-values (s@ ds@) (mark s* ds))
         (values `(begin . ,s@) ds@)]
        [`(if ,cnd ,s1 ,s2)
         (define-values (s@1 ds@1) (mark s1 ds))
         (define-values (s@2 ds@2) (mark s2 ds))
         (let ([dsu (U ds@1 ds@2)])
           (define-values (cnd@ ds@) (mark cnd dsu))
           (values `(if ,cnd@ ,s@1 ,s@2) ds@))]
        ;; loop: need more accurate approximation
        [`(while ,c ,e)
         (define-values (e@ ds-e) (mark e ds))
         (define-values (c@ ds-c) (mark c ds-e))
         (define-values (e* ds-e*) (mark e ds-c))
         (define-values (c* ds-c*) (mark c ds-c))
         (values `(while ,c* ,e*) ds-c)]
        [`(,op . ,es)
         #:when (memq op `(,@op-prims vector-ref))
         (define ds@ (X es ds))
         (values (Exp-Ln exp (K ds@)) (U ds ds@))]
        ['(read) (incc!) (values (Exp-Ln exp '()) ds)]
        [`(collect ,n) (incc!) (values (Exp-Ln exp '()) ds)]
        ;; tail calls
        [`(fun-app ,f . ,args)
         (define ds@ (X `(,f ,@args) ds))
         (values (Exp-Ln exp (K ds@)) (U ds ds@))]
        ['(void) (values (Exp-Ln exp '()) ds)]
        [`(set! ,x ,e)
         (match e
           [`(,op . ,es)
            #:when (memq op `(,@op-prims vector-ref))
            (define ds@ (X `(,x ,@es) ds))
            (values (Exp-Ln exp (K ds@)) (U ds ds@))]
           ['(read) (define ds@ (X `(,x) ds)) (incc!)
                    (values (Exp-Ln exp (K ds@)) (U ds ds@))]
           ;; new
           [`(fun-app ,f . ,args)
            (define ds@1 (X `(,x) ds))
            (incc!)
            (define ds@2 (X `(,f ,@args) ds))
            (define ds@ (U ds@1 ds@2))
            (values (Exp-Ln exp (K ds@)) (U ds ds@))]
           [`(fun-ref ,f)
            (define ds@ (X `(,x) ds))
            (values (Exp-Ln exp (K ds@)) (U ds ds@))]
           [`(global-value ,v)
            (define ds@ (X `(,x) ds))
            (values (Exp-Ln exp (K ds@)) (U ds ds@))]
           [`(allocate ,n ,T)
            (define ds@ (X `(,x) ds))
            (values (Exp-Ln exp (K ds@)) (U ds ds@))]
           [else (define ds@ (X `(,x ,e) ds))
                 (values (Exp-Ln exp (K ds@)) (U ds ds@))])]
        [`(vector-set! ,v ,n ,e)
         (define ds@ (X `(,v ,e) ds))
         (values (Exp-Ln exp (K ds@)) (U ds ds@))]
        [`(,s . ,s*)
         (define-values (s@ ds@) (mark s ds))
         (define-values (s@* ds@*) (mark s* ds@))
         (values `(,s@ . ,s@*) ds@*)]
        [(? symbol? x)
         (define ds@ (X `(,x) ds))
         (values (Exp-Ln x (K ds@)) (U ds ds@))]
        [(? fixnum? n) (values (Exp-Ln n '()) ds)]
        [(? boolean? b) (values (Exp-Ln b '()) ds)]
        ['(void) (values (Exp-Ln exp '()) ds)]
        ['() (values '() ds)]))
    ;; flip the control flow
    (define (flip-cf exp)
      (match exp
        [`(begin . ,s*) `(begin ,@(flip-cf s*))]
        [`(if ,cnd ,s1 ,s2)
         (match cnd
           [`(if . ,ss)
            `(if ,(flip-cf cnd) ,(flip-cf s1) ,(flip-cf s2))]
           [else `(if ,cnd ,(flip-cf s1) ,(flip-cf s2))])]
        [`(while ,c ,e)
         `(while ,(flip-cf c) ,(flip-cf e))]
        [`(,s . ,s*)
         (match s
           [`(if . ,ss) `(,@(flip-cf s*) ,(flip-cf s))]
           [`(while . ,ss) `(,@(flip-cf s*) ,(flip-cf s))]
           [else `(,@(flip-cf s*) ,s)])]
        ['() '()]))
    (define-values (e ds) (mark (flip-cf exp) '()))
    (values (flip-cf e) ds)))
;; liveness analysis for one function
(define uncover-live-proc
  (lambda (def)
    (match def
      [(Def f args T info* exp)
       (let-values ([(exp@ ds) (mark-deaths exp)])
         (let ([info@ `((live-map . ,ds) ,@info*)])
           (Def f args T info@ exp@)))])))
;; liveness analysis for the program
(define uncover-live
  (lambda (p)
    (match p
      [(ProgramDefs info defs)
       (ProgramDefs info (map uncover-live-proc defs))])))



(define alloc
  (lambda (proc)
    ;; really need it?
    (define pe-ts
      (lambda (exp)
        (match exp
          [`(+ ,(? fixnum? a1) ,(? fixnum? a2)) (+ a1 a2)]
          [`(- ,(? fixnum? a)) (- 0 a)]
          [else #f])))
    (define ts
      (lambda (exp m ctx)
        (match* (exp ctx)
          ;; TODO: put it in mkd
          [((Exp-Ln `(set! ,x ,e) die) _)                         
           #:when (and (memq x die)
                       (not (equal? e '(read)))
                       (not (eqv? ctx 'loop)))
           (values '() m)]
          ;; [c] constant
          [((Exp-Ln c die) _)                                    
           #:when (or (fixnum? c) (boolean? c)) (values `(,c) m)]
          ;; fun-ref
          [((Exp-Ln `(fun-ref ,f) die) _)
           (values (list `(fun-ref ,f)) m)]
          ;; fun-app: shuffling call-lives -> bind args
          [((Exp-Ln `(fun-app ,f . ,args) die) `(,x . lhs))
           (define-values (m0 exp-ls0) (load m f))
           (define m-clive (rm-d m die))
           (define clives (M-reg-map m-clive))
           (define-values (m1 exp-ls1) (move-clives m0 clives))
           (define smap (generate-shuffle-map m1 args))
           (define spaths (identify-paths smap))
           (define exp-ls2 (generate-shuffle-paths spaths))
           (values `(,@exp-ls0
                     ,@exp-ls1
                     ,@exp-ls2 (call ,(rwt f m1))) (rm-d m1 die))]
          ;; fun-app tail call
          [((Exp-Ln `(fun-app ,f . ,args) die) 'tail)
           (define-values (m0 exp-ls0) (load m f))
           (define smap (generate-shuffle-map m0 args))
           (define spaths (identify-paths smap))
           (define sloops (identify-loops smap))
           (define exp-ls1 (generate-shuffle-paths spaths))
           (define exp-ls2 (generate-shuffle-loops sloops))
           (values `(,@exp-ls0
                     ,@exp-ls1
                     ,@exp-ls2 (tail-call ,(rwt f m0))) (rm-d m0 die))]
          ;; introduced by GC
          [((Exp-Ln `(global-value ,x) die) _)
           (values (list `(global-value ,x)) m)]
          [((Exp-Ln `(allocate ,n ,T) die) _)
           (values (list `(allocate ,n ,T)) m)]
          [((Exp-Ln `(collect ,n) die) _)
           (values (list `(collect ,n)) m)]
          ;; tuple mutate
          [((Exp-Ln `(vector-set! ,v ,n ,e) die) _)
           (define-values (e* m*)
             (ts (Exp-Ln e die) m `(,v . lhs)))
           (ts (Exp-Ln v die) m* `(,e* . (rhs ,n)))]
          ;; [x = e]
          [((Exp-Ln `(set! ,x ,e) die) _)                         
           (define-values (e* m*)
             (ts (Exp-Ln e die) m `(,x . lhs)))
           (ts (Exp-Ln x die) m* `(,e* . rhs))]
          ;; v[n] = e
          [((Exp-Ln x die) `(,e . (rhs ,n)))
           #:when (symbol? x)
           (define-values (m* exp-ls) (load m x))
           (match e
             [`(,ls ... ,et)
              (define s `(vector-set! ,(rwt x m*) ,n ,et))
              (values `(,@ls ,@exp-ls ,s) m*)])]
          ;; [x] = e
          [((Exp-Ln x die) `(,e . rhs))                           
           #:when (symbol? x)
           (define-values (m* exp-ls) (load m x))
           (match e
             ['((read))
              (define s `(set! ,(rwt x m*) (read)))
              (values `(,@exp-ls ,s) (rm-d m* die))]
             [`(,ls ... ,et)
              (define s `(set! ,(rwt x m*) ,et))
              (values `(,@ls ,@exp-ls ,s) m*)])]
          ;; x = [y] or [y]
          [((Exp-Ln y die) _)                                     
           #:when (symbol? y)
           (define-values (m* exp-ls) (load m y))
           (values `(,@exp-ls ,(rwt y m*)) (rm-d m* die))]
          ;; including vector-ref
          [((Exp-Ln `(,op . ,es) die) _)
           #:when (memq op `(,@op-prims vector-ref))
           (cond
             [(pe-ts `(,op . ,es)) => (lambda (n) (values `(,n) m))]
             [else (define-values (m* exp-ls) (load m es))
                   (define s (rwt `(,op . ,es) m*))
                   (values `(,@exp-ls ,s) (rm-d m* die))])]
          [((Exp-Ln '(read) die) _) (values '((read)) m)]
          [((Exp-Ln '(void) die) _) (values '((void)) m)]
          ;; =>  (ListofIf * ModelAfter)  (... (if cnd th el))
          ;; the cnd part should never be a list of exp
          [(`(if ,cnd ,th ,el) _) 
           (define-values (cnd* m-cnd) (ts cnd m 'cnd))
           (define-values (th* m-th) (ts th m-cnd ctx))
           (define-values (el* m-el) (ts el m-cnd ctx))
           (define-values (m* exp-ls) (shuffle m-th m-el))
           (define exp@ (if (eqv? ctx 'tail)
                            `(if ,@cnd* ,th* ,el*)
                            `(if ,@cnd* ,th* (,@el* ,@exp-ls))))
           (values `(,exp@) m*)]
          ;; loop
          [(`(while ,c ,e) _)
           (define-values (c* mc) (ts c m 'loop))
           (define-values (e* me) (ts e mc 'loop))
           (values `((while ,c* ,e*)) me)]
          [(`(begin . ,seq) _)
           (define-values (e* m*) (ts seq m ctx))
           (values `(begin . ,e*) m*)]
          [(`(,a . ,d) _)
           #:when (not (null? d))
           (define-values (a* ma) (ts a m 'non-tail))
           (define-values (d* md) (ts d ma ctx))
           (values `(,@a* . ,d*) md)]
          [(`(,a . ,d) _)
           (define-values (a* ma) (ts a m ctx))
           (define-values (d* md) (ts d ma ctx))
           (values `(,@a* . ,d*) md)]
          [('() _) (values '() m)])))
    #;(trace ts)
    ;; load vars to m
    (define (load m vars)
      (match vars
        [(? fixnum? n) (values m '())]
        [(? boolean? b) (values m '())]
        [(? symbol? x)
         (cond
           [(in-regs-var? x m)
            => (lambda (pr)    ;; "lift" the binding to the top
                 (match m
                   [(M rm fvm rs)
                    (define rm@ `(,pr . ,(remove pr rm)))
                    (values (M rm@ fvm rs) '())]))]
           [(in-fvs-var? x m)
            => (lambda (pr)
                 (define-values (m@ r/pr) (select m x))
                 (match* (pr r/pr)
                   [(`(,x . ,fv) (? symbol? r))
                    (define exps (list `(set! ,r ,fv)))
                    (values (bind-a-reg x r m@) exps)]
                   [(`(,x . ,fv) `(,y . ,r))
                    (define-values (m@s exps) (save m@ y))
                    (define exps@ `(,@exps (set! ,r ,fv)))
                    (values (bind-a-reg x r m@s) exps@)]))]
           [else
            (define-values (m@ r/pr) (select m x))
            (match r/pr
              [(? symbol? r) (values (bind-a-reg x r m@) '())]
              [`(,y . ,r) (define-values (m@s exps) (save m@ y))
                          (values (bind-a-reg x r m@s) exps)])])]
        [`(,a1 . ,a2)
         (define-values (m1 exps1) (load m a1))
         (define-values (m2 exps2) (load m1 a2))
         (values m2 `(,@exps1 ,@exps2))]
        ['() (values m '())]))

    ;; save var in m
    (define (save m var)
      (match var
        [(? symbol? x)
         (cond
           [(in-fvs-var? x m) (values m '())]
           [(in-regs-var? x m)
            => (lambda (pr)
                 (match* (pr m)
                   [(`(,x . ,r) (M rm fvm rs))
                    (let ([fv (new-fvar 'fv)])
                      (define exps (list `(set! ,fv ,r)))
                      (define fvm@ `((,x . ,fv) . ,fvm))
                      (values (M rm fvm@ rs) exps))]))]
           [else (error 'save "can't save.")])]))

    ;; shuffling and generating moving instr
    (define shuffle
      (lambda (m1 m2)
        (define rm1 (M-reg-map m1))
        (define rm2 (M-reg-map m2))
        (match* (rm1 rm2)
          [(`((,x1 . ,r1) . ,rm1*) `((,x2 . ,r2) . ,rm2*))
           #:when (and (eqv? x1 x2) (not (eqv? r1 r2)))
           (values m1 (list `(set! ,r1 ,r2)))]
          [(_ _) (values m1 '())])))
    
    ;; shuffling call-lives at fun-app position
    (define move-clives
      (lambda (m clives)
        (define exp-ls '())
        (define (shuffle! m clives)
          (match clives
            ['() m]
            [`((,x . ,r) . ,d)
             #:when (memq r R-ee) (shuffle! m d)]
            [`((,x . ,r) . ,d)
             (define-values (m* r*) (pick-a-reg m R-ee))
             (cond
               [r* (define i `(set! ,r* ,r)) (addto! r*)
                   (define m0 (unbind-a-reg x m*))
                   (define m@ (bind-a-reg x r* m0))
                   (set! exp-ls (cons i exp-ls))
                   (shuffle! m@ d)]
               [else (define-values (m* exps) (save m x))
                     (define m@ (unbind-a-reg x m*))
                     (set! exp-ls `(,@exps ,@exp-ls))
                     (shuffle! m@ d)])]))
        (values (shuffle! m clives) exp-ls)))
    
    ;; generate shuffle map for f+arguments
    (define generate-shuffle-map
      (lambda (m args)
        (define map '())
        (define (! args convs)
          (match args
            ['() 'nothing]
            [`(,a . ,d)
             (cond
               [(in-regs-var? a m)
                => (lambda (pr)
                     (define i `(,(cdr pr) . ,(car convs)))
                     (set! map `(,@map ,i))
                     (! d (cdr convs)))]
               [(in-fvs-var? a m)
                => (lambda (pr)
                     (define i `(,(cdr pr) . ,(car convs)))
                     (set! map `(,@map ,i))
                     (! d (cdr convs)))]
               [else (define i `(,a . ,(car convs)))
                     (set! map `(,@map ,i))
                     (! d (cdr convs))])]))
        (! args conventions)
        map))
    
    ;; identify paths and loops in a shuffle map
    (define identify-paths
      (lambda (m)
        (define global-map m)
        (define lhs (map car m))
        (define rhs (map cdr m))
        (define (expose-entrances lhs rhs)
          (cond
            [(null? lhs) '()]
            [(memq (car lhs) rhs) (expose-entrances (cdr lhs) rhs)]
            [else (cons (car lhs) (expose-entrances (cdr lhs) rhs))]))
        (define (get-path ent)
          (cond
            [(assv ent global-map)
             => (lambda (pr)
                  (set! global-map (remove pr global-map))
                  `(,pr . ,(get-path (cdr pr))))]
            [else '()]))
        (map get-path (expose-entrances lhs rhs))))
    ;; identify-loops
    (define identify-loops
      (lambda (m)
        (define (rm-trivial m)
          (cond
            [(null? m) m]
            [(eqv? (caar m) (cdar m)) (rm-trivial (cdr m))]
            [else (cons (car m) (rm-trivial (cdr m)))]))
        (define (rm-non-loop l)
          (cond
            [(null? l) l]
            [(null? (car l)) (rm-non-loop (cdr l))]
            [(eqv? (caaar l) (cdr (last (car l))))
             (cons (car l) (rm-non-loop (cdr l)))]
            [else (rm-non-loop (cdr l))]))
        (define (get-loop ent)
          (cond
            [(assv ent global-map)
             => (lambda (pr)
                  (set! global-map (remove pr global-map))
                  `(,pr . ,(get-loop (cdr pr))))]
            [else '()]))
        (define global-map (rm-trivial m))
        (define entrances (map car m))
        (rm-non-loop (map get-loop entrances))))
    
    ;; generate code according to paths
    (define generate-shuffle-paths
      (lambda (paths)
        (define (gen-code pr)
          (match pr
            [`(,x . ,r) `(set! ,r ,x)]))
        (define (gen-code-paths paths)
          (match paths
            ['() '()]
            [`(,a . ,d)
             (let ([a* (map gen-code (reverse a))]
                   [d* (gen-code-paths d)])
               `(,@a* . ,d*))]))
        (gen-code-paths paths)))
    ;; generate code according to loops
    (define generate-shuffle-loops
      (lambda (loops)
        (define (gen-code pr)
          (match pr
            [`(,x . ,r) `(set! ,r ,x)]))
        (define (gen-code-loops loops)
          (match loops
            ['() '()]
            [`(,a . ,d)
             (let* ([t `(set! rax ,(cdr (last a)))]
                    [h `(set! ,(cdr (first a)) rax)]
                    [a* (map gen-code (reverse (cdr a)))]
                    [d* (gen-code-loops d)])
               `(,t ,@a* ,h . ,d*))]))
        (gen-code-loops loops)))

    
    ;; select a register or victim (x . r)
    (define (select m var)
      (define lv (call-live? var *live-map*))
      (define-values (m@ r) (pick-a-reg m lv))
      (define (select-victim m lv)
        (define (svic ls)
          (match ls
            ['() (error 'svic "can't find a proper victim.")]
            [`((,x . ,r) . ,d) #:when (memq r lv) `(,x . ,r)]
            [`((,x . ,r) . ,d) (svic d)]))
        (match m [(M rm fvm rs) (values m (svic (reverse rm)))]))
      (cond
        [(memq r R-ee) (addto! r) (values m@ r)]
        [r (values m@ r)]
        [else (select-victim m lv)]))
    
    ;; rewrite e with m
    (define (rwt e m)
      (define (find-reg x m) (lookup x (M-reg-map m)))
      (match e
        [(? symbol? x) (find-reg x m)]
        [`(,op . ,es)
         #:when (memq op `(,@op-prims vector-ref))
         `(,op . ,(rwt es m))]
        [`(,a . ,d) `(,(rwt a m) . ,(rwt d m))]
        [o o]))
    
    (define in-regs-var? (lambda (x m) (assv x (M-reg-map m))))
    (define in-fvs-var? (lambda (x m) (assv x (M-fv-map m))))
    (define get-vars (lambda (m) (map car (M-reg-map m))))
    
    (define (call-live? x live-map)
      (let ([pr (assv x live-map)])
        (cond
          [(not (unbox (cdr pr))) R-ee]
          [else R])))
    
    ;; pick a register if available
    (define (pick-a-reg m live?)
      (define rs (M-regs m))
      (define (pick ls)
        (match ls
          ['() (values ls #f)]
          [`(,a . ,d)
           (cond
             [(memq a live?) (values d a)]
             [else (define-values (l r) (pick d))
                   (values `(,a . ,l) r)])]))
      (define-values (rs@ r) (pick rs))
      (values (M (M-reg-map m) (M-fv-map m) rs@) r))

    
    ;; bind x with r in m
    (define (bind-a-reg x r m)
      (define (unbound r rm)
        (match rm
          ['() '()]
          [`(,a . ,d)
           (cond
             [(eqv? r (cdr a)) d]
             [else `(,a . ,(unbound r d))])]))
      (match m
        [(M rm fvm rs)
         (define rm@ (unbound r rm))
         (M `((,x . ,r) . ,rm@) fvm rs)]))
    ;; unbind x in reg-map
    (define (unbind-a-reg x m)
      (define (unbound x rm)
        (match rm
          ['() '()]
          [`(,a . ,d)
           (cond
             [(eqv? x (car a)) d]
             [else `(,a . ,(unbound x d))])]))
      (match m
        [(M rm fvm rs)
         (define rm@ (unbound x rm))
         (M rm@ fvm rs)]))
    
    ;; remove dead bindings
    (define (rm-d m die)
      (define (rm x m)  ;; reverse die?
        (match m
          [(M rm fvm rs)
           (let* ([pr-r (assv x rm)]
                  [pr-fv (assv x fvm)]
                  [rm@ (remove pr-r rm)]
                  [fvm@ (remove pr-fv fvm)])
             (cond
               [(not pr-r) (M rm@ fvm@ rs)]
               [else (define rs@ `(,(cdr pr-r) . ,rs))
                     (M rm@ fvm@ rs@)]))]))
      (match die
        ['() m]
        [`(,x . ,die*)
         (let ([m* (rm x m)]) (rm-d m* die*))]))
    
    (define offset/fv 0)
    (define (new-fvar s)
      (set! offset/fv (add1 offset/fv))
      (string->symbol
       (string-append
        (symbol->string s) "." (number->string offset/fv))))
    
    (define (get-space-x86 c)
      (let* ([s (* 8 (+ c 0))]
             [r (remainder s 16)])
        (+ s r)))
   

    (struct M (reg-map fv-map regs) #:transparent)
    (define R '(rcx rdx rsi rdi r8 r9 r10 rbx r12 r13 r14))
    (define R-er '(rcx rdx rsi rdi r8 r9 r10))
    (define R-ee '(rbx r12 r13 r14))
    (define *live-map* #f)
    (define callee-save-used '())
    (define conventions '(rdi rsi rdx rcx r8 r9))

    (define (addto! r)
      (cond
        [(memq r callee-save-used) 'nothing]
        [else (set! callee-save-used
                    (cons r callee-save-used))]))
    
    (define (get-initial-model args conventions)
      (define get-regs
        (lambda (args l)
          (match args
            ['() (values '() R)]
            [`(,a . ,d)
             (define-values (rm rs) (get-regs d (cdr l)))
             (define rm* `((,a . ,(car l)) . ,rm))
             (define rs* (remove (car l) rs))
             (values rm* rs*)])))
      (define-values (rm rs) (get-regs args conventions))
      (M rm '() rs))
    
    (match proc
      [(Def f args T info* exp)
       (define args* (map car args))
       (define init-m (get-initial-model args* conventions))
       (set! *live-map* (cdr (assv 'live-map info*)))
       (define-values (exp@ m@) (ts exp init-m 'tail))
       (let* ([stack-space (get-space-x86 offset/fv)]
              [root-spills 0]
              [params (length args)])
         (Def f args T `((frame-variables . ,offset/fv)
                         (num-root-spills . ,root-spills)
                         (num-params      . ,params)
                         (callee-saved    . ,callee-save-used))
              exp@))])))

;; alloc for the program
(define register-allocation
  (lambda (p)
    (match (uncover-live p)
      [(ProgramDefs info defs)
       (ProgramDefs info (map alloc defs))])))


(define (sumf fun)
  (pretty-print (alloc (uncover-live-proc fun))))