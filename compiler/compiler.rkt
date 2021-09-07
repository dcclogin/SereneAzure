#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require racket/trace)
(require "interp-Rint.rkt")
(require "interp-Rvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

; redefine the gensym function in Racket
(define gensym
  (let ([n -1])
    (lambda (s)
      (set! n (add1 n))
      (string->symbol
       (string-append (symbol->string s) "." (number->string n))))))


;; parser
(define parse
  (λ (exp)
    (letrec ([Z (λ (x) (Program '() x))]
             [f (λ (exp C)
                  (match exp
                    [(? symbol? x) (C (Var x))]
                    [(? fixnum? x) (C (Int x))]
                    [`(let ([,x ,e]) ,b)
                     (f e
                        (λ (e@)
                          (f b
                             (λ (b@)
                               (C (Let x e@ b@))))))]
                    [`(read) (C (Prim 'read '()))]
                    [`(- ,e)
                     (f e
                        (λ (e@)
                          (C (Prim '- `(,e@)))))]
                    [`(+ ,e1 ,e2)
                     (f e1
                        (λ (e@1)
                          (f e2
                             (λ (e@2)
                               (C (Prim '+ `(,e@1 ,e@2)))))))]))])
      (f exp Z))))

;; unparser
(define unparse
  (λ (exp)
    (match exp
      [(Var x) x]
      [(Int x) x]
      [(Let x e b) `(let ([,x ,(unparse e)])
                      ,(unparse b))]
      [(Prim op es) `(,op ,@(map unparse es))]
      [(Program _ e) (unparse e)])))


;; partial evaluation
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [(Prim '+ `(,(Int n1) ,e2))
     (Prim '+ `(,(Int (fx- 0 n1)) ,(pe-exp (Prim '- `(,e2)))))]
    [(Prim '+ `(,e1 ,e2))
     (let ([e@1 (pe-exp (Prim '- `(,e1)))]
           [e@2 (pe-exp (Prim '- `(,e2)))])
       (Prim '+ `(,e@1 ,e@2)))]
    [(Prim '- `(,e)) e]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2))
     (Int (fx+ n1 n2))]
    [((Int n1) (Prim '+ `(,(Int n2) ,e2)))
     (let ([n@ (Int (fx+ n1 n2))])
       (Prim '+ `(,n@ ,e2)))]
    [((Prim '+ `(,(Int n1) ,e1)) (Int n2))
     (let ([n@ (Int (fx+ n1 n2))])
       (Prim '+ `(,n@ ,e1)))]
    [((Prim '+ `(,(Int n1) ,e1))
      (Prim '+ `(,(Int n2) ,e2)))
     (let ([n@ (Int (fx+ n1 n2))]
           [e@ (Prim '+ `(,e1 ,e2))])
       (Prim '+ `(,n@ ,e@)))]
    [(e1 (Prim '+ `(,(Int n2) ,e2)))
     (pe-exp
      (Prim '+ `(,(Int n2) ,(pe-exp (Prim '+ `(,e1 ,e2))))))]
    [((Prim '+ `(,(Int n1) ,e1)) e2)
     (pe-exp
      (Prim '+ `(,(Int n1) ,(pe-exp (Prim '+ `(,e1 ,e2))))))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Prim 'read '())
     (Prim 'read '())]
    [(Prim '- (list e1))
     (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2))
     (let ([e@1 (pe-exp e1)]
           [e@2 (pe-exp e2)])
       (match e@2
         [(Int n) (pe-add e@2 e@1)]
         [_ (pe-add e@1 e@2)]))]
    [(Let x e@ b)
     (Let x (pe-exp e@) (pe-exp b))]))

(define (pe p)
  (match p
    [(Program info e)
     (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silly
(define silly
  (λ (exp)
    (match exp
      [(Let x e b)
       (Let x (silly e) (silly b))]
      [(Prim '+ `(,e1 ,e2))
       (let ([e@1 (silly e1)]
             [e@2 (silly e2)])
         (Prim '+ `(,e@1 ,e@2)))]
      [(Prim '- `(,e))
       (Prim '+ `(,e ,(Int 1)))]
      [(Prim '(read) '())
       (Prim '+ `(,exp ,(Int 2)))]
      [(Program info e)
       (Program info (silly e))]
      [e (Prim '+ `(,e ,(Int 3)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (letrec ([mt-env '()]
           [ext-env (λ (x v env)
                      `((,x . ,v) . ,env))]
           [u (λ (env)
                (λ (exp)
                  (match exp
                    [(Var x)
                     (Var (lookup x env))]
                    [(Int n) (Int n)]
                    [(Let x e b)
                     (let* ([x@ (gensym x)]
                            [env@ (ext-env x x@ env)])
                       (Let x@
                            ((u env) e)
                            ((u env@) b)))]
                    [(Prim op es)
                     (Prim op (map (u env) es))])))])
    (match p
      [(Program info e)
       (Program info ((u mt-env) e))])))

;; remove-complex-opera* : R1 -> R1
(define remove-complex-opera*
  (λ (exp)
    (letrec ([id (λ (x) x)]
             [anf
              (λ (exp ctx C)    ;; context for current call
                (match exp
                  [(Let x e b)
                   (anf e 'lhs
                        (λ (v)
                          (Let x v (anf b ctx C))))]
                  [(Prim op es)
                   (anf es 'args
                        (λ (es@)
                          (cond
                            [(or (eqv? ctx 'id)
                                 (eqv? ctx 'lhs))
                             (C (Prim op es@))]
                            [else (let ([v@ (gensym 'tmp)])
                                    (Let v@ (Prim op es@)
                                         (C (Var v@))))])))]
                  [`(,a ,as ...)
                   (anf a 'arg
                        (λ (v)
                          (anf as 'args
                               (λ (vs)
                                 (C `(,v ,@vs))))))]
                  [(Program info e)
                   (Program info (anf e 'id C))]
                  [e (C e)]))])
      (anf exp 'id id))))

;; type checker

;; explicate-control : R1 -> C0
(define explicate-control
  (λ (exp)
    (letrec ([vars '()]
             [id (λ (x) x)]
             [r->c
              (λ (exp C)
                (match exp
                  [(Let x e b)
                   (r->c e
                         (λ (e@)
                           (let* ([x@ (Var x)]
                                  [stmt (Assign x@ e@)])
                             (set! vars (cons x vars))
                             (Seq stmt (r->c b C)))))]
                  ;; assume no unbound variables
                  [(or (? Int? x)
                       (? Var? x)
                       (? Prim? x))
                   (if (eqv? C id)
                       (C (Return exp))
                       (C exp))]
                  [(Program info e) (r->c e C)]))]
             [walk-vars
              (λ (ls)
                (match ls
                  ['() '()]
                  [`(,x . ,a)
                   `(,(Var x) . ,(walk-vars a))]))])
      (let* ([tail (r->c exp id)]
             [vars@ (walk-vars vars)]
             [info `(locals . ,vars@)])
        (CProgram info `((start . ,tail)))))))

;; select-instructions : C0 -> pseudo-x86
(define select-instructions
  (λ (p)
    (letrec ([t (λ (e)
                  (match e
                    [(Int x) (Imm x)]
                    [(Var x) (Reg x)]))]
             [select-cfg
              (λ (cfg)
                (match cfg
                  ['() '()]
                  [`((,label . ,tail) . ,d)
                   `((,label . ,(Block '() (select tail)))
                     . ,(select-cfg d))]))]
             [select
              (λ (tail)
                (match tail
                  [(Return e)
                   (match e
                     [(or (? Int?) (? Var?))
                      (let ([i (Instr 'movq `(,(t e) ,(Reg 'rax)))])
                        `(,i))]
                     [(Prim '+ `(,arg1 ,arg2))
                      (let ([i1 (Instr 'movq `(,(t arg1) ,(Reg 'rax)))]
                            [i2 (Instr 'addq `(,(t arg2) ,(Reg 'rax)))])
                        `(,i1 ,i2))]
                     [(Prim '- `(,arg))
                      (let ([i1 (Instr 'movq `(,(t arg) ,(Reg 'rax)))]
                            [i2 (Instr 'negq `(,(Reg 'rax)))])
                        `(,i1 ,i2))]                     
                     [(Prim 'read '())
                      (let ([i (Callq 'read_int 0)])
                        `(,i))])]
                  [(Seq stmt tl)
                   (match stmt
                     [(Assign (Var x) e)
                      (match e
                        [(or (? Int?) (? Var?))
                         (let ([i (Instr 'movq `(,(t e) ,(Reg x)))])
                           `(,i ,@(select tl)))]
                        [(Prim 'read '())
                         (let ([i1 (Callq 'read_int 0)]
                               [i2 (Instr 'movq `(,(Reg 'rax) ,(Reg x)))])
                           `(,i1 ,i2 ,@(select tl)))]
                        ;; (- a)
                        [(Prim '- `(,a))
                         (match a
                           [(Var v)
                            #:when (eqv? v x)
                            (let ([i (Instr 'negq `(,(Reg x)))])
                              `(,i ,@(select tl)))]
                           [a (let ([i1 (Instr 'movq `(,(t a) ,(Reg x)))]
                                    [i2 (Instr 'negq `(,(Reg x)))])
                                `(,i1 ,i2 ,@(select tl)))])]
                        ;; (+ a1 a2)
                        [(Prim '+ `(,a1 ,a2))
                         (match* (a1 a2)
                           [((Var v1) a2)
                            #:when (eqv? v1 x)
                            (let ([i (Instr 'addq `(,(t a2) ,(Reg x)))])
                              `(,i ,@(select tl)))]
                           [(a1 (Var v2))
                            #:when (eqv? v2 x)
                            (let ([i (Instr 'addq `(,(t a1) ,(Reg x)))])
                              `(,i ,@(select tl)))]
                           [(a1 a2)
                            (let ([i1 (Instr 'movq `(,(t a1) ,(Reg x)))]
                                  [i2 (Instr 'addq `(,(t a2) ,(Reg x)))])
                              `(,i1 ,i2 ,@(select tl)))])])])]))]
             [walk-info
              (λ (info)
                (match info
                  [`(,tag . ,vars)
                   `(,tag . ,(walk vars))]))]
             [walk
              (λ (vars)
                (match vars
                  ['() '()]
                  [`(,a . ,d)
                   `(,(t a) . ,(walk d))]))])
      (match p
        [(CProgram info cfg)
         (let ([info@ (walk-info info)]
               [cfg@ (select-cfg cfg)])
           (X86Program info@ cfg@))]))))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define assign-homes
  (λ (p)
    (letrec ([count 1]
             ;; associate variables with a number ((x . -8) (y . -16) ...)
             [build-homes (λ (ls)
                            (match ls
                              ['() '()]
                              [`(,(Reg a) . ,d)
                               `((,a . ,(* count -8))
                                 . ,(begin
                                      (set! count (add1 count))
                                      (build-homes d)))]))]
             ;; assign for the control flow graph
             [assign-cfg (λ (cfg env)
                           (match cfg
                             ['() '()]
                             [`((,label . ,tail) . ,d)
                              (match tail
                                [(Block info ls)
                                 (let* ([ls@ (assign-ls ls env)]
                                        [tl@ (Block info ls@)])
                                   `((,label . ,tl@)
                                     . ,(assign-cfg d env)))])]))]
             ;; assign for a list of instructions
             [assign-ls (λ (ls env)
                          (match ls
                            ['() '()]
                            [`(,a . ,d)
                             `(,(assign-instr a env)
                               . ,(assign-ls d env))]))]
             ;; assign for a single insruction
             [assign-instr (λ (instr env)
                             (match instr
                               [(Instr i args)
                                (Instr i (assign-args args env))]
                               ;; any other instructions here ...
                               [(Callq arg1 arg2) instr]))]
             ;; (Instr _ args)
             [assign-args (λ (args env)
                            (match args
                              ['() '()]
                              [`(,a . ,d)
                               `(,(assign a env)
                                 . ,(assign-args d env))]))]
             [assign (λ (r env)
                       (match r
                         [(Imm n) (Imm n)]
                         [(Reg 'rax) r]    ;; rax
                         [(Reg a)
                          (let ([n (cdr (assv a env))])
                            (Deref 'rbp n))]))]
             [types (λ (x) x)]
             [walk-vars (λ (vars env)
                          (match vars
                            ['() '()]
                            [`(,a . ,d)
                             `(,(assign a env)
                               . ,(walk-vars d env))]))]
             ;; the x86-64 standard requires the frame size to be a multiple of 16 bytes.
             [get-space-x86
              (λ (c)
                (let* ([s (* 8 c)]
                       [r (remainder s 16)])
                  (+ s r)))])
      (match p
        [(X86Program info cfg)
         (match info
           [`(,tag . ,vars)
            (let* ([env (build-homes vars)]
                   [vars@ (walk-vars vars env)]
                   [space (get-space-x86 (sub1 count))]
                   [info@1 `(,tag . ,vars@)]
                   [info@2 `(stack-space . ,space)]
                   [cfg@ (assign-cfg cfg env)])
              (X86Program `(,info@1 ,info@2) cfg@))])]))))

;; patch-instructions : psuedo-x86 -> x86
(define patch-instructions
  (λ (p)
    (letrec ([patch-cfg
              (λ (cfg)
                (match cfg
                  ['() '()]
                  [`((,label . ,tail) . ,d)
                   (match tail
                     [(Block info ls)
                      (let* ([ls@ (patch ls)]
                             [tl@ (Block info ls@)])
                        `((,label . ,tl@)
                          . ,(patch-cfg d)))])]))]
             [patch
              (λ (ls)
                (match ls
                  ['() '()]
                  [`(,a . ,d)
                   (match a
                     [(Instr 'movq `(,arg1 ,arg2))
                      (cond
                        [(and (Deref? arg1) (Deref? arg2))
                         (let ([i@1 (Instr 'movq `(,arg1 ,(Reg 'rax)))]
                               [i@2 (Instr 'movq `(,(Reg 'rax) ,arg2))])
                           `(,i@1 ,i@2 . ,(patch d)))]
                        [else `(,a . ,(patch d))])]
                     [(Instr 'addq `(,arg1 ,arg2))
                      (cond
                        [(and (Deref? arg1) (Deref? arg2))
                         (let ([i@1 (Instr 'movq `(,arg1 ,(Reg 'rax)))]
                               [i@2 (Instr 'addq `(,(Reg 'rax) ,arg2))])
                           `(,i@1 ,i@2 . ,(patch d)))]
                        [else `(,a . ,(patch d))])]
                     ;; any other cases?
                     [else `(,a . ,(patch d))])]))])
      (match p
        [(X86Program info cfg)
         (let ([cfg@ (patch-cfg cfg)])
           (X86Program info cfg@))]))))

;; prelude-and-conclusion : x86 -> x86
(define prelude-and-conclusion
  (λ (p)
    (letrec ([build-main
              (λ (space)
                (let* ([i1 (Instr 'pushq `(,(Reg 'rbp)))]
                       [i2 (Instr 'movq `(,(Reg 'rsp) ,(Reg 'rbp)))]
                       [i3 (Instr 'subq `(,(Imm space) ,(Reg 'rsp)))]
                       [i4 (Jmp 'start)]
                       [b (Block '() `(,i1 ,i2 ,i3 ,i4))])
                  `(main . ,b)))]
             [build-conc
              (λ (space)
                (let* ([i1 (Instr 'addq `(,(Imm space) ,(Reg 'rsp)))]
                       [i2 (Instr 'popq `(,(Reg 'rbp)))]
                       [i3 (Retq)]
                       [b (Block '() `(,i1 ,i2 ,i3))])
                  `(conclusion . ,b)))]
             ;; add the last jump-to-conclusion instruction
             ;; may do this job at other passes...
             [last-jmp
              (λ (cfg)
                (match cfg
                  ['() '()]
                  [`((,label . ,tail) . ,d)
                   (match tail
                     [(Block info ls)
                      (cond
                        [(not (Jmp? (last ls)))
                         (let* ([jmp-c (Jmp 'conclusion)]
                                [ls@ `(,@ls ,jmp-c)]
                                [tl@ (Block info ls@)])
                           `((,label . ,tl@)
                             . ,(last-jmp d)))]
                        [else
                         `((,label . ,tail)
                           . ,(last-jmp d))])])]))])
      (match p
        [(X86Program info cfg*)
         (let* ([pr (assq 'stack-space info)]
                [space (cdr pr)]
                [main (build-main space)]
                [conc (build-conc space)]
                [cfg (last-jmp cfg*)]
                [cfg@ `(,main ,@cfg ,conc)])
           (X86Program info cfg@))]))))


;; instant summary (without pe)
(define summary
  (λ (exp)
    (prelude-and-conclusion
     (patch-instructions
      (assign-homes
       (select-instructions
        (explicate-control
         (remove-complex-opera*
          (uniquify
           (parse exp))))))))))

;; instant summary (with pe)
(define summary-pe
  (λ (exp)
    (prelude-and-conclusion
     (patch-instructions
      (assign-homes
       (select-instructions
        (explicate-control
         (remove-complex-opera*
          (pe
           (uniquify
            (parse exp)))))))))))

;; summary up to the rco pass
(define rco
  (λ (exp)
    (unparse
     (remove-complex-opera*
      (pe
       (uniquify
        (parse exp)))))))

;; partial evaluator test
(define pe-test
  (λ (exp)
    (unparse (pe (parse exp)))))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(;;("silly" ,silly ,interp-Rvar)
    ("uniquify" ,uniquify ,interp-Rvar)
    ("partial evaluation" ,pe ,interp-Rvar)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Rvar)
    ("explicate control" ,explicate-control ,interp-Cvar)
    ("instruction selection" ,select-instructions ,interp-x86-0)
    ("assign homes" ,assign-homes ,interp-x86-0)
    ("patch instructions" ,patch-instructions ,interp-x86-0)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
    ))

