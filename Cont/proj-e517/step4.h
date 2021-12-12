void (*pc)();

void (*pcr1)();

void (*pcr2)();

void *valofr__m__exp, *valofr__m__env, *valofr__m__k, *appr__m__kr__m__k₀, *appr__m__kr__m__v₀, *mkr__m__closr__m__body, *mkr__m__closr__m__env, *mkr__m__closr__m__k, *appr__m__closr__m__c, *appr__m__closr__m__a, *appr__m__closr__m__k, *extr__m__envr__m__v, *extr__m__envr__m__envr, *extr__m__envr__m__k, *appr__m__envr__m__envr, *appr__m__envr__m__y, *appr__m__envr__m__k, *seqr1r__m__exp, *seqr1r__m__env, *seqr1r__m__k, *appr1r__m__kr__m__k₀, *appr1r__m__kr__m__v₀, *mkr1r__m__closr__m__body, *mkr1r__m__closr__m__env, *mkr1r__m__closr__m__k, *appr1r__m__closr__m__c, *appr1r__m__closr__m__a, *appr1r__m__closr__m__k, *extr1r__m__envr__m__v, *extr1r__m__envr__m__envr, *extr1r__m__envr__m__k, *appr1r__m__envr__m__envr, *appr1r__m__envr__m__y, *appr1r__m__envr__m__k, *seqr2r__m__exp, *seqr2r__m__env, *seqr2r__m__k, *appr2r__m__kr__m__k₀, *appr2r__m__kr__m__v₀, *mkr2r__m__closr__m__body, *mkr2r__m__closr__m__env, *mkr2r__m__closr__m__k, *appr2r__m__closr__m__c, *appr2r__m__closr__m__a, *appr2r__m__closr__m__k, *extr2r__m__envr__m__v, *extr2r__m__envr__m__envr, *extr2r__m__envr__m__k, *appr2r__m__envr__m__envr, *appr2r__m__envr__m__y, *appr2r__m__envr__m__k;

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

struct closure;
typedef struct closure closure;
struct closure {
  enum {
    _clos_closure
  } tag;
  union {
    struct { void *_body; void *_env; } _clos;
  } u;
};

void *closurer_clos(void *body, void *env);

struct env;
typedef struct env env;
struct env {
  enum {
    _mt_env,
    _pr_env
  } tag;
  union {
    struct { char dummy; } _mt;
    struct { void *_v; void *_cdrenv; } _pr;
  } u;
};

void *envr_mt();
void *envr_pr(void *v, void *cdrenv);

struct cont;
typedef struct cont cont;
struct cont {
  enum {
    _init_cont,
    _mult₁_cont,
    _mult₂_cont,
    _zero_cont,
    _subr1_cont,
    _throw_cont,
    _clos_cont,
    _arg_cont,
    _if_cont,
    _let₁_cont,
    _let₂_cont,
    _letcc_cont
  } tag;
  union {
    struct { void *_jumpr__m__out; } _init;
    struct { void *_x; void *_env; void *_k; } _mult₁;
    struct { void *_v; void *_k; } _mult₂;
    struct { void *_k; } _zero;
    struct { void *_k; } _subr1;
    struct { void *_v; void *_env; } _throw;
    struct { void *_rand; void *_env; void *_k; } _clos;
    struct { void *_c; void *_k; } _arg;
    struct { void *_conseq; void *_alt; void *_env; void *_k; } _if;
    struct { void *_env; void *_body; void *_k; } _let₁;
    struct { void *_body; void *_k; } _let₂;
    struct { void *_body; void *_k; } _letcc;
  } u;
};

void *contr_init(void *jumpr__m__out);
void *contr_mult₁(void *x, void *env, void *k);
void *contr_mult₂(void *v, void *k);
void *contr_zero(void *k);
void *contr_subr1(void *k);
void *contr_throw(void *v, void *env);
void *contr_clos(void *rand, void *env, void *k);
void *contr_arg(void *c, void *k);
void *contr_if(void *conseq, void *alt, void *env, void *k);
void *contr_let₁(void *env, void *body, void *k);
void *contr_let₂(void *body, void *k);
void *contr_letcc(void *body, void *k);

void valuer__m__ofr__m__cps();
void maker__m__closure();
void applyr__m__closure();
void extendr__m__env();
void applyr__m__env();
void applyr__m__k();
void valuer__m__ofr__m__seqr1();
void maker1r__m__closure();
void applyr1r__m__closure();
void extendr1r__m__env();
void applyr1r__m__env();
void applyr1r__m__k();
void valuer__m__ofr__m__seqr2();
void maker2r__m__closure();
void applyr2r__m__closure();
void extendr2r__m__env();
void applyr2r__m__env();
void applyr2r__m__k();
int main();
int mount_tram();
int mount_tram_1();
int mount_tram_2();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

