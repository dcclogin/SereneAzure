#include <time.h>
#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "interp0.h"

void *contr_init(void *jumpr__m__out) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _init_cont;
  _data->u._init._jumpr__m__out = jumpr__m__out;
  return (void *)_data;
}

void *contr_mult₁(void *x, void *env, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult₁_cont;
  _data->u._mult₁._x = x;
  _data->u._mult₁._env = env;
  _data->u._mult₁._k = k;
  return (void *)_data;
}

void *contr_mult₂(void *v, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult₂_cont;
  _data->u._mult₂._v = v;
  _data->u._mult₂._k = k;
  return (void *)_data;
}

void *contr_zero(void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_cont;
  _data->u._zero._k = k;
  return (void *)_data;
}

void *contr_subr1(void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_cont;
  _data->u._subr1._k = k;
  return (void *)_data;
}

void *contr_throw(void *v, void *env) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_cont;
  _data->u._throw._v = v;
  _data->u._throw._env = env;
  return (void *)_data;
}

void *contr_clos(void *rand, void *env, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _clos_cont;
  _data->u._clos._rand = rand;
  _data->u._clos._env = env;
  _data->u._clos._k = k;
  return (void *)_data;
}

void *contr_arg(void *c, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _arg_cont;
  _data->u._arg._c = c;
  _data->u._arg._k = k;
  return (void *)_data;
}

void *contr_if(void *conseq, void *alt, void *env, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_cont;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  _data->u._if._env = env;
  _data->u._if._k = k;
  return (void *)_data;
}

void *contr_let₁(void *env, void *body, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let₁_cont;
  _data->u._let₁._env = env;
  _data->u._let₁._body = body;
  _data->u._let₁._k = k;
  return (void *)_data;
}

void *contr_let₂(void *body, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let₂_cont;
  _data->u._let₂._body = body;
  _data->u._let₂._k = k;
  return (void *)_data;
}

void *contr_letcc(void *body, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_cont;
  _data->u._letcc._body = body;
  _data->u._letcc._k = k;
  return (void *)_data;
}

void *envr_mt() {
env* _data = (env*)malloc(sizeof(env));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mt_env;
  return (void *)_data;
}

void *envr_pr(void *v, void *cdrenv) {
env* _data = (env*)malloc(sizeof(env));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _pr_env;
  _data->u._pr._v = v;
  _data->u._pr._cdrenv = cdrenv;
  return (void *)_data;
}

void *closurer_clos(void *body, void *env) {
closure* _data = (closure*)malloc(sizeof(closure));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _clos_closure;
  _data->u._clos._body = body;
  _data->u._clos._env = env;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
valofr__m__exp = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_app(exprr_app(exprr_var((void *)0),exprr_var((void *)0)),exprr_const((void *)5)),exprr_app(exprr_app(exprr_var((void *)0),exprr_var((void *)0)),exprr_const((void *)5))));
valofr__m__env = (void *)envr_mt();
pc = (void *)valuer__m__ofr__m__cps;
mount_tram();
printf("The value is %d\n", (long)appr__m__kr__m__v₀);}

void applyr2r__m__k()
{
cont* _c = (cont*)appr2r__m__kr__m__k₀;
switch (_c->tag) {
case _init_cont: {
void *jumpr__m__out = _c->u._init._jumpr__m__out;
_trstr *trstr = (_trstr *)jumpr__m__out;
longjmp(*trstr->jmpbuf, 1);
break; }
case _mult₁_cont: {
void *x = _c->u._mult₁._x;
void *env = _c->u._mult₁._env;
void *k = _c->u._mult₁._k;
seqr2r__m__exp = (void *)x;
seqr2r__m__env = (void *)env;
seqr2r__m__k = (void *)contr_mult₂(appr2r__m__kr__m__v₀,k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _mult₂_cont: {
void *v = _c->u._mult₂._v;
void *k = _c->u._mult₂._k;
appr2r__m__kr__m__k₀ = (void *)k;
appr2r__m__kr__m__v₀ = (void *)(void *)((int)v * (int)appr2r__m__kr__m__v₀);
pcr2 = &applyr2r__m__k;
break; }
case _zero_cont: {
void *k = _c->u._zero._k;
appr2r__m__kr__m__k₀ = (void *)k;
appr2r__m__kr__m__v₀ = (void *)(appr2r__m__kr__m__v₀ == 0);
pcr2 = &applyr2r__m__k;
break; }
case _subr1_cont: {
void *k = _c->u._subr1._k;
appr2r__m__kr__m__k₀ = (void *)k;
appr2r__m__kr__m__v₀ = (void *)(void *)((int)appr2r__m__kr__m__v₀ - 1);
pcr2 = &applyr2r__m__k;
break; }
case _throw_cont: {
void *v = _c->u._throw._v;
void *env = _c->u._throw._env;
seqr2r__m__exp = (void *)v;
seqr2r__m__env = (void *)env;
seqr2r__m__k = (void *)appr2r__m__kr__m__v₀;
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _clos_cont: {
void *rand = _c->u._clos._rand;
void *env = _c->u._clos._env;
void *k = _c->u._clos._k;
seqr2r__m__exp = (void *)rand;
seqr2r__m__env = (void *)env;
seqr2r__m__k = (void *)contr_arg(appr2r__m__kr__m__v₀,k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _arg_cont: {
void *c = _c->u._arg._c;
void *k = _c->u._arg._k;
appr2r__m__closr__m__c = (void *)c;
appr2r__m__closr__m__a = (void *)appr2r__m__kr__m__v₀;
appr2r__m__closr__m__k = (void *)k;
pcr2 = &applyr2r__m__closure;
break; }
case _if_cont: {
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
void *env = _c->u._if._env;
void *k = _c->u._if._k;
if(appr2r__m__kr__m__v₀) {
  seqr2r__m__exp = (void *)conseq;
seqr2r__m__env = (void *)env;
seqr2r__m__k = (void *)k;
pcr2 = &valuer__m__ofr__m__seqr2;

} else {
  seqr2r__m__exp = (void *)alt;
seqr2r__m__env = (void *)env;
seqr2r__m__k = (void *)k;
pcr2 = &valuer__m__ofr__m__seqr2;

}
break; }
case _let₁_cont: {
void *env = _c->u._let₁._env;
void *body = _c->u._let₁._body;
void *k = _c->u._let₁._k;
extr2r__m__envr__m__v = (void *)appr2r__m__kr__m__v₀;
extr2r__m__envr__m__envr = (void *)env;
extr2r__m__envr__m__k = (void *)contr_let₂(body,k);
pcr2 = &extendr2r__m__env;
break; }
case _let₂_cont: {
void *body = _c->u._let₂._body;
void *k = _c->u._let₂._k;
seqr2r__m__exp = (void *)body;
seqr2r__m__env = (void *)appr2r__m__kr__m__v₀;
seqr2r__m__k = (void *)k;
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _letcc_cont: {
void *body = _c->u._letcc._body;
void *k = _c->u._letcc._k;
seqr2r__m__exp = (void *)body;
seqr2r__m__env = (void *)appr2r__m__kr__m__v₀;
seqr2r__m__k = (void *)k;
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
}
}

void applyr2r__m__env()
{
env* _c = (env*)appr2r__m__envr__m__envr;
switch (_c->tag) {
case _mt_env: {
fprintf(stderr, "unbound identifier");
 exit(1);
break; }
case _pr_env: {
void *v = _c->u._pr._v;
void *cdrenv = _c->u._pr._cdrenv;
if((appr2r__m__envr__m__y == 0)) {
  appr2r__m__kr__m__k₀ = (void *)appr2r__m__envr__m__k;
appr2r__m__kr__m__v₀ = (void *)v;
pcr2 = &applyr2r__m__k;

} else {
  appr2r__m__envr__m__envr = (void *)cdrenv;
appr2r__m__envr__m__y = (void *)(void *)((int)appr2r__m__envr__m__y - 1);
pcr2 = &applyr2r__m__env;

}
break; }
}
}

void extendr2r__m__env()
{
appr2r__m__kr__m__k₀ = (void *)extr2r__m__envr__m__k;
appr2r__m__kr__m__v₀ = (void *)envr_pr(extr2r__m__envr__m__v,extr2r__m__envr__m__envr);
pcr2 = &applyr2r__m__k;
}

void applyr2r__m__closure()
{
closure* _c = (closure*)appr2r__m__closr__m__c;
switch (_c->tag) {
case _clos_closure: {
void *body = _c->u._clos._body;
void *env = _c->u._clos._env;
extr2r__m__envr__m__v = (void *)appr2r__m__closr__m__a;
extr2r__m__envr__m__envr = (void *)env;
extr2r__m__envr__m__k = (void *)contr_let₂(body,appr2r__m__closr__m__k);
pcr2 = &extendr2r__m__env;
break; }
}
}

void maker2r__m__closure()
{
appr2r__m__kr__m__k₀ = (void *)mkr2r__m__closr__m__k;
appr2r__m__kr__m__v₀ = (void *)closurer_clos(mkr2r__m__closr__m__body,mkr2r__m__closr__m__env);
pcr2 = &applyr2r__m__k;
}

void valuer__m__ofr__m__seqr2()
{
expr* _c = (expr*)seqr2r__m__exp;
switch (_c->tag) {
case _const_expr: {
void *cexp = _c->u._const._cexp;
appr2r__m__kr__m__k₀ = (void *)seqr2r__m__k;
appr2r__m__kr__m__v₀ = (void *)cexp;
pcr2 = &applyr2r__m__k;
break; }
case _var_expr: {
void *n = _c->u._var._n;
appr2r__m__envr__m__envr = (void *)seqr2r__m__env;
appr2r__m__envr__m__y = (void *)n;
appr2r__m__envr__m__k = (void *)seqr2r__m__k;
pcr2 = &applyr2r__m__env;
break; }
case _mult_expr: {
void *nexpr1 = _c->u._mult._nexpr1;
void *nexpr2 = _c->u._mult._nexpr2;
seqr2r__m__exp = (void *)nexpr1;
seqr2r__m__k = (void *)contr_mult₁(nexpr2,seqr2r__m__env,seqr2r__m__k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _subr1_expr: {
void *nexp = _c->u._subr1._nexp;
seqr2r__m__exp = (void *)nexp;
seqr2r__m__k = (void *)contr_subr1(seqr2r__m__k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _zero_expr: {
void *nexp = _c->u._zero._nexp;
seqr2r__m__exp = (void *)nexp;
seqr2r__m__k = (void *)contr_zero(seqr2r__m__k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
seqr2r__m__exp = (void *)test;
seqr2r__m__k = (void *)contr_if(conseq,alt,seqr2r__m__env,seqr2r__m__k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
extr2r__m__envr__m__v = (void *)seqr2r__m__k;
extr2r__m__envr__m__envr = (void *)seqr2r__m__env;
extr2r__m__envr__m__k = (void *)contr_letcc(body,seqr2r__m__k);
pcr2 = &extendr2r__m__env;
break; }
case _throw_expr: {
void *kexp = _c->u._throw._kexp;
void *vexp = _c->u._throw._vexp;
seqr2r__m__exp = (void *)kexp;
seqr2r__m__k = (void *)contr_throw(vexp,seqr2r__m__env);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _let_expr: {
void *exp = _c->u._let._exp;
void *body = _c->u._let._body;
seqr2r__m__exp = (void *)exp;
seqr2r__m__k = (void *)contr_let₁(seqr2r__m__env,body,seqr2r__m__k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
mkr2r__m__closr__m__body = (void *)body;
mkr2r__m__closr__m__env = (void *)seqr2r__m__env;
mkr2r__m__closr__m__k = (void *)seqr2r__m__k;
pcr2 = &maker2r__m__closure;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
seqr2r__m__exp = (void *)rator;
seqr2r__m__k = (void *)contr_clos(rand,seqr2r__m__env,seqr2r__m__k);
pcr2 = &valuer__m__ofr__m__seqr2;
break; }
}
}

void applyr1r__m__k()
{
cont* _c = (cont*)appr1r__m__kr__m__k₀;
switch (_c->tag) {
case _init_cont: {
void *jumpr__m__out = _c->u._init._jumpr__m__out;
_trstr *trstr = (_trstr *)jumpr__m__out;
longjmp(*trstr->jmpbuf, 1);
break; }
case _mult₁_cont: {
void *x = _c->u._mult₁._x;
void *env = _c->u._mult₁._env;
void *k = _c->u._mult₁._k;
seqr1r__m__exp = (void *)x;
seqr1r__m__env = (void *)env;
seqr1r__m__k = (void *)contr_mult₂(appr1r__m__kr__m__v₀,k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _mult₂_cont: {
void *v = _c->u._mult₂._v;
void *k = _c->u._mult₂._k;
appr1r__m__kr__m__k₀ = (void *)k;
appr1r__m__kr__m__v₀ = (void *)(void *)((int)v * (int)appr1r__m__kr__m__v₀);
pcr1 = (void *)applyr1r__m__k;
break; }
case _zero_cont: {
void *k = _c->u._zero._k;
appr1r__m__kr__m__k₀ = (void *)k;
appr1r__m__kr__m__v₀ = (void *)(appr1r__m__kr__m__v₀ == 0);
pcr1 = (void *)applyr1r__m__k;
break; }
case _subr1_cont: {
void *k = _c->u._subr1._k;
appr1r__m__kr__m__k₀ = (void *)k;
appr1r__m__kr__m__v₀ = (void *)(void *)((int)appr1r__m__kr__m__v₀ - 1);
pcr1 = (void *)applyr1r__m__k;
break; }
case _throw_cont: {
void *v = _c->u._throw._v;
void *env = _c->u._throw._env;
seqr1r__m__exp = (void *)v;
seqr1r__m__env = (void *)env;
seqr1r__m__k = (void *)appr1r__m__kr__m__v₀;
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _clos_cont: {
void *rand = _c->u._clos._rand;
void *env = _c->u._clos._env;
void *k = _c->u._clos._k;
seqr1r__m__exp = (void *)rand;
seqr1r__m__env = (void *)env;
seqr1r__m__k = (void *)contr_arg(appr1r__m__kr__m__v₀,k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _arg_cont: {
void *c = _c->u._arg._c;
void *k = _c->u._arg._k;
appr1r__m__closr__m__c = (void *)c;
appr1r__m__closr__m__a = (void *)appr1r__m__kr__m__v₀;
appr1r__m__closr__m__k = (void *)k;
pcr1 = (void *)applyr1r__m__closure;
break; }
case _if_cont: {
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
void *env = _c->u._if._env;
void *k = _c->u._if._k;
if(appr1r__m__kr__m__v₀) {
  seqr1r__m__exp = (void *)conseq;
seqr1r__m__env = (void *)env;
seqr1r__m__k = (void *)k;
pcr1 = (void *)valuer__m__ofr__m__seqr1;

} else {
  seqr1r__m__exp = (void *)alt;
seqr1r__m__env = (void *)env;
seqr1r__m__k = (void *)k;
pcr1 = (void *)valuer__m__ofr__m__seqr1;

}
break; }
case _let₁_cont: {
void *env = _c->u._let₁._env;
void *body = _c->u._let₁._body;
void *k = _c->u._let₁._k;
extr1r__m__envr__m__v = (void *)appr1r__m__kr__m__v₀;
extr1r__m__envr__m__envr = (void *)env;
extr1r__m__envr__m__k = (void *)contr_let₂(body,k);
pcr1 = (void *)extendr1r__m__env;
break; }
case _let₂_cont: {
void *body = _c->u._let₂._body;
void *k = _c->u._let₂._k;
seqr1r__m__exp = (void *)body;
seqr1r__m__env = (void *)appr1r__m__kr__m__v₀;
seqr1r__m__k = (void *)k;
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _letcc_cont: {
void *body = _c->u._letcc._body;
void *k = _c->u._letcc._k;
seqr1r__m__exp = (void *)body;
seqr1r__m__env = (void *)appr1r__m__kr__m__v₀;
seqr1r__m__k = (void *)k;
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
}
}

void applyr1r__m__env()
{
env* _c = (env*)appr1r__m__envr__m__envr;
switch (_c->tag) {
case _mt_env: {
fprintf(stderr, "unbound identifier");
 exit(1);
break; }
case _pr_env: {
void *v = _c->u._pr._v;
void *cdrenv = _c->u._pr._cdrenv;
if((appr1r__m__envr__m__y == 0)) {
  appr1r__m__kr__m__k₀ = (void *)appr1r__m__envr__m__k;
appr1r__m__kr__m__v₀ = (void *)v;
pcr1 = (void *)applyr1r__m__k;

} else {
  appr1r__m__envr__m__envr = (void *)cdrenv;
appr1r__m__envr__m__y = (void *)(void *)((int)appr1r__m__envr__m__y - 1);
pcr1 = (void *)applyr1r__m__env;

}
break; }
}
}

void extendr1r__m__env()
{
appr1r__m__kr__m__k₀ = (void *)extr1r__m__envr__m__k;
appr1r__m__kr__m__v₀ = (void *)envr_pr(extr1r__m__envr__m__v,extr1r__m__envr__m__envr);
pcr1 = (void *)applyr1r__m__k;
}

void applyr1r__m__closure()
{
closure* _c = (closure*)appr1r__m__closr__m__c;
switch (_c->tag) {
case _clos_closure: {
void *body = _c->u._clos._body;
void *env = _c->u._clos._env;
extr1r__m__envr__m__v = (void *)appr1r__m__closr__m__a;
extr1r__m__envr__m__envr = (void *)env;
extr1r__m__envr__m__k = (void *)contr_let₂(body,appr1r__m__closr__m__k);
pcr1 = (void *)extendr1r__m__env;
break; }
}
}

void maker1r__m__closure()
{
appr1r__m__kr__m__k₀ = (void *)mkr1r__m__closr__m__k;
appr1r__m__kr__m__v₀ = (void *)closurer_clos(mkr1r__m__closr__m__body,mkr1r__m__closr__m__env);
pcr1 = (void *)applyr1r__m__k;
}

void valuer__m__ofr__m__seqr1()
{
expr* _c = (expr*)seqr1r__m__exp;
switch (_c->tag) {
case _const_expr: {
void *cexp = _c->u._const._cexp;
appr1r__m__kr__m__k₀ = (void *)seqr1r__m__k;
appr1r__m__kr__m__v₀ = (void *)cexp;
pcr1 = (void *)applyr1r__m__k;
break; }
case _var_expr: {
void *n = _c->u._var._n;
appr1r__m__envr__m__envr = (void *)seqr1r__m__env;
appr1r__m__envr__m__y = (void *)n;
appr1r__m__envr__m__k = (void *)seqr1r__m__k;
pcr1 = (void *)applyr1r__m__env;
break; }
case _mult_expr: {
void *nexpr1 = _c->u._mult._nexpr1;
void *nexpr2 = _c->u._mult._nexpr2;
seqr1r__m__exp = (void *)nexpr1;
seqr1r__m__k = (void *)contr_mult₁(nexpr2,seqr1r__m__env,seqr1r__m__k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _subr1_expr: {
void *nexp = _c->u._subr1._nexp;
seqr1r__m__exp = (void *)nexp;
seqr1r__m__k = (void *)contr_subr1(seqr1r__m__k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _zero_expr: {
void *nexp = _c->u._zero._nexp;
seqr1r__m__exp = (void *)nexp;
seqr1r__m__k = (void *)contr_zero(seqr1r__m__k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
seqr1r__m__exp = (void *)test;
seqr1r__m__k = (void *)contr_if(conseq,alt,seqr1r__m__env,seqr1r__m__k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
extr1r__m__envr__m__v = (void *)seqr1r__m__k;
extr1r__m__envr__m__envr = (void *)seqr1r__m__env;
extr1r__m__envr__m__k = (void *)contr_letcc(body,seqr1r__m__k);
pcr1 = (void *)extendr1r__m__env;
break; }
case _throw_expr: {
void *kexp = _c->u._throw._kexp;
void *vexp = _c->u._throw._vexp;
seqr1r__m__exp = (void *)kexp;
seqr1r__m__k = (void *)contr_throw(vexp,seqr1r__m__env);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _let_expr: {
void *exp = _c->u._let._exp;
void *body = _c->u._let._body;
seqr1r__m__exp = (void *)exp;
seqr1r__m__k = (void *)contr_let₁(seqr1r__m__env,body,seqr1r__m__k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
mkr1r__m__closr__m__body = (void *)body;
mkr1r__m__closr__m__env = (void *)seqr1r__m__env;
mkr1r__m__closr__m__k = (void *)seqr1r__m__k;
pcr1 = (void *)maker1r__m__closure;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
seqr1r__m__exp = (void *)rator;
seqr1r__m__k = (void *)contr_clos(rand,seqr1r__m__env,seqr1r__m__k);
pcr1 = (void *)valuer__m__ofr__m__seqr1;
break; }
}
}

void applyr__m__k()
{
cont* _c = (cont*)appr__m__kr__m__k₀;
switch (_c->tag) {
case _init_cont: {
void *jumpr__m__out = _c->u._init._jumpr__m__out;
_trstr *trstr = (_trstr *)jumpr__m__out;
longjmp(*trstr->jmpbuf, 1);
break; }
case _mult₁_cont: {
void *x = _c->u._mult₁._x;
void *env = _c->u._mult₁._env;
void *k = _c->u._mult₁._k;
valofr__m__exp = (void *)x;
valofr__m__env = (void *)env;
valofr__m__k = (void *)contr_mult₂(appr__m__kr__m__v₀,k);
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _mult₂_cont: {
void *v = _c->u._mult₂._v;
void *k = _c->u._mult₂._k;
appr__m__kr__m__k₀ = (void *)k;
appr__m__kr__m__v₀ = (void *)(void *)((int)v * (int)appr__m__kr__m__v₀);
pc = (void *)applyr__m__k;
break; }
case _zero_cont: {
void *k = _c->u._zero._k;
appr__m__kr__m__k₀ = (void *)k;
appr__m__kr__m__v₀ = (void *)(appr__m__kr__m__v₀ == 0);
pc = (void *)applyr__m__k;
break; }
case _subr1_cont: {
void *k = _c->u._subr1._k;
appr__m__kr__m__k₀ = (void *)k;
appr__m__kr__m__v₀ = (void *)(void *)((int)appr__m__kr__m__v₀ - 1);
pc = (void *)applyr__m__k;
break; }
case _throw_cont: {
void *v = _c->u._throw._v;
void *env = _c->u._throw._env;
valofr__m__exp = (void *)v;
valofr__m__env = (void *)env;
valofr__m__k = (void *)appr__m__kr__m__v₀;
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _clos_cont: {
void *rand = _c->u._clos._rand;
void *env = _c->u._clos._env;
void *k = _c->u._clos._k;
valofr__m__exp = (void *)rand;
valofr__m__env = (void *)env;
valofr__m__k = (void *)contr_arg(appr__m__kr__m__v₀,k);
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _arg_cont: {
void *c = _c->u._arg._c;
void *k = _c->u._arg._k;
appr__m__closr__m__c = (void *)c;
appr__m__closr__m__a = (void *)appr__m__kr__m__v₀;
appr__m__closr__m__k = (void *)k;
pc = (void *)applyr__m__closure;
break; }
case _if_cont: {
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
void *env = _c->u._if._env;
void *k = _c->u._if._k;
if(appr__m__kr__m__v₀) {
  valofr__m__exp = (void *)conseq;
valofr__m__env = (void *)env;
valofr__m__k = (void *)k;
pc = (void *)valuer__m__ofr__m__cps;

} else {
  valofr__m__exp = (void *)alt;
valofr__m__env = (void *)env;
valofr__m__k = (void *)k;
pc = (void *)valuer__m__ofr__m__cps;

}
break; }
case _let₁_cont: {
void *env = _c->u._let₁._env;
void *body = _c->u._let₁._body;
void *k = _c->u._let₁._k;
extr__m__envr__m__v = (void *)appr__m__kr__m__v₀;
extr__m__envr__m__envr = (void *)env;
extr__m__envr__m__k = (void *)contr_let₂(body,k);
pc = (void *)extendr__m__env;
break; }
case _let₂_cont: {
void *body = _c->u._let₂._body;
void *k = _c->u._let₂._k;
valofr__m__exp = (void *)body;
valofr__m__env = (void *)appr__m__kr__m__v₀;
valofr__m__k = (void *)k;
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _letcc_cont: {
void *body = _c->u._letcc._body;
void *k = _c->u._letcc._k;
valofr__m__exp = (void *)body;
valofr__m__env = (void *)appr__m__kr__m__v₀;
valofr__m__k = (void *)k;
pc = (void *)valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
env* _c = (env*)appr__m__envr__m__envr;
switch (_c->tag) {
case _mt_env: {
fprintf(stderr, "unbound identifier");
 exit(1);
break; }
case _pr_env: {
void *v = _c->u._pr._v;
void *cdrenv = _c->u._pr._cdrenv;
if((appr__m__envr__m__y == 0)) {
  appr__m__kr__m__k₀ = (void *)appr__m__envr__m__k;
appr__m__kr__m__v₀ = (void *)v;
pc = (void *)applyr__m__k;

} else {
  appr__m__envr__m__envr = (void *)cdrenv;
appr__m__envr__m__y = (void *)(void *)((int)appr__m__envr__m__y - 1);
pc = (void *)applyr__m__env;

}
break; }
}
}

void extendr__m__env()
{
appr__m__kr__m__k₀ = (void *)extr__m__envr__m__k;
appr__m__kr__m__v₀ = (void *)envr_pr(extr__m__envr__m__v,extr__m__envr__m__envr);
pc = (void *)applyr__m__k;
}

void applyr__m__closure()
{
closure* _c = (closure*)appr__m__closr__m__c;
switch (_c->tag) {
case _clos_closure: {
void *body = _c->u._clos._body;
void *env = _c->u._clos._env;
extr__m__envr__m__v = (void *)appr__m__closr__m__a;
extr__m__envr__m__envr = (void *)env;
extr__m__envr__m__k = (void *)contr_let₂(body,appr__m__closr__m__k);
pc = (void *)extendr__m__env;
break; }
}
}

void maker__m__closure()
{
appr__m__kr__m__k₀ = (void *)mkr__m__closr__m__k;
appr__m__kr__m__v₀ = (void *)closurer_clos(mkr__m__closr__m__body,mkr__m__closr__m__env);
pc = (void *)applyr__m__k;
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)valofr__m__exp;
switch (_c->tag) {
case _const_expr: {
void *cexp = _c->u._const._cexp;
appr__m__kr__m__k₀ = (void *)valofr__m__k;
appr__m__kr__m__v₀ = (void *)cexp;
pc = (void *)applyr__m__k;
break; }
case _var_expr: {
void *n = _c->u._var._n;
appr__m__envr__m__envr = (void *)valofr__m__env;
appr__m__envr__m__y = (void *)n;
appr__m__envr__m__k = (void *)valofr__m__k;
pc = (void *)applyr__m__env;
break; }
case _mult_expr: {
void *nexpr1 = _c->u._mult._nexpr1;
void *nexpr2 = _c->u._mult._nexpr2;
seqr1r__m__exp = (void *)nexpr1;
seqr2r__m__exp = (void *)nexpr2;
seqr1r__m__env = (void *)valofr__m__env;
seqr2r__m__env = (void *)valofr__m__env;
pcr1 = &valuer__m__ofr__m__seqr1;
pcr2 = &valuer__m__ofr__m__seqr2;
mount_tram_1();
mount_tram_2();
appr__m__kr__m__k₀ = (void *)valofr__m__k;
appr__m__kr__m__v₀ = (void *)(void *)((int)appr1r__m__kr__m__v₀ * (int)appr2r__m__kr__m__v₀);
pc = (void *)applyr__m__k;
break; }
case _subr1_expr: {
void *nexp = _c->u._subr1._nexp;
valofr__m__exp = (void *)nexp;
valofr__m__k = (void *)contr_subr1(valofr__m__k);
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *nexp = _c->u._zero._nexp;
valofr__m__exp = (void *)nexp;
valofr__m__k = (void *)contr_zero(valofr__m__k);
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
valofr__m__exp = (void *)test;
valofr__m__k = (void *)contr_if(conseq,alt,valofr__m__env,valofr__m__k);
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
extr__m__envr__m__v = (void *)valofr__m__k;
extr__m__envr__m__envr = (void *)valofr__m__env;
extr__m__envr__m__k = (void *)contr_letcc(body,valofr__m__k);
pc = (void *)extendr__m__env;
break; }
case _throw_expr: {
void *kexp = _c->u._throw._kexp;
void *vexp = _c->u._throw._vexp;
valofr__m__exp = (void *)kexp;
valofr__m__k = (void *)contr_throw(vexp,valofr__m__env);
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *exp = _c->u._let._exp;
void *body = _c->u._let._body;
valofr__m__exp = (void *)exp;
valofr__m__k = (void *)contr_let₁(valofr__m__env,body,valofr__m__k);
pc = (void *)valuer__m__ofr__m__cps;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
mkr__m__closr__m__body = (void *)body;
mkr__m__closr__m__env = (void *)valofr__m__env;
mkr__m__closr__m__k = (void *)valofr__m__k;
pc = (void *)maker__m__closure;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
valofr__m__exp = (void *)rator;
valofr__m__k = (void *)contr_clos(rand,valofr__m__env,valofr__m__k);
pc = (void *)valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
valofr__m__k= (void *)contr_init(dismount);
for(;;) {
pc();
}
}
return 0;
}

int mount_tram_1 ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
seqr1r__m__k= (void *)contr_init(dismount);
for(;;) {
pcr1();
}
}
return 0;
}

int mount_tram_2 ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
seqr2r__m__k= (void *)contr_init(dismount);
for(;;) {
pcr2();
}
}
return 0;
}
