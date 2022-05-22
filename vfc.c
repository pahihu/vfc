/* C.H.Ting: Virtual Forth Computer
 *
 * Dictionary header:
 *  |...name...| NFA | LFA | CFA | PFA ...
 *  ^
 * NFA
 *
 * NFA points to the beginning of the zero terminated name.
 * CTX is a pointer to a dictionary link cell, which contains
 * the last NFA. <name> is extended to cell boundary.
 *
 *
 * History
 * =======
 * 220309AP buffered output
 * 211122AP simplified header struct (NFA is 0 terminated string)
 *          native addresses, reworked ABORT mechanism
 *          added ZCOUNT @+ !+
 *          soft: added LIBRARY FUNCTION: simplified ABORT" S"
 * 211121AP back to 2-word CREATE, simplified fo_vm, cell counted strings,
 *          added (DLOPEN) (DLSYM) (CB) SP@ BLOCK SAVE
 * 211114AP c_abort() flushes input line
 * 211113AP added ['] LITERAL [ ] POSTPONE NIP PLACE APPEND C" <C">
 * 211112AP removed U. U.R 0< 0>
 *          added ( ' SPACE SPACES INCLUDE ." .( <.">
 *                FILL 1+ 1- 2* 2/ CELL+ CELL- CELLS CELL/
 *                TYPE COUNT -TRAILING ROT
 *          renamed WARM to ABORT
 *          fixed dict header alignment
 * 211111AP removed DO LOOP +LOOP,
 *          added AFT,
 *          changed FOR NEXT to eForth semantics
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>
#include <time.h>
#include <unistd.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#ifdef darwin
#define stricmp   strcasecmp
#endif

typedef void (*prime_t)(void);
typedef long Cell;
typedef unsigned long UCell;
#ifdef __LP64__
typedef unsigned __int128 DCell;
#else
typedef unsigned long long DCell;
#endif
typedef unsigned char Byte;

#define BYTE(x)      ((Byte*)(x))
#define CHAR(x)      ((char*)(x))
#define CELL(x)      ((Cell)(x))
#define PCELL(x)     ((Cell*)(x))

#define DSTACK_SIZE	(1024)
#define RSTACK_SIZE	(2*1024)

#define PRIME_SIZE	(sizeof(prime_t))
#define CELL_SIZE	   (sizeof(Cell))

#define BL	' '

Cell *M = 0;         /* memory */
Cell *P,*W;			   /* indirect-threaded code IP, W */
Cell *S,T,I,*R;		/* data stack ptr, top, rtop, return stack ptr */

typedef struct Dict {
   const Byte *nfa;
   struct Dict *lfa;
   prime_t cfa;
   Cell pfa;
} DICT;

Cell memSize;
const char *blkFile;
FILE *devIN, *devOUT;
Cell *S0, *R0;
Cell BASE = 10;      /* number base */
void (*staFn)(Byte*);
DICT **CTX;			   /* current vocabulary ptr */
DICT *dFORTH,*dMACRO;/* FORTH dict, MACRO dict */
Cell *H;			      /* dictionary ptr */
#define cH           (BYTE(H))
#define PAD          (2*CELL_SIZE*8 + cH)
jmp_buf *errENV = 0;
void *mark[3];       /* mark/empty */
#define MAX_SOBJ  10
void *sobj[MAX_SOBJ];/* shared objects */
Cell nsobj = 0;
void *origin = 0;
size_t norigin;
int dbg=0;

Cell xt_dolit,xt_0branch, xt_branch;
Cell xt_tor, xt_donext;
Cell xt_exit;
Cell xt_dodotstr, xt_docstr;
Cell xt_comma;

void xexit(int);
 int c_isdelim(int delim, int ch);
void c_word(int);
void c_compiler(Byte*);
void c_interpreter(Byte*);
void fo_docol(void);
void fo_docon(void);
void fo_dovar(void);	   /* VARIABLE */
void fo_docre(void);    /* CREATE */
void fo_dodoes(void);   /* DOES> */
void fo_exit(void);
void _abort(void);
void fo_abort(void);
void fo_cold(void);
void fo_save(void);

Cell to_cell(Cell baddr) { return baddr / CELL_SIZE; }
Cell to_byte(Cell waddr) { return waddr * CELL_SIZE; }

#define DBG(lvl,stmt) if(dbg>lvl){stmt;}
// #define DBG(lvl,stmt)

Cell crossLine;
#define CNT_SIZE  8
#define STR_ADDR(x)  (BYTE(x) + CNT_SIZE)

#if CNT_SIZE==1
#define STR_CNT(x)   (*BYTE(x))
#else
#define STR_CNT(x)   (*PCELL(x))
#endif

void xexit(int code)
{
   if (origin)
      munmap(origin, norigin);
   exit(code);
}

int STRcmp(const Byte *s, const Byte *q)
{
   int ret;

	DBG(10,fprintf(stderr,"%s : %s", s, q));
	ret = stricmp(CHAR(s), CHAR(q));
   DBG(10,fprintf(stderr," => %d\n",ret));
   return ret;
}


int STRlen(Cell *nfa)
{
   /* [count]str[\0] */
	return CNT_SIZE + strlen(CHAR(STR_ADDR(nfa))) + 1;
}

int c_iscrlf(int ch)
{
   return (ch == 0x0A) || (ch == 0x0D);
}

#define NIOBUF 256
int niobuf = 0;
char iobuf[NIOBUF+1];

void c_flush(int force)
{
   if (force || (NIOBUF == niobuf)) {
      iobuf[niobuf] = '\0';
      fprintf(devOUT,"%s",&iobuf[0]);
      niobuf = 0;
   }
}

void c_emit(int ch)
{
   c_flush(0);
   iobuf[niobuf++] = ch;
   // fputc(ch, devOUT);
}

int  c_key(void)
{
   int ch;

   ch = fgetc(devIN);
   if (c_iscrlf(ch))
      crossLine = 1;
   return ch;
}
void c_type(char *s, int n)
{
    int i;
    
    if (-1 == n)
        n = strlen(s);
    for (i = 0; i < n; i++)
        c_emit(*s++);
}

void c_error(char *msg)
{
	c_type("error: ", -1);
	c_type(msg, -1);
   c_flush(1);
	xexit(1);
}

void c_throw(jmp_buf *env,int err) { if (env) longjmp(*env,err); }
void c_abort(int err)
{
   DBG(1,fprintf(stderr,"--- c_abort --- [%d]\n",err));
   c_throw(errENV,err); /* unwind stack */
   _abort();
}
void c_doabort(char *msg, int code)
{
   fprintf(stderr,"%s? ",msg); fflush(stderr);
	// c_type(msg, -1); c_type("? ", 2); fflush(devOUT);
   c_abort(code);
}

Cell chk_div0(Cell x)
{
   if (0 == x)
      c_doabort("division by zero", -2);
   return x;
}

void fo_vm(Cell *xt)
{
	prime_t prime;

   W = xt;
   prime = (prime_t)(*W++); (*prime)();
	while (P) {
		W = PCELL(*P++);
		prime = (prime_t)(*W++);
		(*prime)();
	}
}

/* stack */
void fo_drop(void)	{ T = *S++; }
void fo_dup(void)    { *--S = T; }
void fo_tor(void)	   { *--R = I; I = T; fo_drop(); }
void fo_rfrom(void)	{ fo_dup(); T = I; I = *R++; }
void fo_rfetch(void) { fo_dup(); T = I; }
void fo_swap(void)	{ Cell tmp = T; T = *S; *S = tmp; }
void fo_over(void)	{ Cell tmp = *S; fo_dup(); T = tmp; }
void fo_nip(void)    { S++; }
void fo_rot(void)    { Cell tmp = T; T = S[1]; S[1] = *S; *S = tmp; }

/* memory */
void fo_cfetch(void) { T = *BYTE(T); }
void fo_cstore(void) { *BYTE(T) = *S++; fo_drop(); }
void fo_fetch(void)  { T = *PCELL(T); }
void fo_store(void)  { *PCELL(T) = *S++; fo_drop(); }
void fo_plusstore(void)  { *PCELL(T) += *S++; fo_drop(); }
void fo_fetchplus(void) /* : @+ ( a1 - a2 x) */
{
   Cell *p = PCELL(T);

   T = CELL(p + 1);
   fo_dup(); T = CELL(*p);
}
void fo_storeplus(void) /* : !+ ( a1 x - a2) */
{
   Cell *p = PCELL(*S);

   *p++ = T; fo_drop(); 
   T = CELL(p);
}

/* logic */
void fo_and(void)	 { T &= *S++; }
void fo_or(void)	 { T |= *S++; }
void fo_xor(void)	 { T ^= *S++; }
void fo_invert(void) { T = ~T; }
void fo_zequal(void) { T = !T? -1 : 0; }

/* arithmetic */
void fo_add(void)	 { T += *S++; }
void fo_sub(void)	 { T = *S++ - T; }
void fo_mul(void)	 { T *= *S++; }
void fo_div(void)  { Cell tmp = *S++; T = tmp / chk_div0(T); }
void fo_mod(void)  { Cell tmp = *S++; T = tmp % chk_div0(T); }

/* i/o */
void fo_key(void)	 { c_flush(1); fo_dup(); T = c_key(); }
void fo_emit(void) { c_emit(255 & T); fo_drop(); }
void fo_cr(void)   { c_emit('\n'); }
void fo_space(void){ c_emit(BL); }
void fo_spaces(void)
{
   Cell i;
   for (i = 0; i < T; i++)
      c_emit(BL);
   fo_drop();
}
void fo_type(void)
{
   char *w;
   Cell n;

   n = T; fo_drop();
   w = CHAR(T); fo_drop();
   if (n > 0)
      c_type(w, n);
}
void fo_zcount(void) { const char *p = CHAR(T); fo_dup(); T = strlen(p); }
void fo_count(void)
{
   Byte *p = BYTE(T);

   T += CNT_SIZE; fo_dup();
   T = STR_CNT(p);
}
void fo_place(void)
{
   Byte *from, *to;
   Cell n;

   to   = BYTE(T); fo_drop();
   n    = T;       fo_drop();
   from = BYTE(T); fo_drop();
   STR_CNT(to) = n;
   memcpy(STR_ADDR(to), from, n);
}
void fo_append(void)
{
   Byte *from, *to;
   Cell n, oldn;

   to   = BYTE(T); fo_drop();
   n    = T;       fo_drop();
   from = BYTE(T); fo_drop();
   oldn = STR_CNT(to); STR_CNT(to) += n;
   to = STR_ADDR(to);
   memcpy(to + oldn, from, n);
}
void fo_subtrailing(void)
{
   Cell n = T;
   char *w = CHAR(*S);

   while (n && c_isdelim(BL, w[n]))
      n--;
   T = n;
}
/* control */
void c_comma(Cell x) 		{ *H++ = x; }
void c_allot(int ncells)	{ H += ncells; }

void fo_hex(void)     { BASE = 16; }
void fo_decimal(void) { BASE = 10; }
void fo_cell(void)    { fo_dup(); T = CELL_SIZE; }
void fo_cellslash(void) { T /= CELL_SIZE; }
void fo_cells(void)   { T *= CELL_SIZE; }
void fo_cellplus(void){ T += CELL_SIZE; }
void fo_cellsub(void) { T -= CELL_SIZE; }
void fo_1plus(void)   { T++; }
void fo_1sub(void)    { T--; }
void fo_2star(void)   { T <<= 1; }
void fo_2slash(void)  { T >>= 1; }
void fo_here(void)    { fo_dup(); T = CELL(H); }
void fo_comma(void)   { c_comma(T); fo_drop(); }
void c_align(Cell n)  { c_allot((n + CELL_SIZE - 1) / CELL_SIZE); }
void fo_allot(void)   { Cell tmp = T; fo_drop(); c_align(tmp); }

void fo_if(void)
{
	c_comma(xt_0branch);
	fo_dup(); T = CELL(H);
	c_comma(0);
}

void fo_else(void)
{
	Cell pos;

	pos = T;
	c_comma(xt_branch);
	T = CELL(H);
	c_comma(0);
	*PCELL(pos) = CELL(H);
}

void fo_then(void)   { *PCELL(T) = CELL(H); fo_drop(); }
void fo_begin(void)  { fo_dup(); T = CELL(H); }
void fo_again(void)  { c_comma(xt_branch); c_comma(T); fo_drop(); }
void fo_while(void)  { fo_if(); }
void fo_repeat(void) /* ( beginP whileP -- ) */
{
   fo_swap();
   fo_again();
   fo_then();
}

void fo_for(void) { c_comma(xt_tor); fo_dup(); T = CELL(H); }
void fo_next(void){ c_comma(xt_donext); c_comma(T); fo_drop(); }
void c_execute(Cell *xt)
{
   Cell *savP;

   savP = P; P = 0;
	fo_vm(xt);
   P = savP;
}
void fo_execute(void) { Cell *xt; xt = PCELL(T); T = *S++; c_execute(xt); }
void fo_fetchexecute(void)
{
   Cell *xt;

   xt = PCELL(*PCELL(T)); T = *S++;
   if (xt)
      c_execute(xt);
}

int c_isdelim(int delim, int ch)
{
    return BL == delim ? (ch <= 32) : delim == ch;
}

void c_text(Byte *p, int delim)
{
	int ch;
   Byte *q = p;

   c_flush(1);

   p = STR_ADDR(p);
	ch = c_key();
	while (EOF != ch && c_isdelim(delim, ch))
		ch = c_key();
   crossLine = 0;
	while (EOF != ch && !c_isdelim(delim, ch)) {
		*p++ = ch;
		ch = c_key();
	}
	*p++ = '\0';
   STR_CNT(q) = strlen(CHAR(STR_ADDR(q)));
}

void c_word(int delim)  { c_text(cH, delim); }
void fo_paren(void)     { c_word(')'); }
void fo_word(void)      { c_word(255 & T); T = CELL(H); }

void fo_move(void)
{
   Cell n;
   Byte *from, *to;

   n    =       T; fo_drop();
   to   = BYTE(T); fo_drop();
   from = BYTE(T); fo_drop();

   if (n > 0)
      memmove(to, from, n);
}
void fo_fill(void)
{
   Cell ch, n;
   Byte *from;

   ch   =       T; fo_drop();
   n    =       T; fo_drop();
   from = BYTE(T); fo_drop();

   memset(from, ch, n);
}

/*
 * NFA	native ptr, 0 terminated
 * LFA	cell
 * CFA	cell
 * PFA
*/
DICT* c_header(const Byte *w)
{
	DICT *d = (DICT*)H;

   d->nfa = w; d->lfa = *CTX; *CTX = d;
   H += 3;
   return d;
}

DICT* fo_header(void)
{
   Byte *w;

   c_word(BL); w = STR_ADDR(cH);
   c_align(STRlen(H));
   return c_header(w);
}

Cell* c_find(DICT *dict,Byte *s)
{
	DICT *p;
   Cell *xt = PCELL(-1);

   DBG(10,fprintf(stderr, "c_find: [%s]\n", s));

	xt = 0; p = dict;
	while (p && STRcmp(s,p->nfa))
		p = p->lfa;
	if (p)
		xt = PCELL(&(p->cfa));
   DBG(10,fprintf(stderr, " xt=%p\n",xt));
	return xt;
}

void fo_lbracket(void) { staFn = c_interpreter; }
void fo_rbracket(void) { staFn = c_compiler; }
void fo_noname(void)
{
   fo_dup(); T = CELL(H);
   c_comma(CELL(fo_docol));
   fo_rbracket();
}

void fo_colon(void) { DICT *d = fo_header(); d->cfa = fo_docol; fo_rbracket(); }
void fo_semi(void)  { c_comma(xt_exit); fo_lbracket(); }

void fo_constant(void)
{
	DICT *d = fo_header();
   d->cfa = fo_docon;
   d->pfa = T; H++;
	fo_drop();
}
void fo_create(void) { DICT *d = fo_header(); d->cfa = fo_docre; d->pfa = 0; H++; }
void fo_does(void)
{
   DICT *last = *CTX;

   last->cfa = fo_dodoes;
   last->pfa = CELL(P);
	fo_exit();
}
void fo_variable(void) { DICT *d = fo_header(); d->cfa = fo_dovar; d->pfa = 0; H++; }

/* inner interpreter */
void fo_docol(void) 	{ *--R = I; I = CELL(P); P = W; }
void fo_exit(void) 	{ P = PCELL(I); I = *R++; }
void fo_docon(void) 	{ fo_dup(); T = *W; }
void fo_dovar(void) 	{ fo_dup(); T = CELL(W); }
void fo_docre(void)  { fo_dup(); T = CELL(W+1); }
void fo_dodoes(void)
{
	*--R = I; I = CELL(P);
	fo_dup();
	P = PCELL(*W);
	T = CELL(W + 1);
}
void fo_dolit(void)	{ fo_dup(); T = *P++; }
void fo_branch(void) { P = PCELL(*P); }
void fo_0branch(void)
{
	Cell tmp;

	tmp = T; fo_drop();
	if (0 == tmp) P = PCELL(*P);
	else P++;
}
void fo_donext(void)
{
    if (--I >= 0) {
        P = PCELL(*P);        
    } else {
        I = *R++;
        P++;
    }
}
void fo_aft(void)
{
	c_comma(xt_branch);
	T = CELL(H);
	c_comma(0);
   fo_dup(); *S = CELL(H);
}

int c_tolower(int ch)
{
   return ('A' <= ch) && (ch <= 'Z') ? ch + 'a' - 'A' : ch;
}

int c_digitq(int ch)
{
	int ret = '0' <= ch;

	if (BASE <= 10)
		ret = ret && (ch < '0' + BASE);
	else {
		ret = ret && ((ch <= '9') || (('a' <= ch) && (ch < 'a' + BASE - 10)));
	}
	return ret;
}

int c_todigit(int ch)
{
	return ('a' <= ch) ? ch - 'a' + 10 : ch - '0';
}

Cell c_tonumber(Byte *s)
{
	Cell ret = 0, savBASE = BASE;
	int ch, sign = 0;
   Byte *p = s;

	if ('-' == *p) {
    	sign = 1; p++;
	} else if ('$' == *p) {
      BASE = 16; p++;
   }
	while ((ch = c_tolower(*p++))) {
		if (!c_digitq(ch)) { 
              if ('k' == ch) ret <<= 10;
         else if ('m' == ch) ret <<= 20;
         else if ('g' == ch) ret <<= 30;
         else
            c_doabort(CHAR(s),-3);
      }
      else
		   ret = BASE * ret + c_todigit(ch);
	}
	ret = sign ? -ret : ret;
   BASE = savBASE;
   return ret;
}

char* c_ntoa(Cell n, int base)
{
	static char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
	Byte *p = PAD - 1;
   int sign = 0;

   if (n < 0) {
      sign = 1;
      n = -n;
   }
	*p = '\0';
	do {
		*--p = digits[n % base];
		n /= base;
	} while (n);
   if (sign)
      *--p = '-';
	
	return CHAR(p);
}

void c_typer(char *p, int n, int m)
{
   int i;
    
	if (-1 == n)
	   n = strlen(p);
	for (i = 0; i < m - n; i++) {
    	c_emit(' ');
	}
	c_type(p, n);
}

void c_dotr(Cell n, int w, int base)
{
    c_typer(c_ntoa(n, base), -1, w);
    if (!w)
        c_emit(' ');
}

void fo_dot(void)  { Cell n = T; fo_drop(); c_dotr(n, 0, BASE); }
void fo_dotr(void)
{
   Cell w = T, n = *S;

   T = S[1]; S += 2;
   c_dotr(n, w, BASE);
}
Cell* c_docstr(Cell *n)
{
   Cell *w;

   w = P;
   *n = STR_CNT(P);
   P += (STRlen(w) + CELL_SIZE - 1) / CELL_SIZE;

   return w;
}
void fo_dodotstr(void)
{
   Cell *w;
   Cell n;

   w = c_docstr(&n);
   c_typer(CHAR(STR_ADDR(w)), n, 0);
}
void fo_docstr(void)
{
   Cell *w, n;

   w = c_docstr(&n);
   fo_dup(); T = CELL(w);
}
void c_commastr(void)
{
   c_word('"');
   c_align(STRlen(H));
}
void fo_dotstr(void)
{
   c_comma(xt_dodotstr);
   c_commastr();
}
void fo_cstr(void)
{
   c_comma(xt_docstr);
   c_commastr();
}
void fo_dotparen(void)
{
   c_word(')');
   c_typer(CHAR(STR_ADDR(cH)),-1,0);
}
void fo_macro(void) { CTX = &dMACRO; }
void fo_forth(void) { CTX = &dFORTH; }
void fo_mark(void)
{
    mark[0] = H;
    mark[1] = dFORTH;
    mark[2] = dMACRO;
}
void fo_empty(void)
{
    H = mark[0];
    dFORTH = mark[1];
    dMACRO = mark[2];
}
void fo_bye(void)   { xexit(T); }
void fo_divmod(void)
{
    Cell tmp = T;
    
     T = *S / chk_div0(T);
    *S = *S % tmp;
}
void fo_muldiv(void)
{
   DCell d = S[1];

   d = (S[1] * (DCell) S[0]) / chk_div0(T);
   T = d; S += 2;
}
void fo_abs(void)   { if (T < 0) T = -T; }
void fo_negate(void){ T = -T; }
void fo_min(void)   { Cell n = *S++; if (n < T) T = n;}
void fo_max(void)   { Cell n = *S++; if (n > T) T = n;}
void fo_equal(void) { T = *S++ == T ? -1 : 0; }
void fo_less(void)  { T = *S++ <  T ? -1 : 0; }
void fo_greater(void)  { T = *S++ >  T ? -1 : 0; }

void fo_qms(void)
{
    fo_dup();
    T = CELL(1000 * ((double)clock() / CLOCKS_PER_SEC));
}

void c_interpreter(Byte *w)
{
	Cell *xt, n;

   w = STR_ADDR(w);
	if ((xt = c_find(dFORTH, w)))
		c_execute(xt);
	else {
		n = c_tonumber(w);
		fo_dup();
		T = n;
	}
}

void fo_literal(void)  { c_comma(xt_dolit); c_comma(T); fo_drop(); }

void c_compiler(Byte *w)
{
	Cell *xt;

   w = STR_ADDR(w);
	if ((xt = c_find(dMACRO, w)))
		c_execute(xt);
	else if ((xt = c_find(dFORTH, w)))
		c_comma(CELL(xt));
	else {
		fo_dup(); T = c_tonumber(w);
      fo_literal();
	}
}

Cell wtick;
void fo_tick(void)
{
   Byte *w;
   Cell *xt;

   c_word(BL); w = STR_ADDR(cH);
   wtick = -1;
	xt = c_find(dMACRO, w);
	if (!xt) {
      wtick = 1;
      xt = c_find(dFORTH, w);
   }
   if (!xt) {
      wtick = 0;
      c_doabort("undefined",-4);
   }
   fo_dup(); T = CELL(xt);
}
void fo_bratick(void)
{
   fo_tick();
   fo_literal();
}
void fo_postpone(void)
{
   fo_tick();
   if (wtick < 0) { // [COMPILE]
      fo_comma();
   }
   else { // COMPILE
      fo_literal();
      c_comma(xt_comma);
   }
}
void fo_dlopen(void) /* : (dlopen) ( sa -- ) */
{
   Byte *w;

   w = BYTE(T); fo_drop();
   w = STR_ADDR(w);

   if (MAX_SOBJ == nsobj)
      c_doabort("too much shared objects",-5);
   if (!(sobj[nsobj] = dlopen(CHAR(w),RTLD_LAZY)))
      c_doabort(dlerror(),-5);
   nsobj++;
}
void fo_dlsym(void) /* : (dlsym) ( sa -- h ) */
{
   int i;
   Byte *w;
   void *h;

   w = BYTE(T); w = STR_ADDR(w);
   for (i = nsobj; i > 0; i--) {
      if ((h = dlsym(sobj[i-1],CHAR(w)))) {
         T = CELL(h); return;
      }
   }
   T = 0;
}
void fo_callc(void) /* : (CALLC) ( an .. a1 narg fn -- x ) */
{
   long (*fn)(),a[8],ret;
   int narg,i;

   fn = (long (*)())T;
   fo_drop(); narg = T;
   DBG(2,fprintf(stderr,"--- callc: fn=%p narg=%d\n",fn,narg));
   for (i = 0; i < narg; i++) {
      fo_drop(); a[i] = T;
      DBG(2,fprintf(stderr,"--- a[%d] = %ld\n",i,a[i]));
   }

   ret = 0;
   switch(narg) {
   case 0: ret = (*fn)(); break;
   case 1: ret = (*fn)(a[0]); break;
   case 2: ret = (*fn)(a[0],a[1]); break;
   case 3: ret = (*fn)(a[0],a[1],a[2]); break;
   case 4: ret = (*fn)(a[0],a[1],a[2],a[3]); break;
   case 5: ret = (*fn)(a[0],a[1],a[2],a[3],a[4]); break;
   case 6: ret = (*fn)(a[0],a[1],a[2],a[3],a[4],a[5]); break;
   case 7: ret = (*fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6]); break;
   case 8: ret = (*fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]); break;
   default: break;
   }
   T = ret;
}

static long do_cb(long i,long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8);

static long cb0(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(0,a1,a2,a3,a4,a5,a6,a7,a8); }
static long cb1(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(1,a1,a2,a3,a4,a5,a6,a7,a8); }
static long cb2(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(2,a1,a2,a3,a4,a5,a6,a7,a8); }
static long cb3(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(3,a1,a2,a3,a4,a5,a6,a7,a8); }
static long cb4(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(4,a1,a2,a3,a4,a5,a6,a7,a8); }
static long cb5(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(5,a1,a2,a3,a4,a5,a6,a7,a8); }
static long cb6(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(6,a1,a2,a3,a4,a5,a6,a7,a8); }
static long cb7(long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) { return do_cb(7,a1,a2,a3,a4,a5,a6,a7,a8); }

struct {
   Cell *xt;
   int  narg;
   void *fn;
} tabcb[] = {
   {0,-1,cb0},
   {0,-1,cb1},
   {0,-1,cb2},
   {0,-1,cb3},
   {0,-1,cb4},
   {0,-1,cb5},
   {0,-1,cb6},
   {0,-1,cb7}
};
static long do_cb(long i,long a1,long a2,long a3,long a4,long a5,long a6,long a7,long a8) {
   Cell ret;

   if (-1 == tabcb[i].narg)
      return 0;

   switch (tabcb[i].narg) {
   case 0: goto L0;
   case 1: goto L1;
   case 2: goto L2;
   case 3: goto L3;
   case 4: goto L4;
   case 5: goto L5;
   case 6: goto L6;
   case 7: goto L7;
   case 8: goto L8;
   }
L8: fo_dup(); T = a8;
L7: fo_dup(); T = a7;
L6: fo_dup(); T = a6;
L5: fo_dup(); T = a5;
L4: fo_dup(); T = a4;
L3: fo_dup(); T = a3;
L2: fo_dup(); T = a2;
L1: fo_dup(); T = a1;
L0: 
   c_execute(tabcb[i].xt);
   ret = T; fo_drop();
   return ret;
}
void fo_cb(void) /* : (CB) ( xt narg idx -- ptr ) */
{
   Cell idx, narg;

   idx  = T; fo_drop();
   narg = T; fo_drop();
   tabcb[idx].xt = PCELL(T);
   tabcb[idx].narg = narg;
   T = CELL(tabcb[idx].fn);
}
void fo_spat(void) { fo_dup(); T = CELL(S); }

void c_mainloop()
{
   jmp_buf *savERR, errHandler;
   int err;

   savERR = errENV; errENV = &errHandler;
	if ((err = setjmp(errHandler))) {
      DBG(1,fprintf(stderr,"--- c_mainloop ---\n"));
      errENV = savERR;
      c_abort(err);
   }

   crossLine = 0;
	for (;;) {
		c_word(BL);
      if (0 == STR_CNT(cH))
         break;
		(*staFn)(cH);
      if (crossLine && isatty(fileno(devIN))) {
         fo_cr();
         c_dotr(S[2], 0, BASE);
         c_dotr(S[1], 0, BASE);
         c_dotr(S[0], 0, BASE);
         c_dotr(   T, 0, BASE);
         c_type("> ", -1);
         crossLine = 0;
      }
	}
}

void c_include(char *path)
{
   FILE * volatile savIN;
   jmp_buf *savERR, errHandler;
   volatile int err;
   char tmp[FILENAME_MAX];

   strcpy(tmp,path);

   err = 0;
   savIN = devIN;
   savERR = errENV; errENV = &errHandler;

   if ((err = setjmp(errHandler))) {
      DBG(1,fprintf(stderr,"c_include: %s\n",tmp));
      goto Lexit;
   }

   devIN = fopen(path, "rt");
   if (!devIN)
      c_doabort(path,-6);
	c_mainloop();

Lexit:
   if (devIN)
      fclose(devIN);
   errENV = savERR;
   devIN = savIN;
   if (err)
      c_throw(errENV,err);
   return;
}
void fo_include(void) { c_word(BL); c_include(CHAR(STR_ADDR(cH))); }

void c_init_io(void)
{
	devIN  = stdin;
	devOUT = stdout;
}

void fo_block(void)
{
   T = origin? CELL(BYTE(origin) + 1024 * T) : 0;
}

void fo_save(void)
{
   if (-1 == msync(origin, norigin, MS_SYNC))
      c_doabort(strerror(errno),-7);
}

typedef struct _dict_entry {
	const char *nm;
	prime_t fn;
} dict_entry_t;

void c_dict(void)
{
	int i;
   DICT *d;
	dict_entry_t words[] = {
		{"<LIT>",	fo_dolit},
		{"0BRANCH", fo_0branch},
		{"BRANCH",  fo_branch},
		{"<NEXT>",  fo_donext},
      {"<.\">",   fo_dodotstr},
      {"<C\">",   fo_docstr},

		{"EXIT",    fo_exit},      /* control */
		{"'",	      fo_tick},
		{"EXECUTE", fo_execute},
		{"@EXECUTE",fo_fetchexecute},
		{"I",       fo_rfetch},

		{">R",      fo_tor},       /* stack */
		{"R>",      fo_rfrom},
		{"R@",	   fo_rfetch},
		{"DROP",    fo_drop},
		{"DUP",     fo_dup},
		{"SWAP",    fo_swap},
		{"OVER",    fo_over},

		{"C@",      fo_cfetch},    /* memory */
		{"C!",      fo_cstore},
		{"@",       fo_fetch},
		{"!",       fo_store},

		{"AND",     fo_and},       /* logic */
		{"OR",      fo_or},
		{"XOR",     fo_xor},
      {"INVERT",  fo_invert},
      {"0=",      fo_zequal},

		{"+",		fo_add},          /* arithmetic */
		{"-",		fo_sub},
		{"*",		fo_mul},
		{"/",		fo_div},
		{"MOD",		fo_mod},

		{"KEY",		fo_key},       /* io */
		{"EMIT",	fo_emit},

		{":",		fo_colon},        /* defining */
		{"CONSTANT",fo_constant},
		{"VARIABLE",fo_variable},

		{"CR",      fo_cr},        /* Moore io */
      {".R",      fo_dotr},
      {".",       fo_dot},
      {"HEX",     fo_hex},
      {"DECIMAL", fo_decimal},

		{",",       fo_comma},     /* Moore defining */
		{"ALLOT",   fo_allot},
		{"CREATE",  fo_create},

		{"/MOD",    fo_divmod},    /* Moore arithmetic */
		{"ABS",     fo_abs},
		{"NEGATE",  fo_negate},
		{"MIN",     fo_min},
		{"MAX",     fo_max},
      {"*/",      fo_muldiv},

		{"<",       fo_less},      /* Moore logic */
		{"=",       fo_equal},
		{">",       fo_greater},

		{"EMPTY",   fo_empty},     /* Moore control */

/* --- START --- */
		{"MARK",    fo_mark},
		{"MACRO",   fo_macro},
		{"FORTH",   fo_forth},
		{"BYE",     fo_bye},

      {"(",       fo_paren},     /* eForth */
      {"HERE",    fo_here},
      {"+!",      fo_plusstore},
      {"SPACE",   fo_space},
      {"SPACES",  fo_spaces},
      {".(",      fo_dotparen},

		{"?MS",     fo_qms},       /* extensions */
		{"WORD",    fo_word},
      {":NONAME", fo_noname},
      {"INCLUDE", fo_include},
      {"MOVE",    fo_move},
      {"FILL",    fo_fill},
      {"ABORT",   fo_abort},
      {"COLD",    fo_cold},

		{"DOES>",	fo_does},

      {"CELL",    fo_cell},
      {"CELLS",   fo_cells},
      {"CELL/",   fo_cellslash},
      {"CELL+",   fo_cellplus},
      {"CELL-",   fo_cellsub},
      {"1+",      fo_1plus},
      {"1-",      fo_1sub},
      {"2*",      fo_2star},
      {"2/",      fo_2slash},

      {"TYPE",    fo_type},
      {"COUNT",   fo_count},
      {"-TRAILING", fo_subtrailing},
      {"PLACE",   fo_place},
      {"APPEND",  fo_append},

      {"NIP",     fo_nip},
      {"ROT",     fo_rot},
      {"]",       fo_rbracket},

      {"(DLOPEN)",fo_dlopen},
      {"(DLSYM)", fo_dlsym},
      {"(CALLC)", fo_callc},
      {"SP@",     fo_spat},

      {"BLOCK",   fo_block},
      {"SAVE",    fo_save},

      {"ZCOUNT",  fo_zcount},
      {"@+",      fo_fetchplus},
      {"!+",      fo_storeplus},
/* --- END --- */
		{NULL,		0},
	};
	dict_entry_t macros[] = {
		{"IF",		fo_if},           /* control */
		{"ELSE",	   fo_else},
		{"THEN",	   fo_then},
		{"BEGIN",	fo_begin},
		{"WHILE",	fo_while},
		{"REPEAT",	fo_repeat},
		{"AGAIN",	fo_again},
		{"FOR",     fo_for},
		{"NEXT",    fo_next},
      {"(",       fo_paren},

		{";",		   fo_semi},         /* defining */

      {".\"",     fo_dotstr},       /* eForth */
      {"AFT",     fo_aft},

      {"C\"",     fo_cstr},
      {"[']",     fo_bratick},
      {"LITERAL", fo_literal},
      {"[",       fo_lbracket},
      {"POSTPONE",fo_postpone},

		{NULL,		0},
	};

	CTX = &dMACRO;
	for (i = 0; macros[i].nm; i++) {
		d = c_header(BYTE(macros[i].nm));
      d->cfa = macros[i].fn;
	}
	CTX = &dFORTH;
	for (i = 0; words[i].nm; i++) {
		d = c_header(BYTE(words[i].nm));
      d->cfa = words[i].fn;
	}
}
void fo_abort(void) { c_abort(-1); }
void _abort(void)
{
	S = S0; R = R0;
   *--S = 0;
   *--S = 0;
   *--S = 0;
      T = 0;
   *--R = 0xDEADBEEF;
   *--R = 0xDEADBEEF;
   *--R = 0xDEADBEEF;
      I = 0xDEADBEEF;
	BASE  = 10;
   fo_lbracket();
	CTX   = &dFORTH;

	c_init_io();
}

void fo_cold(void)
{
   int fd;

   if (origin) {
      munmap(origin,norigin);
      origin = 0;
   }
   if (M) free(M);
	M = PCELL(malloc(memSize*CELL_SIZE));
	if (0 == M) c_error("not enough memory");
	S0 = M + memSize - DSTACK_SIZE;
	R0 = S0 - RSTACK_SIZE;
	H  = M + 0x100;
	dFORTH = 0;
	dMACRO = 0;

	c_dict();
	xt_exit    = CELL(c_find(dFORTH,BYTE("EXIT")));
	xt_0branch = CELL(c_find(dFORTH,BYTE("0BRANCH")));
	xt_branch  = CELL(c_find(dFORTH,BYTE("BRANCH")));
	xt_dolit   = CELL(c_find(dFORTH,BYTE("<LIT>")));
	xt_tor     = CELL(c_find(dFORTH,BYTE(">R")));
	xt_donext  = CELL(c_find(dFORTH,BYTE("<NEXT>")));
   xt_dodotstr= CELL(c_find(dFORTH,BYTE("<.\">")));
   xt_docstr  = CELL(c_find(dFORTH,BYTE("<C\">")));
   xt_comma   = CELL(c_find(dFORTH,BYTE(",")));

   sobj[nsobj++] = dlopen(NULL,RTLD_LAZY);

   fd = open(blkFile, O_RDWR);
   if (fd >= 0) {
      off_t offs = lseek(fd, 0, SEEK_END);
      if (-1 != offs) {
         norigin = offs;
         origin = mmap(0, norigin, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
         if (MAP_FAILED == origin)
            origin = 0;
      }
      close(fd);
   }

	fo_abort();
}

void usage()
{
   fprintf(stderr,"usage: vfc [-b file.blk][-m mem] include1 ...\n");
   exit(1);
}

int main(int argc, char *argv[])
{
   int i;
   char *str;

	if (PRIME_SIZE > CELL_SIZE) {
		c_error("address size is greater than cell size");
	}
   memSize = 256*1024*CELL_SIZE;
   blkFile = "fo.blk";
   for (i = 1; i < argc; i++) {
      str = argv[i];
      if ('-' == *str) {
         switch (str[1]) {
         case 'b':
            i++; blkFile = argv[i];
            break;
         case 'd':
            i++; dbg = c_tonumber(BYTE(argv[i]));
            break;
         case 'm':
            i++; memSize = c_tonumber(BYTE(argv[i]));
            break;
         default: usage();
         }
      }
   }

	fo_cold();
   for (i = 1; i < argc; i++) {
      str = argv[i];
      if ('-' == *str)
         i++;
      else
         c_include(argv[i]);
   }
   fo_abort();
   c_mainloop();
	return 0;
}


// vim:sw=3:ts=3:et
