/* C.H.Ting: Virtual Forth Computer
 *
 * Dictionary header:
 *  |...name...| NFA | LFA | CFA | PFA ...
 *  ^
 * NFA
 *
 * NFA points to the beginning of the zero terminated name.
 * CONTEXT is a pointer to a dictionary link cell, which contains
 * the last NFA. <name> is extended to cell boundary.
 *
 *
 * History
 * =======
 * 231209AP added UPDATE, block cache (-c option sets the size)
 * 230925AP added \, it searches only the MACRO dictionary
 *          changed ', it searches the current dictionary
 *          renamed CURRENT to CONTEXT
 * 230922AP added /BLOCK, added reDef msg, renamed CTX to CURRENT
 * 230921AP added UP USER, removed BASE OFFSET
 * 230915AP added LOAD OFFSET SP! RP@ RP!
 * 230914AP removed LITERAL POSTPONE [ added PAD
 *          removed address register, ." and <."> runtime
 *          removed AFT :NONAME
 * 230910AP added BASE removed CO, block file loaded instead of mmapped
 *          added decimal number input #1234
 * 230909AP removed NIP ROT CELL/ CELL- @+ !+ ['] @EXECUTE .(
 * 221014AP >A A> A@ A! A@+ A!+
 * 220930AP coroutines CO
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
#include <errno.h>
#include <string.h>

typedef void (*prime_t)(void);
typedef long Cell;
typedef unsigned long UCell;
#ifdef __LP64__
#define ha  11400714819323198485ULL
typedef unsigned __int128 DCell;
#else
#define ha  2654435769UL
typedef unsigned long long DCell;
#endif
typedef unsigned char Byte;

#define BYTE(x)      ((Byte*)(x))
#define CHAR(x)      ((char*)(x))
#define CELL(x)      ((Cell)(x))
#define UCELL(x)     ((UCell)(x))
#define PCELL(x)     ((Cell*)(x))

#define CELL_MSB     (UCELL(-1) ^ (UCELL(-1) >> 1))
#define ABS(x)       ((x) < 0 ? -(x) : (x))

#define DSTACK_SIZE	(1500)
#define RSTACK_SIZE	( 750)

#define PRIME_SIZE	(sizeof(prime_t))
#define CELL_SIZE	   (sizeof(Cell))

#define BL	' '

Cell *M = 0;         /* memory */
Cell *P,*W;			   /* indirect-threaded code IP, W */
Cell *S,T,I,*R;		/* data stack ptr, top, rtop, return stack ptr */
Cell *UP;            /* user area */

#define N   *S

typedef struct Dict {
   const Byte *nfa;
   struct Dict *lfa;
   prime_t cfa;
   Cell pfa;
} DICT;

Cell memSize;
Cell *S0, *R0;
/* User variables: LINK STATUS TOS TOS0 BASE OFFSET */

Cell dummyUP[6];
#define LINK   UP[0]
#define STATUS UP[1]
#define TOS    UP[2]
#define TOS0   UP[3]
#define BASE   UP[4]
#define OFFSET UP[5]

void (*staFn)(Byte*);
DICT **CONTEXT;	   /* current vocabulary ptr */
DICT *dFORTH,*dMACRO;/* FORTH dict, MACRO dict */
Cell *H;			      /* dictionary ptr */
#define cH           (BYTE(H))
#define PAD          (2*CELL_SIZE*8 + cH)
jmp_buf *errENV = 0;
void *mark[3];       /* mark/empty */
#define MAX_SOBJ  10
void *sobj[MAX_SOBJ];/* shared objects */
Cell nsobj = 0;
const char *blkFile;    /* name of BLOCK file */
Cell nblk;              /* no. of blocks in the BLOCK file */
Cell curBLK = 0;        /* current block buffer*/
int fdBLK = -1;         /* fd of BLOCK file */
Cell *blkupd;           /* contains block numbers, if < 0, modified */
void *origin = 0;       /* base of BLOCK cache */
int nbuf = 128;         /* no. of buffers in the BLOCK cache */
int dbg=0;

Cell xt_dolit,xt_0branch, xt_branch;
Cell xt_tor, xt_donext;
Cell xt_exit;
Cell xt_docstr;

void xexit(int);
 int c_isdelim(int delim, int ch);
void c_word(int);
void c_compiler(Byte*);
void c_interpreter(Byte*);
void fo_docol(void);
void fo_docon(void);
void fo_douser(void);   /* USER     */
void fo_dovar(void);	   /* VARIABLE */
void fo_docre(void);    /* CREATE   */
void fo_dodoes(void);   /* DOES>    */
void fo_exit(void);
void _abort(void);
void fo_abort(void);
void fo_cold(void);
void fo_save(void);
char* c_block(Cell blk);

#ifdef NDEBUG
#define DBG(lvl,stmt)
#else
#define DBG(lvl,stmt) if(dbg>lvl){stmt;}
#endif

#define CNT_SIZE  8
#define STR_ADDR(x)  (BYTE(x) + CNT_SIZE)

#if CNT_SIZE==1
#define STR_CNT(x)   (*BYTE(x))
#else
#define STR_CNT(x)   (*PCELL(x))
#endif

int ToLower(int ch)
{
   return ('A' <= ch) && (ch <= 'Z') ? ch + 'a' - 'A' : ch;
}

unsigned long StrLen(const char *s)
{
   unsigned long cnt = 0;

   if (s)
      while (*s++)
         cnt++;
   return cnt;
}

char *StrCpy(char *dst, const char *src)
{
   char *p = dst;

   if (src)
      while ((*p++ = *src++))
         ;
   return dst;
}

int StrCaseCmp(const char *s1, const char *s2)
{
   char c1, c2;

   c1 = ToLower(*s1); c2 = ToLower(*s2);
   while (c1 && c2 && (c1 == c2)) {
      s1++; s2++;
      c1 = ToLower(*s1); c2 = ToLower(*s2);
   }
   if (c1 || c2)
      return c1 - c2;
   return 0;
}

unsigned char *MemSet(unsigned char *b, int c, unsigned long len)
{
   unsigned char *p = b;

   while (len--)
      *p++ = c;
   return b;
}

unsigned char *MemMove(unsigned char *dst, unsigned char *src, unsigned long len)
{
   unsigned char *p = dst;

   if (src <= p && p < src + len)
   {
      src += len; p += len;
      while (len--)
         *--p = *--src;
   } else {
      while (len--)
         *p++ = *src++;
   }

   return dst;
}

int FPutS(const char *s, FILE *stream)
{
   int i, rc;

   for (i = 0; s[i]; i++) {
      rc = fputc(s[i], stream);
      if (EOF == rc)
         return rc;
   }
   return 0;
}


void xexit(int code)
{
   if (origin) {
      free(origin);
      close(fdBLK);
   }
   exit(code);
}

int STRcmp(const Byte *s, const Byte *q)
{
   int ret;

	DBG(10,fprintf(stderr,"%s : %s", s, q));
	ret = StrCaseCmp(CHAR(s), CHAR(q));
   DBG(10,fprintf(stderr," => %d\n",ret));
   return ret;
}


int STRlen(Cell *nfa)
{
   /* [count]str[\0] */
	return CNT_SIZE + StrLen(CHAR(STR_ADDR(nfa))) + 1;
}

int c_iscrlf(int ch)
{
   return (ch == 0x0A) || (ch == 0x0D);
}

#define NIOBUF 256
char conbufOUT[NIOBUF+2];
char conbufIN[NIOBUF+2];

typedef struct _IODESC {
   int   f_nbuf;
   char *f_buf;
   int   f_IN;
   FILE *f_dev;
   Cell  f_BLK;
} IODESC;
IODESC currIN, currOUT;
int ioinput = 0;

#define noutbuf   currOUT.f_nbuf
#define outbuf    currOUT.f_buf
#define devOUT    currOUT.f_dev

#define ninbuf    currIN.f_nbuf
#define inbuf     currIN.f_buf
#define IN        currIN.f_IN
#define devIN     currIN.f_dev
#define BLK       currIN.f_BLK

void io_init(IODESC *desc,FILE *dev,char *buf,Cell blk)
{
   desc->f_dev  = dev;
   desc->f_nbuf = blk ? 1024 : 0;
   desc->f_buf  = blk ? c_block(blk) : buf;
   desc->f_IN   = 0;
   desc->f_BLK  = blk;
}

void c_flush(int force)
{
   if (force || (NIOBUF == noutbuf)) {
      outbuf[noutbuf] = '\0';
      FPutS(&outbuf[0], devOUT);
      if (dbg) fflush(devOUT);
      noutbuf = 0;
   }
}

void c_emit(int ch)
{
   if (ioinput)
      noutbuf = 0;
   ioinput = 0;
   c_flush(dbg);
   outbuf[noutbuf++] = ch;
}

int  c_key(void)
{
   int ch;

   ch = fgetc(devIN);
   return ch;
}

void c_type(const char *s, int n)
{
    int i;
    
    if (-1 == n)
        n = StrLen(s);
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
   FPutS(msg, stderr); FPutS("? ", stderr); fflush(stderr);
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
void fo_swap(void)	{ Cell tmp = T; T = N; N = tmp; }
void fo_over(void)	{ Cell tmp = N; fo_dup(); T = tmp; }

void fo_up(void)     { fo_dup(); T = CELL(&UP); }
void fo_context(void){ fo_dup(); T = CELL(&CONTEXT); }

/* memory */
void fo_cfetch(void) { T = *BYTE(T); }
void fo_cstore(void) { *BYTE(T) = *S++; fo_drop(); }
void fo_fetch(void)  { T = *PCELL(T); }
void fo_store(void)  { *PCELL(T) = *S++; fo_drop(); }
void fo_plusstore(void)  { *PCELL(T) += *S++; fo_drop(); }

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
void fo_divmod(void) /* /MOD ( a b - r q) */
{
    Cell tmp = chk_div0(T);

    T = N / tmp;
    N = N % tmp;
}
void fo_div(void)  { fo_divmod(); S++; }
void fo_mod(void)  { fo_divmod(); fo_drop(); }

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
void fo_zcount(void) { const char *p = CHAR(T); fo_dup(); T = StrLen(p); }
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
   MemMove(STR_ADDR(to), from, n);
}
void fo_append(void)
{
   Byte *from, *to;
   Cell n, oldn;

   to   = BYTE(T);    fo_drop();
   n    = T;          fo_drop();
   from = BYTE(T);    fo_drop();
   oldn = STR_CNT(to); STR_CNT(to) += n;
   to = STR_ADDR(to);
   MemMove(to + oldn, from, n);
}
void fo_subtrailing(void)
{
   Cell n = T;
   char *w = CHAR(N);

   while (n && c_isdelim(BL, w[n-1]))
      n--;
   T = n;
}
/* control */
void c_comma(Cell x) 		{ *H++ = x; }
void c_allot(int ncells)	{ H += ncells; }

void fo_hex(void)     { BASE = 16; }
void fo_decimal(void) { BASE = 10; }
void fo_cell(void)    { fo_dup(); T = CELL_SIZE; }
void fo_cells(void)   { T *= CELL_SIZE; }
void fo_cellplus(void){ T += CELL_SIZE; }
void fo_1plus(void)   { T++; }
void fo_1sub(void)    { T--; }
void fo_2star(void)   { T <<= 1; }
void fo_2slash(void)  { T >>= 1; }
void fo_here(void)    { fo_dup(); T = CELL(H); }
void fo_pad(void)     { fo_dup(); T = CELL(PAD); }
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
void fo_until(void)  { c_comma(xt_0branch); c_comma(T); fo_drop(); }
void fo_while(void)  { fo_if(); fo_swap(); }
void fo_repeat(void) /* ( whileP beginP -- ) */
{
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

int c_isdelim(int delim, int ch)
{
    return BL == delim ? (ch <= 32) : delim == ch;
}

int c_refill(void)
{
   int ch;

   if (isatty(fileno(devOUT)) && !ioinput)
      c_flush(1);

   if (BLK) {
      return IN ? 1 : 0;
   }

   ninbuf = 0; ioinput = 1;
   ch = c_key();
   if (EOF == ch)
      return 1;
   while (EOF != ch && !c_iscrlf(ch)) {
      inbuf[ninbuf++] = ch;
      ch = c_key();
   }
   inbuf[ninbuf++] = ' ';
   IN = 0;

   DBG(2,fprintf(stdout,"REFILL: [%s] ninbuf=%d\n",inbuf,ninbuf));
   return 0;
}

void c_skipscan(Byte *p, int delim)
{
	int ch;
   Byte *q = p;

   p = STR_ADDR(p);
	ch = inbuf[IN++];
	while (IN < ninbuf && c_isdelim(delim, ch))
		ch = inbuf[IN++];
	while (IN < ninbuf && !c_isdelim(delim, ch)) {
		*p++ = ch;
		ch = inbuf[IN++];
	}
	*p++ = '\0';
   STR_CNT(q) = StrLen(CHAR(STR_ADDR(q)));
}

void c_word(int delim)  { c_skipscan(cH, delim); }
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
      MemMove(to, from, n);
}
void fo_fill(void)
{
   Cell ch, n;
   Byte *from;

   ch   =       T; fo_drop();
   n    =       T; fo_drop();
   from = BYTE(T); fo_drop();

   MemSet(from, ch, n);
}

/*
 * NFA	native ptr, 0 terminated
 * LFA	cell
 * CFA	cell
 * PFA
*/
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
DICT* c_header(const Byte *w)
{
	DICT *d = (DICT*)H;

   d->nfa = w; d->lfa = *CONTEXT; *CONTEXT = d;
   H += 3;
   return d;
}
DICT* fo_header(void)
{
   Byte *w;

   c_word(BL); w = STR_ADDR(cH);
   if (c_find(*CONTEXT,w)) {
      c_type("reDef ",-1);
      c_type(CHAR(w), -1);
   }
   c_align(STRlen(H));
   return c_header(w);
}

void fo_lbracket(void) { staFn = c_interpreter; }
void fo_rbracket(void) { staFn = c_compiler; }
void fo_colon(void) { DICT *d = fo_header(); d->cfa = fo_docol; fo_rbracket(); }
void fo_semi(void)  { c_comma(xt_exit); fo_lbracket(); }

void fo_constant(void)
{
	DICT *d = fo_header();
   d->cfa = fo_docon;
   d->pfa = T; H++;
	fo_drop();
}
void fo_user(void)
{
	DICT *d = fo_header();
   d->cfa = fo_douser;
   d->pfa = T; H++;
	fo_drop();
}
void fo_create(void) { DICT *d = fo_header(); d->cfa = fo_docre; d->pfa = 0; H++; }
void fo_does(void)
{
   DICT *last = *CONTEXT;

   last->cfa = fo_dodoes;
   last->pfa = CELL(P);
	fo_exit();
}
void fo_variable(void) { DICT *d = fo_header(); d->cfa = fo_dovar; d->pfa = 0; H++; }

/* inner interpreter */
void fo_docol(void) 	{ *--R = I; I = CELL(P); P = W; }
void fo_exit(void) 	{ P = PCELL(I); I = *R++; }
void fo_douser(void) { fo_dup(); T = CELL((Byte*)UP + (*W)); }
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
   } else if ('#' == *p) {
      BASE = 10; p++;
   }
	while ((ch = ToLower(*p++))) {
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
	   n = StrLen(p);
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
   Cell w = T, n = N;

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
void fo_cstr(void)
{
   c_comma(xt_docstr);
   c_commastr();
}
void fo_macro(void) { CONTEXT = &dMACRO; }
void fo_forth(void) { CONTEXT = &dFORTH; }
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
void c_tick(DICT *d)
{
   Byte *w;
   Cell *xt;

   c_word(BL); w = STR_ADDR(cH);
	xt = c_find(d, w);
   if (!xt)
      c_doabort("undefined",-4);
   fo_dup(); T = CELL(xt);
}
void fo_tick(void)
{
   c_tick(*CONTEXT);
}
void fo_bkslash(void)
{
   c_tick(dMACRO);
   fo_comma();
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
void fo_spat(void)    { fo_dup(); T = CELL(S); }
void fo_spstore(void) { S = PCELL(T); fo_drop(); }
void fo_rpat(void)    { fo_dup(); T = CELL(R); }
void fo_rpstore(void) { R = PCELL(T); fo_drop(); }

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

	for (;;) {
      if (devIN && isatty(fileno(devIN))) {
         fo_cr();
         c_dotr(S[2], 0, BASE);
         c_dotr(S[1], 0, BASE);
         c_dotr(S[0], 0, BASE);
         c_dotr(   T, 0, BASE);
         c_type("> ", -1);
      }
      if (c_refill())
         return;
      for (;;) {
		   c_word(BL);
         if (0 == STR_CNT(cH))
            break;
		   (*staFn)(cH);
      }
	}
}

void c_include(char *path)
{
   IODESC savIN;
   jmp_buf *savERR, errHandler;
   volatile int err;
   FILE * volatile fin;
   char tmp[FILENAME_MAX];
   char buf[NIOBUF+2];

   StrCpy(tmp,path);

   savERR = errENV; errENV = &errHandler; err = 0;
   savIN = currIN;

   if ((err = setjmp(errHandler))) {
      DBG(1,fprintf(stderr,"include: %s\n",tmp));
      goto Lexit;
   }

   fin = fopen(path, "rt");
   if (!fin)
      c_doabort(path,-6);
   io_init(&currIN, fin, buf, 0);
	c_mainloop();

Lexit:
   if (fin)
      fclose(fin);
   currIN = savIN;
   errENV = savERR;
   if (err)
      c_throw(errENV,err);
}
void fo_include(void) { c_word(BL); c_include(CHAR(STR_ADDR(cH))); }
void fo_load(void)
{
   IODESC savIN;
   jmp_buf *savERR, errHandler;
   volatile int err;

   volatile Cell blk;

   savERR = errENV; errENV = &errHandler; err = 0;
   savIN = currIN;

   if ((err = setjmp(errHandler))) {
      DBG(1,fprintf(stderr,"load: %ld\n",blk));
      goto Lexit;
   }

   blk = T; fo_drop();
   io_init(&currIN, NULL, NULL, blk);
   c_mainloop();

Lexit:
   currIN = savIN;
   errENV = savERR;
   if (err)
      c_throw(errENV,err);
}
void c_init_io(void)
{
   io_init(&currIN,  stdin,  conbufIN, 0);
   io_init(&currOUT, stdout, conbufOUT, 0);
}

#define hW     ((UCELL(1) << (sizeof(Cell)*8-1)) - 1)
#define hM     nbuf

UCell hash(UCell K)
{
   UCell WperM = hW / hM;

   K ^= K / WperM;
   return (ha * K) % nbuf;
}

#define BLOCK(x)     ((~CELL_MSB) & (x))
#define UPDATED(x)   (  CELL_MSB  & (x))
#define BLKADDR(x)   (CHAR(origin) + 1024*(x))

char* c_block(Cell blk)
{
   UCell h;
   char *adr;

   if (!origin)
      return 0;
   blk = (blk + OFFSET) % nblk;
   h = hash(blk);
   adr = BLKADDR(h);
   curBLK = h;
   if (BLOCK(blkupd[h]) == blk)
      return adr;
   if (UPDATED(blkupd[h])) {
      Cell oblk = BLOCK(blkupd[h]);
      lseek(fdBLK, 1024 * oblk, SEEK_SET);
      write(fdBLK, adr, 1024);
   }
   lseek(fdBLK, 1024 * blk, SEEK_SET);
   read(fdBLK, adr, 1024);
   blkupd[h] = blk;
   return adr;
}

void fo_nblock(void)
{
   fo_dup(); T = CELL(nblk);
}

void fo_block(void)
{
   T = CELL(c_block(T));
}

void fo_update(void)
{
   if (origin) {
      blkupd[curBLK] |= CELL_MSB;
   }
}

void fo_save(void)
{
   int i;

   if (!origin)
      return;

   for (i = 0; i < nbuf; i++) {
      if (UPDATED(blkupd[i])) {
         Cell oblk = BLOCK(blkupd[i]);
         lseek(fdBLK, 1024LL * oblk, SEEK_SET);
         write(fdBLK, BLKADDR(i), 1024);
         blkupd[i] &= ~CELL_MSB;
      }
   }
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
		{"<lit>",	fo_dolit},     /* runtime */
		{"0branch", fo_0branch},
		{"branch",  fo_branch},
		{"<next>",  fo_donext},
      {"<c\">",   fo_docstr},
		{">r",      fo_tor},
		{"exit",    fo_exit},

		{"@",       fo_fetch},     /* memory */
		{"!",       fo_store},

		{"+",		   fo_add},       /* arithmetic */
		{"-",		   fo_sub},
		{"*",		   fo_mul},
		{"/",		   fo_div},
		{"mod",		fo_mod},
      {"*/",      fo_muldiv},
		{"abs",     fo_abs},
		{"negate",  fo_negate},
		{"min",     fo_min},
		{"max",     fo_max},

		{"<",       fo_less},      /* logic */
		{"=",       fo_equal},
		{">",       fo_greater},
		{"and",     fo_and},
		{"or",      fo_or},
		{"xor",     fo_xor},
      {"invert",  fo_invert},
      {"not",     fo_zequal},

		{"drop",    fo_drop},      /* stack */
		{"dup",     fo_dup},
		{"swap",    fo_swap},
		{"over",    fo_over},

      {".",       fo_dot},       /* io */
      {".r",      fo_dotr},
		{"emit",	   fo_emit},
		{"key",		fo_key},
		{"cr",      fo_cr},
      {"hex",     fo_hex},
      {"decimal", fo_decimal},

		{":",		   fo_colon},     /* defining */
		{"variable",fo_variable},
		{"create",  fo_create},
		{"allot",   fo_allot},
		{",",       fo_comma},

      {"i",       fo_rfetch},    /* control */
		{"empty",   fo_empty},

      {"(",       fo_paren},

		{"bye",     fo_bye},

#ifndef MOORE_INTRO
		{"'",	      fo_tick},
		{"execute", fo_execute},

		{"r>",      fo_rfrom},     /* stack */
		{"r@",	   fo_rfetch},
		{"c@",      fo_cfetch},    /* memory */
		{"c!",      fo_cstore},

      {"0=",      fo_zequal},

		{"constant",fo_constant},
		{"/mod",    fo_divmod},    /* Moore arithmetic */

/* --- START --- */
		{"mark",    fo_mark},
		{"macro",   fo_macro},
		{"forth",   fo_forth},

      {"here",    fo_here},
      {"pad",     fo_pad},
      {"+!",      fo_plusstore},
      {"space",   fo_space},
      {"spaces",  fo_spaces},

		{"?ms",     fo_qms},       /* extensions */
		{"word",    fo_word},
      {"include", fo_include},
      {"move",    fo_move},
      {"fill",    fo_fill},
      {"abort",   fo_abort},
      {"cold",    fo_cold},

		{"does>",	fo_does},

      {"cell",    fo_cell},
      {"cells",   fo_cells},
      {"cell+",   fo_cellplus},

      {"1+",      fo_1plus},
      {"1-",      fo_1sub},
      {"2*",      fo_2star},
      {"2/",      fo_2slash},

      {"type",    fo_type},         /* strings */
      {"count",   fo_count},
      {"place",   fo_place},
      {"append",  fo_append},
      {"-trailing", fo_subtrailing},

      {"(dlopen)",fo_dlopen},       /* foreign functions */
      {"(dlsym)", fo_dlsym},
      {"(callc)", fo_callc},
      {"zcount",  fo_zcount},

      {"block",   fo_block},       /* memory block storage */
      {"update",  fo_update},
      {"save",    fo_save},
      {"load",    fo_load},
      {"/block",  fo_nblock},

      {"user",    fo_user},        /* tasking */
      {"up",      fo_up},
      {"sp@",     fo_spat},
      {"sp!",     fo_spstore},
      {"rp@",     fo_rpat},
      {"rp!",     fo_rpstore},
      {"context", fo_context},     /* context dictionary */
/* --- END --- */
#endif
		{NULL,		0},
	};
	dict_entry_t macros[] = {
		{"if",		fo_if},           /* control */
		{"else",	   fo_else},
		{"then",	   fo_then},
		{"for",     fo_for},
		{"next",    fo_next},
      {"(",       fo_paren},
		{";",		   fo_semi},         /* defining */

#ifndef MOORE_INTRO
		{"begin",	fo_begin},
		{"while",	fo_while},
		{"repeat",	fo_repeat},
		{"until",	fo_until},

      {"c\"",     fo_cstr},
      {"]",       fo_rbracket},
      {"\\",      fo_bkslash},
#endif
		{NULL,		0},
	};

	CONTEXT = &dMACRO;
	for (i = 0; macros[i].nm; i++) {
		d = c_header(BYTE(macros[i].nm));
      d->cfa = macros[i].fn;
	}
	CONTEXT = &dFORTH;
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
	CONTEXT = &dFORTH;

	c_init_io();
}

void fo_cold(void)
{
   if (origin) {
      free(origin); origin = 0;
      close(fdBLK); fdBLK = -1;
   }
   if (M) free(M);
	M = PCELL(malloc(memSize*CELL_SIZE));
	if (0 == M) c_error("not enough memory");
	S0 = M + memSize - DSTACK_SIZE;
	R0 = S0 - RSTACK_SIZE;
	H  = M + 0x100;
   UP = M;
	dFORTH = 0;
	dMACRO = 0;

   BASE   = 10;
   OFFSET = 0;

	c_dict();
	xt_exit    = CELL(c_find(dFORTH,BYTE("EXIT")));
	xt_0branch = CELL(c_find(dFORTH,BYTE("0BRANCH")));
	xt_branch  = CELL(c_find(dFORTH,BYTE("BRANCH")));
	xt_dolit   = CELL(c_find(dFORTH,BYTE("<LIT>")));
	xt_tor     = CELL(c_find(dFORTH,BYTE(">R")));
	xt_donext  = CELL(c_find(dFORTH,BYTE("<NEXT>")));
   xt_docstr  = CELL(c_find(dFORTH,BYTE("<C\">")));

   sobj[nsobj++] = dlopen(NULL,RTLD_LAZY);

   c_init_io();

   fdBLK = open(blkFile, O_RDWR);
   if (fdBLK >= 0) {
      off_t offs = lseek(fdBLK, 0, SEEK_END);
      if (-1 != offs) {
         nblk = offs / 1024;
         if ((origin = malloc(1024 * nbuf + sizeof(Cell) * nbuf))) {
            blkupd = (Cell*)(origin + 1024 * nbuf);
            MemSet(BYTE(blkupd), 0, sizeof(Cell) * nbuf);
            lseek(fdBLK, 0, SEEK_SET);
            c_type("block file ",-1);
            c_type(blkFile,-1);
            fo_cr(); c_flush(1);
         }
      }
   }

	fo_abort();
   fo_mark();
}

void usage()
{
   FPutS("usage: vfc [-b file.blk][-c #buf][-d lvl][-m mem] [include1...]\n", stderr);
   exit(1);
}

int main(int argc, char *argv[])
{
   int i;
   char *str;

	if (PRIME_SIZE > CELL_SIZE) {
		c_error("address size is greater than cell size");
	}
   memSize = 256*1024;
   blkFile = "fo.blk";
   nbuf    = 128;
   UP = dummyUP; BASE = 10;
   for (i = 1; i < argc; i++) {
      str = argv[i];
      if ('-' == *str) {
         switch (str[1]) {
         case 'b':
            i++; blkFile = argv[i];
            break;
         case 'c':
            i++; nbuf = c_tonumber(BYTE(argv[i]));
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


/* vim:set sw=3 ts=3 et: */
