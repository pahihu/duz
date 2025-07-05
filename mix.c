/* MIX
 *
 * 012345
 * +AAIFC
 *
 *  unit time	10us	inexpensive computer
 *		        1us		high-priced machine
 *  this machine has 6us
 *  ADD,SUB,LOAD,STORE,shift,comparisons	2u
 *  MOVE 1u+Nx2u
 *  MUL	 10u
 *  DIV	 12u
 *  FADD  4u
 *  FSUB  4u
 *  FMUL  9u
 *  FDIV 11u
 *  FLOT  3u
 *  FCMP  4u
 *  FIX   3u
 *
 *  TODO
 *  ====
 *  - B^E table
 *  - floating-point attachment
 *
 *  Instruction extensions
 *  ======================
 *  - standard: 1.3.1 pp.124, 1.3.2 pp.141
 *  - binary: AND OR XOR NEG XCH SLB SRB JrE JrO (4.5.2 pp.339 and Ex.2.5-28 pp.455) [f]
 *  - float: FLOT FIX FADD FSUB FMUL FDIV FCMP Vol.2 4.2 pp.214
 *		FLOT pp.221
 *		FIX	 pp.224
 *		FCMP Ex.4.2.2-17 pp.244 (solution pp.615)
 *		FIX  subroutine Ex.4.2.1-14 (solution pp.612)
 *      FP conversion Vol.2 pp.326
 *          f*2^e, 2^e = F*10^E, then convert F*f to decimal
 *          F*10^E, convert F, then multiply by the FP number 10^E
 *  - interrupt: INT Ex.1.4.4-18 pp.228 [d]
 *  - master: XEQ CPMr Ex.1.3.1-25 pp.139 (solution pp.510) [cgh]
 *  - double/indirect-indexing: Ex.2.2.2-5 (solution pp.541) [b]
 *  - real-time clock [e]
 *  
 *
 *  History:
 *  ========
 *  250705AP    B^E table
 *              added DoubleToFP/FPToDouble
 *				float AtomicExpr
 *				fpFIX skeleton
 *  250704AP    fpCMP skeleton, FP_EPSILON
 *              10^E table
 *  250703AP    binary and BYTE shifts
 *              fpMUL, fpDIV skeletons
 *              added MIXTRACE env.var
 *  250702AP    free fmt asm
 *				FP skeletons
 *  250701AP    fixed double-indexing in assembler
 *              refactored logging
 *              refactored options, added MIXCONFIG env.var
 *              added KIPS rating
 *              removed DEC 026/029, MIX/360 is 64-char card code
 *              replaced most shift operations w/ MOD and DIV
 *              negative P/TRANS fmt fixes
 *              negative address fixes in assembler
 *  250630AP    added memory access checking
 *              fixed MOVE, Go button
 *              refactored blkRead/blkWrite
 *              more work on interrupt handling
 *              added MIPS rating
 *              multiply defined asm literals
 *              fixed MUL/DIV timing
 *              added FP routine skeletons + timing + decoding
 *              added DEC 026/029 card codes
 *  250629AP    refactored options, Knuth or Stanford MIX/360 charset
 *              fixed save CORE
 *              added SLB, SRB, JrE, JrO, CPMr
 *              disable nested XEQ
 *              added F check for I/O ops
 *              added Mixmaster: 32KW memory + 64 I/O devices
 *              added double/indirect-addressing
 *				added interrupt handling and real-time clock
 *  250628AP    fixed DEV_TT handling
 *              fixed JNE 'F' error
 *              report undefined syms
 *              added AND, OR, XOR, NEG, XCH, XEQ
 *              added CONFIG options: bcfgimx
 *  250627AP    wait for I/O completion on HLT
 *              asm error handling
 *              fixed LNKLD card
 *              fixed smMUL, smDIV, simplified ENTr, fixed shift
 *  250626AP    fixed smDIV, local symbols, LNKLD card
 *  250624AP    local symbols skeleton
 *  250623AP    literal constants
 * 	250622AP	TT and PT fixes, for shift M should be non-negative
 *  250621AP    assembler debugging
 *  250620AP    more runtime checks
 *              skeleton assembler
 *  250619AP    changed CS/RUN to STATE
 *              added Schedule(), wait on busy device
 *				I/O at half time
 *  250618AP    CS - control state, changed status() display
 *              added disk/drum rotational delay
 *              added stats: nonzero locations, total/idle tyme
 *              added TRACE (4000), TIMER (4001)
 *              added -x TRANS format, reformat error messages
 *  250616AP    paper tape max size (~ 1000')
 *              added teletype
 *              added fictitious paper tape reader (B 141) and punch (B 341)
 *  250615AP    blk_read/write tmp char buffer for conversion
 *              added CR/LF + flush after char I/O out
 *              changed LP_LINES to 66
 *              fixed instr disasm (print A instead of M), added rJ/ZERO as reg[8] and reg[9]
 *              fixed JrN conditions
 *              fixed mem_write() w/ FIELD
 *              fixed unpack()
 *              fixed tape write
 *              revised IOchars, added drum, fixed disk seek, added device close
 *  250613AP    added disasm in trace
 *              changed dev_open() and dev_in/out/ioc order
 *  250612AP    blk_read in char mode, skip CRLF
 *              input record length SHOULD match (no longer or shorter char mode lines!)
 *  250611AP    mix2char was char * instead of char []
 *              to_num() fixed
 *              fixed I/O evt handling
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#if defined(__linux__) || defined(__APPLE__)
#include <sys/time.h>
unsigned CurrentMS(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1000 + (tv.tv_usec + 500) / 1000;
}
#elif defined(WIN32)
#include <sys/types.h>
#include <sys/timeb.h>
unsigned CurrentMS(void)
{
    struct _timeb tb;
    
    _ftime(&tb);
    return tb.time * 1000 + tb.millitm;
}
#else
#include <time.h>
unsigned CurrentMS(void)
{
    return time(NULL) * 1000;
}
#endif

#define IGNORE_VALUE(x) (void)((x)+1)

#if defined(NDEBUG)
# define ASSERT(x)
#else
# define ASSERT(x)  __assert(__LINE__,"fun",x,""#x)
void __assert(int lno, const char *fun, int cond, const char *expr)
{
    if (!cond) {
        fprintf(stderr, "%s:%d: %s failed\n", fun, lno, expr);
        fflush(stderr);
        exit(1);
    }
}
#endif

char *strtolower(char *s)
{
    char *p;
    if ((p = s))
        while ((*p = tolower(*p)))
            p++;
    return s;
}


char *strtoupper(char *s)
{
    char *p;
    if ((p = s))
        while ((*p = toupper(*p)))
            p++;
    return s;
}

#define RANGE(x,lo,hi)  (lo <= (x) && (x) <= hi)
#define MIN(x,y)        ((x) < (y) ? (x) : (y))
#define MAX(x,y)        ((x) > (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))
#define SWAP(x,y)		{ Word tmp = x; x = y; y = tmp; }


typedef unsigned int Word;
typedef unsigned char Byte;
typedef enum {OFF, ON} Toggle;
typedef enum {S_HALT, S_STOP, S_WAIT, S_NORMAL, S_CONTROL} MachineState;
#define MIX_BINARY       1
#define MIX_INTERRUPT    2
#define MIX_FLOAT        4
#define MIX_CORE         8
#define MIX_PUSHGO      16
#define MIX_INDEX       32
#define MIX_MASTER     128
Word CONFIG;

#define CORE_MEM    "core.mem"
#define CORE_CTL    "core.ctl"
int MAX_MEM;
#define ADDR_TRACE  (MAX_MEM+1)
#define TRACE       mem[ADDR_TRACE]
#define TIMER       mem[MAX_MEM+2]

#define ADDR_RTC			10
#define INT_ADDR_RTC		11
#define INT_ADDR_OP			12
#define INT_ADDR_DEV_BASE	20
#define	RTC					ctlmem[ADDR_RTC]

Word reg[10], *mem, *ctlmem, P;
Toggle OT;
enum {LESS, EQUAL, GREATER} CI;
Toggle TRANS, LNKLD, DUMP;
#define CARD_MIX    0
#define CARD_MIX360 1
#define CARD_DEC026 2
#define CARD_DEC029 3
int CARDCODE;
char TRANSNM[5+1];

#define TYME_BASE   6

FILE *LPT;
unsigned Tyme, IdleTyme, InstCount, TraceCount, ElapsedMS;
unsigned short *freq, *ctlfreq;
MachineState STATE, STATESAV;
int WaitRTI;
Toggle CY;
Toggle TRACEOP, TRACEIO, TRACEA;
Toggle XEQTING;

#define rA	 reg[0]
#define rI1	 reg[1]
#define rI2	 reg[2]
#define rI3	 reg[3]
#define rI4  reg[4]
#define rI5  reg[5]
#define rI6  reg[6]
#define rX   reg[7]
#define rJ   reg[8]
#define ZERO reg[9]


void MemWrite(Word a, int f, Word w);
Word MemRead(Word a);
int Stop(void);

#define UNO(dev)    ((dev) - 1)
#define DEVNO(u)    ((u) + 1)

#define DEV_RTC	0
#define	DEV_MT 	1
#define	DEV_DK 	2
#define DEV_DR 	3
#define	DEV_CR 	4
#define	DEV_CP 	5
#define	DEV_LP 	6
#define DEV_TT 	7
#define	DEV_PT 	8

typedef enum {DO_NOTHING, DO_IOC, DO_IN, DO_OUT, DO_BRK} EventType;
int EventH, PendingH;

void Schedule(unsigned delta, int u, EventType what, Word M);
void ScheduleINT(int u);

void devError(int u);
void Usage(void);


/* ============== S M  A R I T H M E T I C ================== */

#define SM_MSB		(1U << 29)
#define SM_NAN      (1U << 30)
#define SM_SIGN		(1U << 31)
#define SM_MASK(x)	((1U << (x)) - 1)
#define A_MASK      SM_MASK(12)
unsigned IX_MASK;
#define SM_NOADDR   (SM_SIGN + 07777U)

#define SIGN(x)		(SM_SIGN & (x))
#define	SM_WORD		SM_MASK(30)
#define MSB(x)		((x) & SM_MSB)
#define MAG(x)		((x) & SM_WORD)

#define MOD         %
#define DIV         /
#define R(x)		((x) MOD 8)
#define L(x)		R((x) DIV 8)
#define FIELD(x,y)	((8 * R(x)) + R(y))
#define	FULL		FIELD(0,5)
#define BYTES		5
#define BYTESIZE    64
#define	BYTE(x)		((x) MOD BYTESIZE)
#define MSBYTE(x)   ((x) & 07700000000U)
#define TRACK(x)    BYTE((x) / BYTESIZE)
#define PLUS(x)     (SIGN(x) ? '-' : '+')
#define ONOFF(x)    ((x) ? "ON ": "OFF")
#define TONOFF(x)  	((x) ? ON : OFF)



/* ===================== L O G G I N G ====================== */

void LogLoc(int severity, const char *fmt, va_list args)
{
    fprintf(stderr, "-%c-MIX: LOC=%c%04o ", severity, PLUS(P), MAG(P));

    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

void Log(int severity, const char *fmt, va_list args)
{
    fprintf(stderr, "-%c-MIX: ", severity);

    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

void DEV_LogLoc(int severity, int u, Word M, const char *fmt, va_list args)
{
    fprintf(stderr, "-%c-MIX: LOC=%c%04o UNO=%02o/%c%04o ", severity, PLUS(P), MAG(P), UNO(u), PLUS(M), MAG(M));

    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

void DEV_Log(int severity, int u, Word M, const char *fmt, va_list args)
{
    fprintf(stderr, "-%c-MIX: UNO=%02o/%c%04o ", severity, UNO(u), PLUS(M), MAG(M));

    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

int MIX_ErrorLoc(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    LogLoc('E', fmt, args);
    va_end(args);

    return Stop();
}

int MIX_Error(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    Log('E', fmt, args);
    va_end(args);

    return Stop();
}

void Error(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    Log('E', fmt, args);
    va_end(args);
}

void DEV_ErrorLoc(int u, Word M, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    DEV_LogLoc('E', u, M, fmt, args);
    va_end(args);

    devError(u);
}

void DEV_Error(int u, Word M, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    DEV_Log('E', u, M, fmt, args);
    va_end(args);

    devError(u);
}

void WarningLoc(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    LogLoc('W', fmt, args);
    va_end(args);
}

void Warning(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    Log('W', fmt, args);
    va_end(args);
}

void InfoLoc(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    LogLoc('I', fmt, args);
    va_end(args);
}

void Info(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    Log('I', fmt, args);
    va_end(args);
}

void TraceIOLoc(const char *fmt, ...)
{
    va_list args;

    if (!TRACEIO)
        return;

    va_start(args, fmt);
    LogLoc('I', fmt, args);
    va_end(args);
}

void TraceIO(const char *fmt, ...)
{
    va_list args;

    if (!TRACEIO)
        return;

    va_start(args, fmt);
    Log('I', fmt, args);
    va_end(args);
}

void TraceA(const char *fmt, ...)
{
    va_list args;

    if (!TRACEA)
        return;

    va_start(args, fmt);
    Log('I', fmt, args);
    va_end(args);
}

void DEV_TraceIOLoc(int u, Word M, const char *fmt, ...)
{
    va_list args;

    if (!TRACEIO)
        return;

    va_start(args, fmt);
    DEV_LogLoc('I', u, M, fmt, args);
    va_end(args);
}

void DEV_TraceIO(int u, Word M, const char *fmt, ...)
{
    va_list args;

    if (!TRACEIO)
        return;

    va_start(args, fmt);
    DEV_Log('I', u, M, fmt, args);
    va_end(args);
}

void Print(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}

void PrintLPT(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    vfprintf(LPT, fmt, args);
    va_end(args);
}


/* ================= L P T  O U T P U T ==================== */

void nl(void)
{
    PrintLPT("\n");
}

void space(void)
{
	PrintLPT(" ");
}

void spaces(int n)
{
	int i;

	for (i = 0; i < n; i++)
		space();
}

void bprint(Byte w)
{
	PrintLPT("%02o ", w);
}

void aprin(Word w)
{
	PrintLPT("%05o", MAG(w));
}

void aprint(Word w)
{
	aprin(w); space();
}

void wprint(Word w)
{
	PrintLPT("%c%010o ", PLUS(w), MAG(w));
}

#define	FP_FMT	"%+12.5E"

void dprint(double d)
{
	char buf[64], *ptr;
	int epart;

	sprintf(buf, FP_FMT, d);
	ptr = strchr(buf, 'E');
	if (ptr)
		*ptr = 0;
	sscanf(ptr + 1, "%d", &epart);
	PrintLPT("%s%+02d ", buf, epart);
}

void xprin(Word w)
{
	PrintLPT("%c%05o", PLUS(w), MAG(w));
}

void xprint(Word w)
{
    xprin(w); space();
}

void aprint4(Word w)
{
	PrintLPT("%c%04o ", PLUS(w), MAG(w));
}

void dprin(int d)
{
    PrintLPT("%d ", d);
}

void cprin(int ch)
{
	PrintLPT("%c", ch);
}

void prin(char *s)
{
    while (*s)
        cprin(*s++);
}



/* ============== S M  A R I T H M E T I C ================== */

Word i2w(int i)
{	int sign;
	
	sign = 0;
	if (i < 0) {
		sign = 1;
		i = -i;
	}
	return (sign ? SM_SIGN : 0) + MAG(i);
}

int w2i(Word w)
{	int i;
	
	i = MAG(w);
	return SIGN(w) ? -i : i;
}

Word field(Word w, int f)
{
	int sign, l, r;
	Word v;
	
	v = w; l = L(f); r = R(f);
	sign = 0;
	if (0 == l)
		sign = ++l;
	v = SM_MASK(6 * (r - l + 1)) & (v >> (6 * (5 - r)));
	if (sign && (SIGN(w)))
		v |= SM_SIGN;
	return v;
}

Word smNEG(Word x)
{
	return SIGN(x) ? MAG(x) : SM_SIGN + x;
}

Word smADD(Word x, Word y)
{
	Word z;

	CY=OFF;
	if (SIGN(x) == SIGN(y)) {
		z = MAG(x) + MAG(y);
		if (z > SM_WORD) {
			z = MAG(z); CY = ON;
		}
		return SIGN(x) + z;
	}
	if (SIGN(y))
		SWAP(x, y);
		
	if (MAG(y) >= MAG(x))
		return MAG(y) - MAG(x);
	return SIGN(x) + MAG(x) - MAG(y);
}

void smINC(Word *pw)
{
    *pw = smADD(*pw, 1);
}

Word smSUB(Word x, Word y)
{
	return smADD(x, smNEG(y));
}

void smDEC(Word *pw)
{
    *pw = smSUB(*pw, 1);
}

Word smSLAX(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;
    int signA, signX;
	
    signA = SIGN(a); a = MAG(a);
    signX = SIGN(x); x = MAG(x);

    shmt = shmt MOD 10;
	for (i = 0; i < shmt; i++) {
		sav = MSBYTE(a);
		a = MAG(BYTESIZE * a);
        a += MSBYTE(x) >> (4*6);
		x = MAG(BYTESIZE * x);
		if (circ && sav)
			x += sav >> (4*6);
	}
    a += signA;
    x += signX;

	*pa = a;
	if (px) *px = x;
	return a;
}

Word smSRAX(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;
    Word signA, signX;
	
    signA = SIGN(a); a = MAG(a);
    signX = SIGN(x); x = MAG(x);

    shmt = shmt MOD 10;
	for (i = 0; i < shmt; i++) {
		sav = BYTE(x);
		x /= BYTESIZE;
		x += BYTE(a) << (4*6);
		a /= BYTESIZE;
		if (circ && sav)
			a += sav << (4*6);
	}

    a += signA;
    x += signX;

	*pa = a;
	if (px) *px = x;
	return a;
}


Word smSLB(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;
    Word signA, signX;
    
    signA = SIGN(a); a = MAG(a);
    signX = SIGN(x); x = MAG(x);
	
    shmt = shmt MOD 60;
	for (i = 0; i < shmt; i++) {
		sav = MSB(a);
		a = MAG(2 * a);
		if (MSB(x))
			a++;
		x = MAG(2 * x);
		if (circ && sav)
			x++;
	}
    a += signA;
    x += signX;

	*pa = a;
	if (px) *px = x;
	return a;
}

Word smSRB(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;
    Word signA, signX;
	
    signA = SIGN(a); a = MAG(a);
    signX = SIGN(x); x = MAG(x);
	
    shmt = shmt MOD 60;
	for (i = 0; i < shmt; i++) {
		sav = x MOD 2;
		x /= 2;
		if (a MOD 2)
			x += SM_MSB;
		a /= 2;
		if (circ && sav)
			a += SM_MSB;
	}

    a += signA;
    x += signX;

	*pa = a;
	if (px) *px = x;
	return a;
}


#define LO(x)	(077777 & (x))
#define HI(x)	LO((x) >> 15)

/* result sign is both a/x, is algebraic: +/+ or -/- is + */
void smMUL(Word *pa, Word *px, Word a, Word x)
{
	Word ma, mx;
	Word lo, mid1, mid2, hi;
    unsigned cy;
	
	ma = MAG(a); mx = MAG(x); cy = 0;
	/*
		(hA +lA) * (hX + lX) = hA hX + (hA lX + lA hX) + lA lX
	*/
	lo   = LO(ma) * LO(mx);
	mid1 = HI(ma) * LO(mx);
    mid2 = LO(ma) * HI(mx);
	hi   = HI(ma) * HI(mx);

	lo = smADD(lo, LO(mid1) << 15);
	if (CY) cy++;
    lo = smADD(lo, LO(mid2) << 15);
	if (CY) {
        cy++;
        hi = smADD(hi, cy);
    }
	hi = smADD(hi, HI(mid1));
	hi = smADD(hi, HI(mid2));
	if (SIGN(a) != SIGN(x)) {
		hi += SM_SIGN;
		lo += SM_SIGN;
	}
	*pa = hi;
	*px = lo;
}

#define UNDEF	((SM_SIGN + IX_MASK) & rand())

/* sign is a */
void smDIV(Word *pquo, Word *prem, Word a, Word x, Word v)
{
	Word ma, mx, mv;
	int i, d;
	
	if (!MAG(v) || MAG(a) >= MAG(v)) {
	    OT = ON;
		*pquo = UNDEF; if (prem) *prem = UNDEF;
        return;
	}
	ma = MAG(a); mx = MAG(x); mv = MAG(v);
    smSLB(&ma, &mx, ma, mx, 1, 0);
	for (i = 0; i < 30; i++) {
		d = 0;
		if (ma >= mv) {
			d = 1; ma -= mv;
		}
		smSLB(&ma, &mx, ma, mx, 1, 0); mx += d;
	}
    ma = ma DIV 2;
    /* rX: ma - rem, rA: mx - quo */
	if (SIGN(a) != SIGN(v))
		mx += SM_SIGN;

	ma += SIGN(a);
    
	*pquo = mx;
	if (prem) *prem = ma;
}


/* ============== F P  A R I T H M E T I C ================== */

#define FP_B        BYTESIZE
#define FP_P		4
#define FP_ONE		(00100000000U)
#define FP_REPRB	(00001000000U)
#define FP_Q		(BYTESIZE >> 1)

/* 
 * Vol.2 pp. 234.
 * 
 *      e0 = b^(1-p), e >= 2*e0 / (1 - .5e0)^2 ~ 7.62942363516372e-06
 *
 */
#define FP_EPSILON  (000000200U)

void fpFREXP(Word u, int *eq, Word *f)
{
	*f = SIGN(u) + field(u, FIELD(2,5));
	*eq = field(u, FIELD(1,1));
}

double FPToDouble(Word u)
{
    Word f;
    double d;
    int i, sign, e, eq;

    fpFREXP(u, &eq, &f);
    sign = SIGN(f);
    f = MAG(f);

    d = 0.0;
    for (i = 0; i < 4; i++) {
        d += BYTE(f);
        d /= FP_B; f /= BYTESIZE;
    }
    e = eq - FP_Q;
    if (e < 0) {
        for (i = 0; i < -e; i++) {
            d /= FP_B;
        }
    } else {
        for (i = 0; i < e; i++) {
            d *= FP_B;
        }
    }
    return sign ? -d : d;
}

void Approximate(double f, int *fracs)
{
    int j;
    double ipart;
    int inc;

    for (j = 0; j < BYTES; j++) {
        modf(f, &ipart);
        fracs[j] = (int)ipart;
        f = FP_B * (f - ipart);
    }
    /* round */
    inc = 0;
    if (fracs[4] > FP_Q) {
        inc = 1;
    } else if (FP_Q == fracs[4]) {
        if (0 == ((fracs[3] + FP_Q) & 1)) {
            inc = 1;
        }
    }
    for (j = 3; inc && (j >= 0); j--) {
        fracs[j] += inc;
        if (fracs[j] >= FP_B) {
            fracs[j] %= FP_B;
            inc = 1;
        } else {
            inc = 0;
        }
    }
}

Word DoubleToFP(double d)
{
    int i, e, eq, fracs[BYTES];
    int sign;
    Word w;

    sign = d < 0.0;
    d = fabs(d);

    e = 0;
    while (d < 1.0) {
        d *= FP_B; e--;
    }
    while (d >= FP_B) {
        d /= FP_B; e++;
    }
    Approximate(d, fracs); e++;
    eq = e + FP_Q;
    if (eq >= BYTESIZE) {
        Warning("EXPONENT OVERFLOW %d", eq);
    } else if (eq < 0) {
        Warning("EXPONENT UNDERFLOW %d", eq);
    }
    for (i = 0; i < 4; i++) {
        w = BYTESIZE * w + BYTE(fracs[i]);
    }
    w += BYTE(eq) << (4*6);
    w += sign ? SM_SIGN : 0;
    return w;
}

/* assume |f| < b */
Word fpNORM(int e, Word hi_f, Word lo_f)
{
	Word f11, w;

/*N1*/
	if (MAG(hi_f) >= FP_ONE)
		goto N4;
	if (!MAG(hi_f)) {
		e = 0; goto N7;
	}
N2:
	if (MAG(hi_f) >= FP_REPRB)
		goto N5;
/*N3*/
	smSLAX(&hi_f, &lo_f, hi_f, lo_f, 1, 0);
	e--;
	goto N2;
N4: smSRAX(&hi_f, &lo_f, hi_f, lo_f, 1, 0);
	e++;
N5:	f11 = field(lo_f, FIELD(1,1));
    if (FP_Q < f11) {
		smINC(&hi_f);
	} else if (FP_Q == f11) {
        if ((MAG(hi_f) + FP_Q) MOD 2 == 0)
            smDEC(&hi_f);
    }
	lo_f = 0;
	if (FP_ONE == MAG(hi_f))
		goto N4;
/*N6*/
	if (-FP_Q < e) {
		OT = ON; CI = GREATER;
	} else if (e > FP_Q - 1) {
		OT = ON; CI = LESS;
	}
N7:	e += FP_Q;
	w = hi_f + (e << (4*6));
	return w;
}

void uDADD(unsigned *phi, unsigned *plo, unsigned hiX, unsigned loX, unsigned hiY, unsigned loY)
{
    unsigned hiZ, loZ;

    loZ = loX + loY;
    if (loZ > SM_WORD) {
        loZ = MAG(loZ); hiY++;
    }
    hiZ = hiX + hiY;

    *phi = hiZ;
    *plo = loZ;
}

/* |x| >= |y| */
void uDSUB(unsigned *phi, unsigned *plo, unsigned hiX, unsigned loX, unsigned hiY, unsigned loY)
{
    if (loX >= loY) {
        *plo = loX - loY;
    } else {
        *plo = loY + (SM_MSB << 1) - loX;
        hiY++;
    }
    *phi = hiX - hiY;
}

void smDADD(Word *phi, Word *plo, Word hiX, Word loX, Word hiY, Word loY)
{
    Word hiZ, loZ;

	CY=OFF;
	if (SIGN(hiX) == SIGN(hiY)) {
        uDADD(&hiZ, &loZ, MAG(hiX), MAG(loX), MAG(hiY), MAG(loY));
		if (hiZ > SM_WORD) {
			hiZ = MAG(hiZ); CY = ON;
		}
        *phi = SIGN(hiX) + hiZ;
        *plo = SIGN(hiX) + loZ;
        return;
	}
	if (SIGN(hiY)) {
		SWAP(hiX, hiY);
		SWAP(loX, loY);
    }
		
    if (MAG(hiY) > MAG(hiX) || (MAG(hiY) == MAG(hiX) && MAG(loY) >= MAG(loX))) {
        uDSUB(phi, plo, MAG(hiY), MAG(loY), MAG(hiX), MAG(loX));
    }        
    uDSUB(&hiZ, &loZ, MAG(hiX), MAG(loX), MAG(hiY), MAG(loY));
    *phi = SIGN(hiX) + hiZ;
    *plo = SIGN(hiX) + loZ;
}

Word fpADD(Word u, Word v)
{
	Word uf, vf;
	int ue, ve, we;
	Word wf, hi_wf;
	Word w;

    hi_wf = 0;

/*A1.A2*/
	if (MAG(v) < MAG(u))
		SWAP(v, u);
		
	/* u >= v */
	fpFREXP(u, &ue, &uf);
	fpFREXP(v, &ve, &vf);
/*A3*/
	we = ue;
/*A4*/
	if (ue - ve >= FP_P + 2) {
		wf = uf; goto A7;
	}
/*A5*/
	smSRAX(&hi_wf, &wf, vf, 0, ue - ve, 0);
	hi_wf += SIGN(vf);
	smDADD(&hi_wf, &wf, hi_wf, wf, uf, 0);
A7:
	w = fpNORM(we, hi_wf, wf);
	return w;
}

Word fpSUB(Word u, Word v)
{
    return fpADD(u, smNEG(v));
}

Word fpMUL(Word u, Word v)
{
	Word uf, vf;
	int ue, ve, we;
	Word wf, hi_wf;
	Word w;

    hi_wf = 0; wf = 0;
	
	fpFREXP(u, &ue, &uf);
	fpFREXP(v, &ve, &vf);

    we = ue + ve - FP_Q;
    smMUL(&hi_wf, &wf, uf, vf);

    w = fpNORM(we, hi_wf, wf);
    return w;
}

Word fpDIV(Word u, Word v)
{
	Word uf, vf;
	int ue, ve, we;
	Word hi_wf;
    Word hi_uf, lo_uf;
	Word re, w;

    hi_wf = 0;
    hi_uf = lo_uf = 0;
	
	fpFREXP(u, &ue, &uf);
	fpFREXP(v, &ve, &vf);

    we = ue - ve + FP_Q + 1;
    /* b^-1 * uf / vf  */
    smSRAX(&hi_uf, &lo_uf, uf, 0, 1, 0);
    smDIV(&hi_wf, &re, hi_uf, lo_uf, vf);

    w = fpNORM(we, hi_wf, 0);
    return w;
}

Word fpFLOT(Word u)
{
    return fpNORM(FP_Q + 5, u, 0);
}

void fpCMP(Word u, Word v)
{
    Word uf, vf;
    int ue, ve;

    if (!MAG(u) && !MAG(v)) {
        CI = EQUAL;
        return;
    }

    if (SIGN(u) == SIGN(v)) {
	    fpFREXP(u, &ue, &uf);
	    fpFREXP(v, &ve, &vf);
        if (ue < ve) {
            CI = SIGN(u) ? GREATER : LESS;
            return;
        } else if (ue > ve) {
            CI = SIGN(u) ? LESS : GREATER;
            return;
        }
        uf = MAG(uf);
        vf = MAG(vf);
        if (uf < vf) {
            if (uf - vf <= FP_EPSILON) {
                CI = EQUAL;
                return;
            }
            CI = SIGN(u) ? GREATER : LESS;
        } else {
            if (vf - uf <= FP_EPSILON) {
                CI = EQUAL;
                return;
            }
            CI = SIGN(u) ? LESS : GREATER;
        }
    } else if (SIGN(u)) {
        CI = LESS;
    } else {
        CI = GREATER;
    }
}

Word fpFIX(Word u)
{
	Word uf, f11;
	Word a, x;
	int ue, e;

	fpFREXP(u, &ue, &uf);
	e = ue - FP_Q;

	a = SIGN(uf); x = uf;
	smSLAX(&x, NULL, x, 0, 1, 0);
	if (e > 0)
		smSLAX(&a, &x, a, x, e, 0);
	else
		x = 0;
	f11 = field(x, FIELD(1,1));
	if (FP_Q < f11)
		smINC(&a);
    else if (FP_Q == f11) {
	    if ((MAG(a) + FP_Q) MOD 2 == 0) {
			smDEC(&a);
		}
	}
	return a;
}

/* ============== M A C H I N E  S T A T E ================== */

int Stop(void)
{
    STATESAV = STATE;
    STATE = S_STOP;
    return 1;
}

int Halt(void)
{
    STATESAV = STATE;
    STATE = S_HALT;
    return 1;
}

int Running(void)
{
    return (S_HALT != STATE && S_STOP != STATE);
}

unsigned WaitEvt;

void Awake(void)
{
    ASSERT(0 < WaitEvt && WaitEvt <= Tyme);
    ASSERT(S_NORMAL == STATESAV || S_CONTROL == STATESAV);

    TraceIOLoc("AWAKE AT %09u", Tyme);

    STATE = STATESAV;
    STATESAV = S_HALT;
    WaitEvt = 0;
}

void WaitFor(unsigned evt)
{
    ASSERT(evt > Tyme);
    ASSERT(0 == WaitEvt);
    ASSERT(S_HALT == STATESAV);

    TraceIOLoc("WAIT UNTIL %07u", evt);

    WaitEvt = evt;
    STATESAV = STATE;
    STATE = S_WAIT;
}

int IsNormal(void)
{
	return S_NORMAL == STATE;
}

int IsControl(void)
{
	return S_CONTROL == STATE;
}

int IsWaiting(void)
{
    return S_WAIT == STATE;
}

int Halted(void)
{
    return S_HALT == STATE;
}

void SaveSTATE(Word loc)
{
	int i;
	Word w;
	
    ASSERT(CONFIG & MIX_INTERRUPT);
	ASSERT(IsNormal());
	
	for (i = 0; i < 8; i++)
		ctlmem[i + 1] = reg[i];

    /* TBD MIXMASTER */
	w  = MAG(P);       w *= BYTESIZE;
	w += FIELD(OT,CI); w *= (BYTESIZE * BYTESIZE);
	w += MAG(rJ);
	ctlmem[1] = w;

	STATE = S_CONTROL;
	P = loc;
}

void RestoreSTATE(void)
{
	int i;
	Word w;
	Byte f;
	
    ASSERT(CONFIG & MIX_INTERRUPT);
	ASSERT(IsControl());
	
    /* TBD MIXMASTER */
	w  = ctlmem[1];
	rJ = A_MASK & w; w /= BYTESIZE * BYTESIZE;
	f  = BYTE(w);    w /= BYTESIZE;
	OT = L(f); CI = R(f);
	P  = A_MASK & w;

	for (i = 0; i < 8; i++)
		reg[i] = ctlmem[i + 1];

	STATE = S_NORMAL;
    WaitRTI = 1;
}


int CheckBinaryOption(void)
{
    if (0 == (CONFIG & MIX_BINARY)) {
        return MIX_Error("NOT A BINARY MIX");
    }
    return 0;
}

int CheckFloatOption(void)
{
    if (0 == (CONFIG & MIX_FLOAT)) {
        return MIX_Error("NO FLOATING POINT ATTACHMENT INSTALLED");
    }
    return 0;
}

int CheckInterruptOption(void)
{
    if (0 == (CONFIG & MIX_INTERRUPT)) {
        return MIX_Error("NO INTERRUPT FACILITY INSTALLED");
    }
    return 0;
}

int CheckMasterOption(void)
{
    if (0 == (CONFIG & MIX_MASTER)) {
        return MIX_Error("NOT A MIXMASTER");
    }
    return 0;
}


/* =================== M E M O R Y ========================== */


int CheckAddr(Word a, char *msg)
{
	int ret;
	
	ret = 0;
	if ((!(CONFIG & MIX_INTERRUPT) && SIGN(a)) || MAG(a) > MAX_MEM+2) {
		ret = 1;
		if (msg) {
			return MIX_ErrorLoc("M=%c%010o INV.MEMORY ADDRESS %s", PLUS(a), MAG(a), msg);
    	}
	}
    return ret;
}

int CheckMemRead(Word a)
{
    ASSERT(IsNormal() || IsControl());

    if (CheckAddr(a, "MEMORY READ"))
        return 1;

    return 0;
}

int CheckAccess(Word a)
{
    if (SIGN(a) && IsNormal()) {
        return MIX_ErrorLoc("M=%c%010o ILLEGAL ACCESS", PLUS(a), MAG(a));
    }
    return 0;
}

int CheckFetch(Word a)
{
    ASSERT(IsNormal() || IsControl() || IsWaiting());

    if (CheckAccess(a) || CheckAddr(a, "MEMORY FETCH"))
        return 1;

    return 0;
}

Word MemRead(Word a)
{
    Word signA;

	Tyme++; TIMER++;
    signA = SIGN(a); a = MAG(a);
	return signA ? ctlmem[a] : mem[a];
}

int CheckMemWrite(Word a)
{
    ASSERT(IsNormal() || IsControl());

    if (CheckAccess(a) || CheckAddr(a, "MEMORY WRITE"))
        return 1;
    return 0;
}

Word WriteField(Word v, int f, Word w)
{
	int sign, l, r, shmt;
	unsigned mask;
	Word vv;
	
	if (FULL == f)
		return w;
	l = L(f); r = R(f);
    sign = 0;
	if (0 == l)
        sign = ++l;
	mask = SM_MASK(6 * (r - l + 1));
	shmt = 6 * (5 - r);
	vv = mask & w;
	vv <<= shmt; mask <<= shmt;
	if (sign) {
		mask += SM_SIGN;
		vv += SIGN(w);
	}
	return vv + (~mask & v);
}

void MemWrite(Word a, int f, Word w)
{
    Word signA;

    Tyme++; TIMER++;
    signA = SIGN(a); a = MAG(a);
    if (signA) {
        ctlmem[a] = WriteField(ctlmem[a], f, w);
        if (ADDR_RTC == a) {
            Schedule(1000U, DEV_RTC, DO_IOC, 0);
        }
    } else {
        mem[a] = WriteField(mem[a], f, w);
    }
}

int CheckMemMove(Word src, Word dst, int n)
{
	return (CheckAddr(src, "MOVE SOURCE")
        || CheckAddr(dst, "MOVE DEST.")
	    || CheckAddr(smADD(src, n - 1), "MOVE SOURCE END")
	    || CheckAddr(smADD(dst, n - 1), "MOVE DEST.END")) ? 1 : 0;
}

void MemMove(Word src, Word dst, int n)
{
	int i;

	for (i = 0; i < n; i++) {
        MemWrite(dst, FULL, MemRead(src));
        smINC(&dst); smINC(&src);
    }
}

void MemSet(char *s, int ch, int len)
{
    int i;

    s[len] = 0;
    for (i = 0; i < len; i++)
        s[i] = ch;
}




/* ================== I N P U T / O U T P U T =============== */

typedef struct __Device {
    unsigned Prio;      /* fixed INT priority */
    Word IntAddr;       /* fixed INT address  */

	FILE *fd, *fdout;
	unsigned pos;
	unsigned max_pos;
	unsigned evt;       /* busy until */

    EventType what;
    unsigned when;      /* event time */
    unsigned LOC;       /* source loc */
    unsigned M;         /* param      */
    MachineState S;     /* init STATE */

    Toggle pending;     /* pending INT */

    int evtNext, intNext;
} Device;

#define IO_SLOTS    21

/*		UNO									DEVNO
 *   -: RTC									 0
 *   0: MT0 MT1 MT2 MT3 MT4 MT5 MT6 MT7		 1.. 8
 *   8: DK0 DK1 DK2 DK3 DK4 DK5 DR6 DR7		 9..16
 *	16: CR  CP  LP  TT  PT					17..21
 */
int MAX_DEVS;
Device *devs;

/* 
Input/Output
	t	0<=t<8	100
	d	8<=d<15	100
	cr	16		16
	cp	17		16
	lp	18		24
	tt	19		14
    pt  20      14
	
IOC
	t	M=0 rewind
		M<0	skip backward
		M>0	skip forward
	d	M=0 seek to rX
	lp	M=0	page
	pr	M=0	rewind
*/

#define NMIXCHARS   64
#define NASCCHARS   256
/*		                          1         2         3         4         5         6	 */
/*		                0123456789012345678901234567890123456789012345678901234567890123*/
char  knuth_m2a[64+1] = " ABCDEFGHI~JKLMNOPQR|_STUVWXYZ0123456789.,()+-*/=$<>@;:'????????";
char   stan_m2a[64+1] = " ABCDEFGHI~JKLMNOPQR|_STUVWXYZ0123456789.,()+-*/=$<>@;:'\"%&#c!^?";
char dec026_m2a[64+1] = " +-0123456789ABCDEFGHIJKLMNOPQR/STUVWXYZ_=@^'\\?.)]<!:$*[>&;,(\"#%";
char dec029_m2a[64+1] = " &-0123456789ABCDEFGHIJKLMNOPQR/STUVWXYZ:#@'=\"[.<(+^!$*);\\],%_>?";
char *cardcodes[] = { knuth_m2a, stan_m2a, dec026_m2a, dec029_m2a };
char m2a[64+1], cr_m2a[64];
Byte a2m[256], cr_a2m[256];


/* B 200 series device characteristics
 * ===================================
 * drum B 430
 *  3750rpm (?)
 *  2ms transfer time + max. 16ms rotational delay (avg. 8ms)
 *
 * disk B 475
 *  1500rpm, head/track, 100Kcps
 *  5ms transfer time + max. 40ms rotational delay (avg. 20ms)
 *
 * tape B 421
 *  90ips, 200/556cpi, 2400' reel, 18KC/50KC, 320ips high-speed rewind
 *  0.75" gap (417c) ~ 8.3ms
 *  100W requires 500 BCD chars = 0.9" + 0.75" gap = 1.65" @ 556cpi
 *  10' load-point marker ... 14' end-of-reel marker => 28,512" usable
 *
 * card_in B 122
 *  200cpm, 80 column, 450 card hopper
 *
 * card_out B 303
 *  100cpm, 80 column, 800 card stacker
 *
 * printer B 320
 *  475lpm
 *
 * paper tape reader B 141
 *  500cps, 10cpi
 *
 * paper tape punch B 341
 *  100cps, 10cpi
 *
 * console ASR33
 *  10cps, 1000 feet, 10cpi
 *
 */

#define CARD_READER  16
#define MAX_WORD_BLOCK  100
#define MAX_CHAR_BLOCK  24
#define LP_LINES 66

#ifdef WIN32
#define	TXT_APPEND   "at"
#define	TXT_RDONLY   "rt"
#else
#define	TXT_APPEND   "at"
#define	TXT_RDONLY   "rt"
#endif
#define	BIN_RWRITE   "r+b"
#define	BIN_CREATE   "w+b"
#define	TXT_RWRITE   "r+t"
#define	TXT_CREATE   "w+t"
struct {
	char *name;
	char *fam;
    Byte *a2m;
    char *m2a;
	unsigned max_pos;
	unsigned blk_size;
    int cr;
	unsigned in_tyme;
	unsigned out_tyme;
    unsigned rot_tyme;
	unsigned seek_tyme;
} IOchar[] = {
    /*     name         fam     a2m     m2a    max  blk cr     in      out   rot     seek */
	{      NULL,       NULL,   NULL,   NULL,     0,   0, 0,     0,       0,    0,       0 },    /* DEV_RTC */
	{    "tape", BIN_RWRITE,   NULL,   NULL, 17280, 100, 0,  3056,    3056,    0,     859 },    /* DEV_MT */
	{    "disk", BIN_RWRITE,   NULL,   NULL,  4096, 100, 0,   833,     833, 6666,       0 },    /* DEV_DK */
    {    "drum", BIN_RWRITE,   NULL,   NULL,   512, 100, 0,   333,    1333, 2666,       0 },    /* DEV_DR */
	{  "reader", TXT_RDONLY, cr_a2m,   NULL,     0,  16, 0, 50000,       0,    0,       0 },    /* DEV_CR */
	{   "punch", TXT_APPEND,   NULL, cr_m2a,     0,  16, 1,     0,  100000,    0,       0 },    /* DEV_CP */
	{ "printer", TXT_APPEND,   NULL,    m2a,     0,  24, 1,     0,   21054,    0,     833 },    /* DEV_LP */
    {      NULL,       NULL,    a2m,    m2a,     0,  14, 1,     0, 1166667,    0, 1166667 },    /* DEV_TT */
	{   "ptape", TXT_RWRITE,    a2m,    m2a,  1707,  14, 0, 23333,  116667,    0,   23333 },    /* DEV_PT */
};

int devType(int u);
void blkSeek(int u, unsigned pos);
void blkRead(int u, unsigned adr, Byte *cvt);
void blkWrite(int u, unsigned adr, char *cvt);

static char *sio[] = {
	"DOIO.NOP",
	"DOIO.IOC",
	"DOIO.IN",
	"DOIO.OUT",
	"DOIO.BRK"
};

void Schedule(unsigned delta, int u, EventType what, Word M)
{
    int i, p;
    unsigned when;

    ASSERT(0 <= EventH && EventH <= MAX_DEVS);

    ASSERT(0 == devs[u].evtNext);
    ASSERT(DO_NOTHING == devs[u].what);

    /* unit is busy until delta */
    devs[u].evt = Tyme + delta;

    /* do actual I/O at delta/2 */
    when = Tyme + delta / 2;
    devs[u].what = what;
    devs[u].when = when;
    devs[u].LOC = P;
    devs[u].M = M;
    devs[u].S = STATE;

    i = EventH; p = 0;
    while (i && devs[i-1].when <= when) {
    	p = i;
        i = devs[i-1].evtNext;
    }
    if ((0 == EventH) || (EventH == i)) { /* empty or head */
        devs[u].evtNext = EventH;
        EventH = u+1;
    } else if (0 == devs[p-1].evtNext) /* tail */
        devs[p-1].evtNext = u+1;
    else { /* middle */
        devs[p-1].evtNext = u+1;
        devs[u].evtNext = i;
    }
    
    if (TRACEIO) {
	    Info("*********** SCHEDULED I/O ***********");
	    i = EventH;
	    while (i) {
		    p = i-1;
			Info("%07u LOC=%04o DEV=%02o/%04o %s",
                devs[p].when, devs[p].LOC, p, devs[p].M, sio[devs[p].what]);
		    i = devs[p].evtNext;
	    }
	    Info("*************************************");
    }
}

void ScheduleINT(int u)
{
    int i, p;
    unsigned prio;

    ASSERT(CONFIG & MIX_INTERRUPT);

    ASSERT(0 <= PendingH && PendingH <= MAX_DEVS);

    ASSERT(0 == devs[u].intNext);

    devs[u].when = Tyme;
    devs[u].pending = ON;

    prio = devs[u].Prio;

    i = PendingH; p = 0;
    while (i && devs[i-1].Prio <= prio) {
        p = i;
        i = devs[i-1].intNext;
    }
    if ((0 == PendingH) || (PendingH == i)) { /* empty or head */
        devs[u].intNext = PendingH;
        PendingH = u+1;
    } else if (0 == devs[p-1].intNext) /* tail */
        devs[p-1].intNext = u+1;
    else { /* middle */
        devs[p-1].intNext = u+1;
        devs[u].intNext = i;
    }

    if (TRACEIO) {
	    Info("************ PENDING INTS ***********");
	    i = PendingH;
	    while (i) {
		    p = i-1;
			Info("%d DEV=%02o %s %c%05o", devs[p].Prio, p, sio[devs[p].what], PLUS(devs[p].IntAddr), MAG(devs[p].IntAddr));
		    i = devs[p].intNext;
	    }
	    Info("*************************************");
    }
}

void doIO(int u)
{
    Word M, LOC, PSAV;
    int x;
    EventType what;

    ASSERT(EventH != u+1);
    ASSERT(0 == devs[u].evtNext);
    ASSERT(DO_NOTHING != devs[u].what);

    LOC = devs[u].LOC;
    M = devs[u].M;
    x = devType(u);
	what = devs[u].what;

    PSAV = P; P = LOC;
        DEV_TraceIOLoc(u, M, "%09u %s", Tyme, sio[what]);
    P = PSAV;
    	
    devs[u].what = DO_NOTHING;

    switch (what) {
    case DO_NOTHING:
        break;
    case DO_IOC:
        blkSeek(u, M);
        break;
    case DO_IN:
        blkRead(u, M, IOchar[x].a2m);
        break;
    case DO_OUT:
        blkWrite(u, M, IOchar[x].m2a);
        break;
	case DO_BRK:
        if (devs[u].pending) {
            Warning("UNO=%02o PENDING INTERRUPT SINCE %08d", UNO(u), devs[u].when);
        } else {
            ScheduleINT(u);
        }
		break;
    }
    
    if (S_CONTROL == devs[u].S && u && DO_BRK != what) {
	    Schedule(devs[u].evt - Tyme, u, DO_BRK, M);
    }
}

void DoEvents(void)
{
	int u;
	
	while (EventH && devs[EventH-1].when <= Tyme) {
		u = EventH-1;
		EventH = devs[u].evtNext;
		devs[u].evtNext = 0;
		doIO(u);
	}
}

void DoInterrupts(void)
{
	int u;

    if (IsNormal()) {
        if (WaitRTI) {
            WaitRTI--;
        } else if (PendingH) {
            ASSERT(CONFIG & MIX_INTERRUPT);
            u = PendingH - 1;
            ASSERT(devs[u].pending);
            PendingH = devs[u].intNext;
            devs[u].intNext = 0;
		    SaveSTATE(devs[u].IntAddr);
        }
    }
}

int devType(int u)
{
	int ret;

	/* special real-time clock */
	if (DEV_RTC == u)
		return DEV_RTC;
	/* 21 I/O cards in a cabinet, Mixmaster have 3 cabinets */
    u = (u - 1) % IO_SLOTS;
    if (u <= 15) {
        ret = u > 13 ? 2: u / 8;
	} else {
        ret = u - 13;
	}
    return ret + 1;
}

Word Pack(Byte *buf)
{
	Word w;
	int i;
	
	w = 0;
	for (i = 0; i < BYTES; i++)
		w = (w * BYTESIZE) + buf[i];
	return w;
}


void UnPack(Word w, Byte *buf)
{
	int i;
	
	for (i = BYTES; i >= 1; i--) {
		buf[i - 1] = BYTE(w);
		w /= BYTESIZE;
	}
}

#define DT_STUCK    ((unsigned)-1)

void devError(int u)
{
	devs[u].evt = DT_STUCK;
}

int devStuck(int u)
{
    return DT_STUCK == devs[u].evt;
}

int devBusy(int u)
{
    if (IsWaiting()) {
        if (Tyme >= WaitEvt)
            Awake();
    }
    return (DT_STUCK == devs[u].evt) || (Tyme < devs[u].evt);
}

int IsCRLF(int ch)
{
	return '\r' == ch || '\n' == ch;
}

int StripCRLF(char *s, int n)
{
    while (n && IsCRLF(s[n-1])) {
        s[n-1] = ' '; n--;
    }
    return n;
}

void blkRead(int u, Word adr, Byte *cvt)
{
	unsigned ret, n;
	int i, j, x;
    Word mar, w, iobuf[MAX_WORD_BLOCK];
    char tmp[MAX_CHAR_BLOCK * BYTES + 1];
	unsigned Blk_size;
    char c, *ptr;
	
	x = devType(u);
	Blk_size = IOchar[x].blk_size;
	
	devs[u].pos++;
    ASSERT(Blk_size <= MAX_WORD_BLOCK);
	if (NULL == cvt) {
		ret = fread(&iobuf[0], sizeof(Word), Blk_size, devs[u].fd);
		if (ret != Blk_size)
			goto ErrOut;
        mar = adr;
        for (i = 0; i < Blk_size; i++) {
            MemWrite(mar, FULL, iobuf[i]); smINC(&mar);
        }
		return;
	}
	
    /* skip CR/LF */
    c = getc(devs[u].fd);
    while (IsCRLF(c))
        c = getc(devs[u].fd);
    if (!IsCRLF(c)) {
        ungetc(c, devs[u].fd);
    }

	n = Blk_size * BYTES;
    ASSERT(n < MAX_CHAR_BLOCK * BYTES);
    if (DEV_TT == x) {
        MemSet(tmp, ' ', sizeof(tmp)-1);
        ptr = fgets(tmp, sizeof(tmp), devs[u].fd);
        if (!ptr || ferror(devs[u].fd))
            goto ErrOut;
        ret = strlen(tmp); strtoupper(tmp);
        tmp[ret] = ' ';
        while (ret && IsCRLF(tmp[ret-1])) {
            tmp[ret-1] = ' '; ret--;
        }
    } else {
	    ret = fread(tmp, sizeof(char), n, devs[u].fd);
	    if (ferror(devs[u].fd) || ret != n)
		    goto ErrOut;
    }

    mar = adr;
	for (i = 0; i < Blk_size; i++) {
        Byte buf[BYTES];
        for (j = 0; j < BYTES; j++)
            buf[j] = cvt[(int) tmp[i * BYTES + j]];
		w = Pack(buf);
        MemWrite(mar, FULL, w); smINC(&mar);
	}
	return;
ErrOut:
	DEV_ErrorLoc(u, adr, "BLKREAD FAILED");
}

void blkDump(int u, Word adr, Toggle bytes)
{
    int i, j, x;
    unsigned Blk_size;
    Word mar, w;

	x = devType(u);
	Blk_size = IOchar[x].blk_size;

    Info("UNO=%02o/%04o BLOCK: ", UNO(u), adr);
    if (bytes) {
	    mar = adr;
	    for (i = 0; i < Blk_size; i++) {
            Byte tmp[BYTES];
            w = MemRead(mar); smINC(&mar);
		    UnPack(w, tmp);
	        for (j = 0; j < BYTES; j++) {
                bprint(tmp[j]);
            }
	    }
    } else {
        mar = adr;
        for (i = 0; i < Blk_size; i++) {
            w = MemRead(mar); smINC(&mar);
            if (0 == (i + 1) % 10)
                nl();
            wprint(w);
        }
    }
    nl();
}

void blkWrite(int u, unsigned adr, char *cvt)
{
	unsigned ret, n;
	int i, j, x;
    Word mar, w;
    char buf[MAX_CHAR_BLOCK * BYTES + 1 + 1];     /* + CR/LF + NUL */
	unsigned Blk_size;
    FILE *fd;
	
	x = devType(u);
	Blk_size = IOchar[x].blk_size;
    fd = devs[u].fdout ? devs[u].fdout : devs[u].fd;
	
	devs[u].pos++;
	if (NULL == cvt) {
        Word iobuf[MAX_WORD_BLOCK];

        ASSERT(Blk_size <= MAX_WORD_BLOCK);
        mar = adr;
        for (i = 0; i < Blk_size; i++) {
            iobuf[i] = MemRead(mar); smINC(&mar);
        }
		ret = fwrite(&iobuf[0], sizeof(Word), Blk_size, fd);
		if (ret != Blk_size)
			goto ErrOut;
		return;
	}

	n = Blk_size * BYTES;
    ASSERT(n <= MAX_CHAR_BLOCK * BYTES);
    mar = adr;
	for (i = 0; i < Blk_size; i++) {
        Byte tmp[BYTES];
        w = MemRead(mar); smINC(&mar);
		UnPack(w, tmp);
        for (j = 0; j < BYTES; j++) {
            buf[i * BYTES + j] = cvt[(int) tmp[j]];
        }
	}

    if (IOchar[x].cr) {
        buf[n++] = '\n';
    }
    buf[n] = 0;

	ret = fwrite(buf, sizeof(char), n, fd);
    DEV_TraceIO(u, adr, "BLKWRITE BUF='%s'", buf);
	if (ret == n) {
        fflush(fd);
		return;
    }
ErrOut:
	DEV_ErrorLoc(u, adr, "BLKWRITE FAILED");
}


void blkSeek(int u, unsigned pos)
{
	int x;
	
    if (DEV_RTC == u) {
        if (MAG(RTC)) {
            RTC = smSUB(RTC, 1);
            if (MAG(RTC)) {
                Schedule(1000, u, DO_IOC, 0);
            } else {
                ScheduleINT(u);
            }
        }
        return;
    }
	if (pos != devs[u].pos) {
		x = devType(u);
    	if (stdin != devs[u].fd) {
        	unsigned blk_size = IOchar[x].blk_size * (x > DEV_DR ? 5 : sizeof(Word));
	    	fseek(devs[u].fd, pos * blk_size, SEEK_SET);
    	}
		devs[u].pos = pos;
	}
}


int devOpen(int u)
{
	int x;
	char devname[32];
	FILE *fd;
	
	if (NULL != devs[u].fd)
		return 0;
		
	x = devType(u);

    if (DEV_TT == x) {
        devs[u].fd = stdin;
        devs[u].fdout = stdout;
        return 0;
    }
	if (x <= DEV_DR)
		sprintf(devname,"%s%d.img", IOchar[x].name, u);
	else
		strcpy(devname, IOchar[x].name);

	devs[u].evt = 0;
	devs[u].pos = 0;
	devs[u].max_pos = 0;
	fd = fopen(devname, IOchar[x].fam);
    TraceIOLoc("UNO=%02o INIT %s\n", UNO(u), devname);
	if (fd) {
		if (DEV_MT == x || DEV_CR == x) {
			fseek(fd, 0, SEEK_END);
			devs[u].max_pos = ftell(fd) / IOchar[x].blk_size;
			fseek(fd, 0, SEEK_SET);
		}	
	} else {
		/* auto-create tape/disk/drum/ptape */
		if (x <= DEV_DR || x == DEV_PT)
			fd = fopen(devname, BIN_CREATE);
	}
	if (NULL == fd) {
		DEV_Error(u, SM_NOADDR, "%s init failed", devname);
		return 1;
	}
	devs[u].fd = fd;
	return 0;
}


unsigned Diff(unsigned a, unsigned b)
{
    return a > b ? a - b : b - a;
}


unsigned doIOC(int u,int *pM)
{
	int x;
	int new_pos, old_track, new_track;
	int M;
	
	M = *pM;
	
    DEV_TraceIOLoc(u, M, "OP.IOC\n");

	x = devType(u);	
	if (!M) {
		if (DEV_MT == x) {
			M = devs[u].pos;
            new_pos = 0;
		} else if (x <= DEV_DR) {
            new_pos = MAG(rX) % IOchar[x].max_pos;
            new_track = TRACK(new_pos);
            old_track = TRACK(devs[u].pos);
			M = Diff(old_track, new_track);
		} else if (DEV_LP == x) {
			fprintf(devs[u].fd,"\f");
			devs[u].pos %= LP_LINES;
			M = (LP_LINES - devs[u].pos);
            new_pos = 0;
		} else if (x == DEV_PT) {
			M = devs[u].pos;
            new_pos = 0;
		} else
            goto ErrOut;
        *pM = new_pos;
		return IOchar[x].seek_tyme * M;
		
	}
	if (DEV_MT == x) {
		if (M > 0) {
			if (M > devs[u].max_pos - devs[u].pos)
				M = devs[u].max_pos - devs[u].pos;
            new_pos = devs[u].pos + M;
		}
		else {
			M = -M;
			if (M > devs[u].pos)
				M = devs[u].pos;
            new_pos = devs[u].pos - M;
		}
        *pM = new_pos;
		return IOchar[x].in_tyme * M;
	}
ErrOut:
	DEV_ErrorLoc(u, M, "OP.IOC UNSUPPORTED");
	return 0;
}


void devIOC(int u, int M)
{
    unsigned delta;

    delta = doIOC(u, &M);
    if (!devStuck(u)) {
		// blkSeek(u, M);
		if (delta)
			Schedule(delta, u, DO_IOC, M);
    }
}


void devINP(int u, Word M)
{
	int x;
	char *errmsg = NULL;
    unsigned delta;
	
	x = devType(u);
    if (CheckAddr(M, "IN BUFFER")
        || CheckAddr(M + IOchar[x].blk_size, "IN BUFFER END"))
    {
        Stop();
        return;
    }
	
	M = MAG(M);
	delta = 0;
	
	DEV_TraceIOLoc(u, M, "OP.IN");

	if (DEV_MT == x) {
		if (devs[u].pos >= devs[u].max_pos) {
			errmsg = "EOT"; goto ErrOut;
		}
	} else if (x <= DEV_DR) {
		int new_pos = 0;
		delta += doIOC(u, &new_pos);
		if (!devStuck(u)) {
			if (delta)
				blkSeek(u, new_pos);
            delta += IOchar[x].rot_tyme * Diff(Tyme MOD BYTESIZE, BYTE(rX)) / (double)BYTESIZE;
    	}
	} else if (x != DEV_CR && x != DEV_PT && x != DEV_TT) {
		errmsg = "UNSUPPORTED"; goto ErrOut;
	}
	if (!devStuck(u)) {
    	// blkRead(u, MAG(M), cvt);
    	delta += IOchar[x].in_tyme;
    	Schedule(delta, u, DO_IN, M);		
	}
	return;
ErrOut:
	DEV_ErrorLoc(u, M, "OP.IN %s", errmsg);
}


void devOUT(Word u,Word M)
{
	int x;
	char *errmsg = NULL;
    unsigned delta;
	
	x = devType(u);
	if (CheckAddr(M, "OUT BUFFER")
        || CheckAddr(M + IOchar[x].blk_size, "OUT BUFFER END"))
    {
        Stop();
        return;
    }
	
	M = MAG(M);
	delta = 0;

	DEV_TraceIOLoc(u, M, "OP.OUT");

	if (DEV_MT == x) {
		if (devs[u].pos >= IOchar[x].max_pos) {
			errmsg = "MAG.TAPE FULL"; goto ErrOut;
		}
	    devs[u].max_pos = devs[u].pos;
	} else if (x <= DEV_DR) {
		int new_pos = 0;
		delta += doIOC(u, &new_pos);
		if (!devStuck(u)) {
			if (delta)
				blkSeek(u, new_pos);
            delta += IOchar[x].rot_tyme * Diff(Tyme MOD BYTESIZE, BYTE(rX)) / (double)BYTESIZE;
        }
	} else if (DEV_CR < x) {
        if (x == DEV_PT) {
		    if (devs[u].pos >= IOchar[x].max_pos) {
			    errmsg = "PAPER TAPE FULL"; goto ErrOut;
		    }
        }
	} else {
		errmsg = "UNSUPPORTED"; goto ErrOut;
	}
	if (!devStuck(u)) {
    	// blkWrite(u, M, cvt);
    	delta += IOchar[x].out_tyme;
    	Schedule(delta, u, DO_OUT, M);
	}
	return;
ErrOut:
	DEV_ErrorLoc(u, M, "OP.OUT %s", errmsg);
}

#define MM(c,f)	(((f) * BYTESIZE) + (c))
static struct {
	char *nm;
	int  c0de;
} opcodes[] = {
	{"NOP ", MM(00,05)},
	{"ADD ", MM(01,05)},
	{"SUB ", MM(02,05)},
	{"MUL ", MM(03,05)},
	{"DIV ", MM(04,05)},

	{"FADD", MM(01,06)},
	{"FSUB", MM(02,06)},
	{"FMUL", MM(03,06)},
	{"FDIV", MM(04,06)},

	{"NUM ", MM(05,00)},
	{"CHAR", MM(05,01)},
	{"HLT ", MM(05,02)},
	{"AND ", MM(05,03)},
	{"OR  ", MM(05,04)},
	{"XOR ", MM(05,05)},
	{"FLOT", MM(05,06)},
	{"FIX ", MM(05,07)},
	{"NEG ", MM(05,010)},
	{"INT ", MM(05,011)},
	{"XCH ", MM(05,012)},
	{"XEQ ", MM(05,013)},

	{"SLA ", MM(06,00)},
	{"SRA ", MM(06,01)},
	{"SLAX", MM(06,02)},
	{"SRAX", MM(06,03)},
	{"SLC ", MM(06,04)},
	{"SRC ", MM(06,05)},
	{"SLB ", MM(06,06)},
	{"SRB ", MM(06,07)},

	{"MOVE", MM(07,00)},

	{"LDA ", MM(010,05)},
	{"LD1 ", MM(011,05)},
	{"LD2 ", MM(012,05)},
	{"LD3 ", MM(013,05)},
	{"LD4 ", MM(014,05)},
	{"LD5 ", MM(015,05)},
	{"LD6 ", MM(016,05)},
	{"LDX ", MM(017,05)},

	{"LDAN", MM(020,05)},
	{"LD1N", MM(021,05)},
	{"LD2N", MM(022,05)},
	{"LD3N", MM(023,05)},
	{"LD4N", MM(024,05)},
	{"LD5N", MM(025,05)},
	{"LD6N", MM(026,05)},
	{"LDXN", MM(027,05)},

	{"STA ", MM(030,05)},
	{"ST1 ", MM(031,05)},
	{"ST2 ", MM(032,05)},
	{"ST3 ", MM(033,05)},
	{"ST4 ", MM(034,05)},
	{"ST5 ", MM(035,05)},
	{"ST6 ", MM(036,05)},
	{"STX ", MM(037,05)},

	{"STJ ", MM(040,02)},
	{"STZ ", MM(041,05)},
	{"JBUS", MM(042,00)},
	{"IOC ", MM(043,00)},
	{"IN  ", MM(044,00)},
	{"OUT ", MM(045,00)},
	{"JRED", MM(046,00)},

	{"JMP ", MM(047,00)},
	{"JSJ ", MM(047,01)},
	{"JOV ", MM(047,02)},
	{"JNOV", MM(047,03)},
	{"JL  ", MM(047,04)},
	{"JE  ", MM(047,05)},
	{"JG  ", MM(047,06)},
	{"JGE ", MM(047,07)},
	{"JNE ", MM(047,010)},
	{"JLE ", MM(047,011)},
	
	{"JAN ", MM(050,00)},
	{"J1N ", MM(051,00)},
	{"J2N ", MM(052,00)},
	{"J3N ", MM(053,00)},
	{"J4N ", MM(054,00)},
	{"J5N ", MM(055,00)},
	{"J6N ", MM(056,00)},
	{"JXN ", MM(057,00)},
	
	{"JAZ ", MM(050,01)},
	{"J1Z ", MM(051,01)},
	{"J2Z ", MM(052,01)},
	{"J3Z ", MM(053,01)},
	{"J4Z ", MM(054,01)},
	{"J5Z ", MM(055,01)},
	{"J6Z ", MM(056,01)},
	{"JXZ ", MM(057,01)},

	{"JAP ", MM(050,02)},
	{"J1P ", MM(051,02)},
	{"J2P ", MM(052,02)},
	{"J3P ", MM(053,02)},
	{"J4P ", MM(054,02)},
	{"J5P ", MM(055,02)},
	{"J6P ", MM(056,02)},
	{"JXP ", MM(057,02)},

	{"JANN", MM(050,03)},
	{"J1NN", MM(051,03)},
	{"J2NN", MM(052,03)},
	{"J3NN", MM(053,03)},
	{"J4NN", MM(054,03)},
	{"J5NN", MM(055,03)},
	{"J6NN", MM(056,03)},
	{"JXNN", MM(057,03)},
	
	{"JANZ", MM(050,04)},
	{"J1NZ", MM(051,04)},
	{"J2NZ", MM(052,04)},
	{"J3NZ", MM(053,04)},
	{"J4NZ", MM(054,04)},
	{"J5NZ", MM(055,04)},
	{"J6NZ", MM(056,04)},
	{"JXNZ", MM(057,04)},

	{"JANP", MM(050,05)},
	{"J1NP", MM(051,05)},
	{"J2NP", MM(052,05)},
	{"J3NP", MM(053,05)},
	{"J4NP", MM(054,05)},
	{"J5NP", MM(055,05)},
	{"J6NP", MM(056,05)},
	{"JXNP", MM(057,05)},

	{"JAE ", MM(050,06)},
	{"J1E ", MM(051,06)},
	{"J2E ", MM(052,06)},
	{"J3E ", MM(053,06)},
	{"J4E ", MM(054,06)},
	{"J5E ", MM(055,06)},
	{"J6E ", MM(056,06)},
	{"JXE ", MM(057,06)},

	{"JAO ", MM(050,07)},
	{"J1O ", MM(051,07)},
	{"J2O ", MM(052,07)},
	{"J3O ", MM(053,07)},
	{"J4O ", MM(054,07)},
	{"J5O ", MM(055,07)},
	{"J6O ", MM(056,07)},
	{"JXO ", MM(057,07)},

	{"INCA", MM(060,00)},
	{"INC1", MM(061,00)},
	{"INC2", MM(062,00)},
	{"INC3", MM(063,00)},
	{"INC4", MM(064,00)},
	{"INC5", MM(065,00)},
	{"INC6", MM(066,00)},
	{"INCX", MM(067,00)},

	{"DECA", MM(060,01)},
	{"DEC1", MM(061,01)},
	{"DEC2", MM(062,01)},
	{"DEC3", MM(063,01)},
	{"DEC4", MM(064,01)},
	{"DEC5", MM(065,01)},
	{"DEC6", MM(066,01)},
	{"DECX", MM(067,01)},
	
	{"ENTA", MM(060,02)},
	{"ENT1", MM(061,02)},
	{"ENT2", MM(062,02)},
	{"ENT3", MM(063,02)},
	{"ENT4", MM(064,02)},
	{"ENT5", MM(065,02)},
	{"ENT6", MM(066,02)},
	{"ENTX", MM(067,02)},

	{"ENNA", MM(060,03)},
	{"ENN1", MM(061,03)},
	{"ENN2", MM(062,03)},
	{"ENN3", MM(063,03)},
	{"ENN4", MM(064,03)},
	{"ENN5", MM(065,03)},
	{"ENN6", MM(066,03)},
	{"ENNX", MM(067,03)},

	{"CPMA", MM(060,04)},
	{"CPM1", MM(061,04)},
	{"CPM2", MM(062,04)},
	{"CPM3", MM(063,04)},
	{"CPM4", MM(064,04)},
	{"CPM5", MM(065,04)},
	{"CPM6", MM(066,04)},
	{"CPMX", MM(067,04)},

	{"CMPA", MM(070,05)},
	{"CMP1", MM(071,05)},
	{"CMP1", MM(072,05)},
	{"CMP1", MM(073,05)},
	{"CMP1", MM(074,05)},
	{"CMP1", MM(075,05)},
	{"CMP1", MM(076,05)},
	{"CMPX", MM(077,05)},

	{"FCMP", MM(070,06)},

    {  NULL, MM(000,00)},
};


#define MAX_LINE 80

char mnemo[5];      /* mnemonic */

int LNO;            /* line no */
char LINE[MAX_LINE + 1];  /* curr.line */
char *LOCATION, *OP, *ADDRESS;
int CH;             /* curr.char */
char *PLN;          /* line ptr */
Toggle E;           /* error? */
char EC[4 + 1];		/* error codes */
int NE;				/* #error codes */
char FREF;			/* future ref. */
Toggle FF;          /* free fmt. */
char *stok[] = { "ERR", "LOC", "NUM", "FLT", "SYM"};
enum {TOK_ERR, TOK_MTY, TOK_LOC, TOK_NUM, TOK_FLT, TOK_SYM} T; /* token type */
char S[10 + 1];     /* parsed symbol */
Word N;             /* parsed number */
int  B;             /* binary op */
int  SX;            /* TOK_SYM FindSym() result */
Toggle UNDSYM;      /* undefined symbol */


#define ENSURE(ch)  \
    if (ch != CH) { \
        return AsmError(EA_INVCHR); \
    } \
    NEXT();


/* address has wrong syntax */
#define EA_ADRSYN	'A'
/* backward local symbol undefined */
#define EA_UNDBCK	'B'
/* invalid char */
#define EA_INVCHR	'C'
/* duplicate location symbol def */
#define EA_DUPSYM	'D'
/* END has non-blank location */
#define EA_ENDLOC	'E'
/* invalid field spec (3:2) */
#define EA_INVFLD	'F'
/* symbol/const/literal len */
#define EA_MAXLEN	'L'
/* missing operand (binop) */
#define EA_MISSOP	'M'
/* invalid number */
#define EA_INVNUM	'N'
/* unknown opcode */
#define EA_UNKOPC	'O'
/* location out of range */
#define EA_INVORG	'R'
/* invalid symbol (eg. 2H in ADDRESS) */
#define EA_INVSYM   'S'
/* too big F or I spec */
#define EA_TBIGFI	'T'
/* undefined symbol other than address */
#define EA_UNDSYM	'U'
/* overflow during expr eval */
#define EA_OVRFLW	'V'
/* extra operand */
#define EA_XTRAOP	'X'
/* linker error */
#define EA_LINKLD	'9'


#define NSYMS 1500
struct {
    char S[10+1];   /* symbol  */
    Word N;         /* value   */
    Word A;			/* loader addr */
    Toggle D;       /* defined? */
} syms[NSYMS];
int nsyms;          /* no. of syms */

#define NLOAD 1500
Word LOAD[2*NLOAD];
int nload;

Toggle OPEND;       /* "END" ? */
Word START;         /* start addr */

int FieldError(Byte F)
{
    return MIX_ErrorLoc("F=%02o INVALID FIELD", F);
}

int GetV(Word M, Byte F, Word *ret)
{
    if (L(F) > 5 || R(F) > 5 || L(F) > R(F))
        return FieldError(F);
    *ret = field(MemRead(M),F);
    return 0;
}

int LinkLoad(Word adr, Word w)
{
	Word nadr;
	int ret = 0;
	
    Info("LINKLOAD");
	while (SM_NOADDR != adr) {
	    if (GetV(adr, FIELD(0, 2), &nadr)) {
		    ret = 1;
		    break;
	    }
	    MemWrite(adr, FIELD(0, 2), w);
        Print(" %c%04o/%c%04o", PLUS(adr), MAG(adr), PLUS(w), MAG(w));
		if (adr == nadr) {
			ret = 1;
			break;
		}
		adr = nadr;
	}
	Print("\n");
	return ret;
}

void OverPunch(char *buf, int sign, unsigned w);

void PunchLinkLoad(FILE *fout)
{
    int i, j, n;
    char buf[8 + 1];
    Word a, w;

    // LNKLDn  12341234...
    for (i = 0; i < nload; i += 18) {
        n = MIN(nload - i, 18);
        fprintf(fout, "LNKLD%d  ", n / 2);
        for (j = 0; j < 18; j += 2) {
            if (j < n) {
                a = LOAD[i + j]; w = LOAD[i + j + 1];
                sprintf(buf, "%04d%04d", a, w);
                OverPunch(buf + 3, SIGN(a), MAG(a));
                OverPunch(buf + 7, SIGN(w), MAG(w));
                fprintf(fout, "%s", buf);
                LinkLoad(a, w);
            }
            else
                fprintf(fout, "        ");
        }
        fprintf(fout, "\n");
    }
}



/* ==================== A S S E M B L E R =================== */

void NEXT(void)
{
    ASSERT( CH );
    CH = *PLN++;
}

int IsDigit(int ch)
{
    return RANGE(ch,'0','9');
}

int IsAlpha(int ch)
{
    return RANGE(ch,'A','Z') || ('~' == ch);
}

int AsmError(char e)
{
	if (NE < 4)
		EC[NE++] = e;
    E = ON; T = TOK_ERR;
    return 0;
}

int GetSym(void)
{
    int i, n, d, isnum, isfloat, savn;
    char *SBEG, *SEND;

    if (CH && '*' == CH) {
        NEXT();
        T = TOK_LOC;
        return 0;
    }
    n = 0;
    isnum = 1; isfloat = 0; SBEG = SEND = NULL;
    MemSet(S, ' ', sizeof(S)-1);
    while (CH && ((d = IsDigit(CH)) || IsAlpha(CH))) {
	    SBEG = PLN-1;
        isnum = isnum && d;
        if (n < 10)
            S[n] = CH;
        n++; NEXT();
    }
    if (isnum && '.' == CH) {
	    isfloat = 1; isnum = 0; NEXT();
	    while (CH && (d = IsDigit(CH))) {
	        if (n < 10)
	            S[n] = CH;
	        n++; NEXT();
	    }
	    if ('E' == CH) {
            NEXT();
            if ('+' == CH || '-' == CH) {
                NEXT();
            } else if (IsDigit(CH)) {
                ;
            } else
                AsmError(EA_INVNUM);
            savn = n;
		    while (CH && (d = IsDigit(CH))) {
		        if (n < 10)
		            S[n] = CH;
		        n++; NEXT();
		    }
		    if (n == savn)
                AsmError(EA_INVNUM);
	    }
	    SEND = PLN-1;
    }
    if (!isfloat && n > 9)
        AsmError(EA_MAXLEN);
    if (0 == n)
        T = TOK_MTY;
    else if (isnum) {
        N = 0;
        for (i = 0; i < n; i++)
            N = 10 * N + S[i] - '0';
        T = TOK_NUM;
    } else if (isfloat) {
	    char buf[MAX_LINE+1];
	    double D;

	    strncpy(buf, SBEG, SEND - SBEG + 1);
	    if (1 != sscanf(buf, "%lg", &D))
		    AsmError(EA_INVNUM);
	    N = DoubleToFP(D);
	    T = TOK_FLT;
    } else
        T = TOK_SYM;
    return 0;
}

int FindOp(void)
{
    int i;

    for (i = 0; opcodes[i].nm; i++) {
        if (!strcmp(opcodes[i].nm, OP))
            return i+1;
    }
    return 0;
}

int FindSym(char *S)
{
    int i, found;

    found = 0;
    for (i = 0; i < nsyms; i++) {
        if (!strcmp(syms[i].S, S)) {
            found = i + 1;
            break;
        }
    }
    TraceA("    FIND.SYM='%s' RESULT=%d", S, found);
    if (found)
        TraceA("    N=%d D=%s", w2i(syms[found-1].N), ONOFF(syms[found-1].D));
    return found;
}

int IsLocalSym(char *s)
{
    int ch1, ch2, ch3;

    ch1 = s[0]; ch2 = s[1]; ch3 = s[2];
    return IsDigit(ch1) && ('H' == ch2 || 'B' == ch2 || 'F' == ch2) && (' ' == ch3);
}

int LSYM;	/* last DefineSym() result */
Word LREF;	/* last reference to just defined sym */
int DefineSymIdx(int found, char *S, Word w, Toggle defd)
{
	Toggle islocal;
	Word adr;

	adr = SM_NOADDR;
    if (found) {
        if (NULL == S)
            S = syms[found-1].S;
		islocal = IsLocalSym(S);
        // TAB ARG
        // D 	D	DUPSYM
        // D 	U	ASSERT
        // U 	D	got value
        // U 	U	just another ref in the chain, handle elswhere
        if (syms[found-1].D && defd) {
            syms[found-1].N = w;
            return AsmError(EA_DUPSYM);
        }
        ASSERT(OFF == syms[found-1].D && ON == defd);
	    N = syms[found-1].N;
	    TraceA("    %s '%s' DEFINED %c%05o (CHAIN %c%05o)",
	        	islocal ? " LOCAL.SYM" : "FUTURE.REF", S, PLUS(w), MAG(w), PLUS(N), MAG(N));
        if (islocal) {
	        ASSERT('B' != S[1]);
	        if ('F' == S[1]) {
		       	adr = syms[found-1].N;
	        }
       	} else {
	        LREF = adr = syms[found-1].A = syms[found-1].N;
	        FREF = 'L';
        }
        syms[found-1].N = w;
        syms[found-1].D = ON;
        if (SM_NOADDR != adr) {
	        if (nload >= NLOAD) {
		        Error("LOADER TABLE FULL");
		        return AsmError(EA_LINKLD);
	        }
			LOAD[nload++] = adr;
			LOAD[nload++] = w;
            if (!LNKLD && LinkLoad(adr, w))
                AsmError(EA_LINKLD);
		}
        return 0;
    }
    ASSERT(NULL != S);
    if (NSYMS == nsyms) {
        Error("SYMBOL TABLE FULL");
        return AsmError(EA_DUPSYM);
    }
    TraceA("    N=%d", w2i(w));
    TraceA("    D=%s", ONOFF(defd));
    strcpy(syms[nsyms].S, S);
    syms[nsyms].N = w;
    syms[nsyms].A = SM_NOADDR;
    syms[nsyms].D = defd;
    nsyms++;
    LSYM = nsyms;
    return 0;
}

int DefineSym(char *S, Word w, Toggle defd)
{
    int found;

    TraceA("    DEFINE SYM '%s'", S);
    if ('=' == S[0]) {
        LSYM = found = 0;
    } else {
        LSYM = found = FindSym(S);
    }
    return DefineSymIdx(found, S, w, defd);
}

void InitLocals(char typ)
{
    int i;

    MemSet(S, ' ', sizeof(S)-1);
    for (i = 0; i < 10; i++) {
        S[0] = '0' + i;
        S[1] = typ;
        DefineSym(S, SM_NOADDR, OFF);
    }
}

int ShowLocal(int i)
{
    if (syms[i].D) {
        space(); dprin(i); prin(syms[i].S); space();
        wprint(syms[i].N);
    }
    return syms[i].D;
}

void ShowLocalSyms(char *msg)
{
    int i, d;

    TraceA("**** LOCALS %s ****", msg);
    for (i = 0; i < 10; i++) {
        d  = ShowLocal(i);
        d += ShowLocal(i + 10);
        d += ShowLocal(i + 20);
        if (d) nl();
    }
    TraceA("****************");
}

void InitLocalSyms(void)
{
    InitLocals('B');
    InitLocals('H');
    InitLocals('F');
}

int localSymIdx(char *s, char typ)
{
    int x;

    ASSERT(IsLocalSym(s));

    x = 0;
    switch (typ) {
    case 'B': x = 0; break;
    case 'H': x = 10; break;
    case 'F': x = 20; break;
    default:
        ASSERT(OFF);
    }
    /* result is like FindSym() */
    return 1 + x + s[0] - '0';
}

Word AtomicExpr(void)
{
    int found;
    Word ret = 0;
    Toggle localB;

    UNDSYM = OFF;
    GetSym();
    TraceA("    ATOMICEXPR S='%s' T=%s", S, stok[T]);
    switch (T) {
    case TOK_ERR:
        break;
    case TOK_MTY:
        break;
    case TOK_LOC:
        ret = P;
        break;
    case TOK_NUM:
    case TOK_FLT:
        ret = N;
        break;
    case TOK_SYM:
        localB = OFF;
        if (IsLocalSym(S)) {
            if ('H' == S[1])
                AsmError(EA_INVSYM);
            localB = TONOFF('B' == S[1]);
        }
        SX = found = FindSym(S);
        if (found) {
            if (OFF == syms[found-1].D) {
                if (localB)
                    AsmError(EA_UNDBCK);
                UNDSYM = ON; /* FUTURE.REF */
            } else {
                ret = syms[found-1].N;
            }
        } else {
            if (IsLocalSym(S))
                ASSERT(OFF);
            UNDSYM = ON; /* UNDEFINED */
        }
    }
    TraceA("    ATOMICEXPR=%c%010o", PLUS(ret), MAG(ret));
    return ret;
}

int IsBinOp(void)
{
    if ('+' == CH || '-' == CH || '*' == CH || '/' == CH || ':' == CH) {
        B = CH; NEXT();
        if ((B == '/') && '/' == CH) {
            B = 'D'; NEXT();
        }
        return 1;
    }
    return 0;
}

void TraceAV(Toggle isfloat, const char *msg, char var, Word w)
{
	char buf[32];

	if (!TRACEA)
		return;

	if (isfloat)
		sprintf(buf, FP_FMT, FPToDouble(w));
	else
		sprintf(buf, "%c%010o", PLUS(w), MAG(w));
	Print("    %s%c=%s", msg ? msg : "", var, buf);
}

Word Expr(void)
{
    Word v = 0, w = 0;
    Toggle FLOATARG;

    TraceA("    EXPR '%c%s'", CH, PLN);
    if ('+' == CH || '-' == CH) {
	    int sign = CH;
	    NEXT();
        v = AtomicExpr();
		if (UNDSYM) AsmError(EA_UNDSYM);
		if ('-' == sign)
			v = smNEG(v);
    } else {
        /* future reference? */
        v = AtomicExpr();
    }
    FLOATARG = TONOFF(TOK_FLT == T);
    TraceAV(FLOATARG, NULL, 'V', v);
    while (IsBinOp()) {
		TraceA("    B=%c", B);
		if (UNDSYM) AsmError(EA_UNDSYM);
        w = AtomicExpr();
        if (TOK_MTY == T)
            AsmError(EA_MISSOP);
		TraceAV(TOK_FLT == T, NULL, 'W', w);
		if (FLOATARG && TOK_FLT != T) {
			Warning("FLOAT CONVERSION");
			w = DoubleToFP(w2i(w));
		}
        OT = OFF;
        if (FLOATARG) {
	        switch (B) {
	        case '+': v = fpADD(v, w); break;
	        case '-': v = fpSUB(v, w); break;
	        case '*': v = fpMUL(v, w); break;
	        case '/': v = fpDIV(v, w); break;
	        default: AsmError(EA_INVCHR);
	        }
        } else {
	        switch (B) {
	        case '+': v = smADD(v, w); break;
	        case '-': v = smSUB(v, w); break;
	        case '*': smMUL(&w, &v, w, v); break;
	        case '/': smDIV(&v, NULL, 0, v, MAG(w) ? w : 1);
	        case 'D': /*//*/
                smDIV(&v, NULL, v, 0, MAG(w) ? w : 1);
                break;
	        case ':': v = i2w(8 * w2i(v) + w2i(w)); break;
	        default: break;
	        }
        }
        TraceAV(FLOATARG, "AFTER", 'V', v);
        if (OT)
        	AsmError(EA_OVRFLW);
    }
    TraceAV(FLOATARG, "EXPR", 'V', v);
    return v;
}

Word Apart(void)
{
    Word v = 0;
    char *LBEG = NULL, *LEND = NULL;
    int i;

    if (E) return 0;
    if (CH && ',' != CH && '(' != CH) {
	    if ('=' == CH) {
        	LBEG = PLN-1; NEXT();
            TraceA("    EXPR LBEG=%s", LBEG);
	    }
        v = Expr();
	    if (LBEG) {
	        /* =123= */
	        LEND = PLN-1;
            TraceA("    EXPR LEND=%c%s", CH, LEND);
	        ENSURE('=');
            if (LEND - LBEG <= 10) {
                if (UNDSYM)
                    AsmError(EA_UNDSYM);
		        *LEND = 0;
                MemSet(S, ' ', sizeof(S)-1);
                for (i = 0; i < LEND - LBEG; i++)
                    S[i] = LBEG[i];
                TraceA("    LIT='%s' V=%c%010o", S, PLUS(v), MAG(v));
                UNDSYM = ON; SX = 0;
	        } else
	        	AsmError(EA_MAXLEN);
	    }
        if (UNDSYM) {
            if (SX) {
                /* already defined future.ref */
                v = syms[SX-1].N;
                syms[SX-1].N = P;
            } else {
                /* future.ref */
                DefineSym(S, P, OFF);
                if ('=' == S[0])
                    syms[LSYM-1].A = v;
                v = SM_NOADDR;
            }
            if (!IsLocalSym(S))
                FREF = '/';
        }
    }
    return v;
}

Word Ipart(void)
{
    Word v = 0;

    if (',' == CH) {
        NEXT();
        v = Expr();
    }
    return v;
}

Word Fpart(Byte C, Word F)
{
    Word v = F;

    if ('(' == CH) {
        NEXT();
        v = Expr();
        ENSURE(')');
    }
    if (v != F && !RANGE(C,042,046)) {
	    if (!RANGE(v,0,63))
	        AsmError(EA_TBIGFI);
	    if (L(v) > R(v))
	        AsmError(EA_INVFLD);
    }
    return BYTE(v);
}

Word Wvalue(void)
{
    Word v = 0, w = 0;
    int f;

   	TraceA("    WVALUE");
    w = Expr();
    f = Fpart(0, 05);
    TraceA("    W=%c%010o F=%o", PLUS(w), MAG(w), f);
    v = WriteField(v, f, w);
    TraceA("    V=%c%010o", PLUS(v), MAG(v));
    while (!E && ',' == CH) {
        NEXT();
        w = Expr(); f = Fpart(0, 05);
	    TraceA("    W=%c%010o F=%o", PLUS(w), MAG(w), f);
        v = WriteField(v, f, w);
	    TraceA("    V=%c%010o", PLUS(v), MAG(v));
    }
    TraceA("    WVALUE=%c%010o", PLUS(v), MAG(v));
    return v;
}

char* GetWord(char *dst, char *src, int n, int alf)
{
    int i;

    i = 0;
    while (*src && *src <= 32) {
      src++; i++;
      if (alf && i > 1)
        break;
    }
    for (i = 0; i < n; i++) {
        if (!*src)
            break;
        if (alf || (!alf && *src > 32))
            *dst++ = *src++;
    }
    while (*src && *src > 32)
        src++;
    return src;
}

#define NEEDAWS needs[0]
#define NEEDP   needs[1]
#define NEEDL   needs[2]

void PrintList(char *needs, Word w, Word OLDP, char *line)
{
    int i;
    Word A, I, F, C, INST;

	if ('L' == FREF) {
		NEEDL = 'L'; FREF = ' ';
	}
    Print("%s%c", EC, FREF);
    i = 0;
    if (NEEDAWS) {
	   	if ('A' == NEEDAWS) {
            INST = w; w = MAG(w);
	        C = BYTE(w); w /= BYTESIZE;
	        F = BYTE(w); w /= BYTESIZE;
	        I = BYTE(w); w /= BYTESIZE;
	        A = A_MASK & w; if (SIGN(INST)) A += SM_SIGN;
		   	aprint4(A); bprint(I); bprint(F); bprint(C);
    	} else if ('W' == NEEDAWS) {
	    	wprint(w); i += 3;
    	} else if ('S' == NEEDAWS) {
	    	xprint(START); i += 8;
    	}
    } else i+= 15;
    spaces(i + 1); i = 0;
    if (NEEDP) {
	    xprint(OLDP);
	} else i += 7;
	if (NEEDL) aprint(LREF);
	else i += 6;
	spaces(i + 1);
    Print("|%s\n", line);
}

void DefineLiterals(void)
{
    int i;
    char needs[3];
    Word w;

    NE = 0;
    MemSet(EC, ' ', sizeof(EC)-1);
    FREF = ' ';
    NEEDP = 'P'; NEEDL = 'L';
    for (i = 0; i < nsyms; i++) {
        if (IsLocalSym(syms[i].S))
            continue;
        if ('=' == syms[i].S[0]) {
            ASSERT(!syms[i].D);
            EC[0] = ' ';
            NEEDAWS = 'W';
            w = syms[i].A;
            DefineSymIdx(i+1 /*syms[i].S*/, syms[i].S, P, ON);
            PrintList(needs, w, P, syms[i].S);
            MemWrite(P, FULL, w); smINC(&P);
        } else if (!syms[i].D) {
            EC[0] = EA_UNDSYM;
            NEEDAWS = 0;
            LREF = syms[i].N;
            PrintList(needs, 0, SM_NOADDR, syms[i].S);
        }
    }
    EC[0] = ' ';
}

int XH;
void DefineLocationSym(Word w)
{
    int xb, xf;

    if (IsLocalSym(LOCATION)) {
        if ('H' != LOCATION[1])
            AsmError(EA_INVSYM);
        xb = localSymIdx(LOCATION, 'B');
        XH = localSymIdx(LOCATION, 'H');
        xf = localSymIdx(LOCATION, 'F');
        if (OFF == syms[xb-1].D) {
            syms[xb-1].N = syms[XH-1].N;
            syms[xb-1].D = syms[XH-1].D;
        }
        if (SM_NOADDR != syms[xf-1].N) {
            DefineSymIdx(xf, NULL, w, ON);
            syms[xf-1].N = SM_NOADDR;
            syms[xf-1].D = OFF;
        }
        DefineSymIdx(XH, NULL, w, ON);
    } else
        DefineSym(LOCATION, w, ON);
}

int Assemble(char *line)
{
    Word w = 0, OLDP, A, I, F, C;
    int i, found;
    char needs[3]; /* A|W|S P L */

    if (!FF || '*' == line[0]) {
        strncpy(LINE, line, MAX_LINE);
    } else {
        MemSet(LINE, ' ', MAX_LINE);
        char *ptr = line;
        if (' ' != *ptr) {
            ptr = GetWord(&LINE[0], ptr, 10, 0);
        }
        ptr = GetWord(&LINE[11], ptr, 4, 0);
        ptr = GetWord(&LINE[16], ptr, MAX_LINE - 14, !strncmp(LINE + 11, "ALF ", 4));
    }
    LINE[MAX_LINE] = 0;
    i = MAX_LINE - 1;
    while (i && LINE[i] < 32)
        LINE[i--] = ' ';

    LINE[10] = 0;
    LINE[15] = 0;
    LOCATION = LINE;
    OP = LINE+11;
    ADDRESS = LINE+16;

    if (TRACEA) {
        TraceA("LINE=%04d", LNO);
        TraceA("    LOCATION='%s'", LOCATION);
        TraceA("    OP='%s'", OP);
        TraceA("    ADDRESS='%s'", ADDRESS);
        ShowLocalSyms("BEFORE");
    }
    
    PLN = ADDRESS; CH = 1; NEXT();
    E = OFF;
    NE = 0; MemSet(EC, ' ', sizeof(EC)-1); FREF = ' ';
    NEEDAWS = 0; NEEDP = 'P'; NEEDL = 0;
    XH = 0;

	OLDP = P;
    if ('*' == LOCATION[0]) {
        goto Out;
    }
    if (!strcmp(OP, "EQU ")) {
        w = Wvalue();
        if (' ' != LOCATION[0])
            DefineLocationSym(w);
        NEEDAWS = 'W'; NEEDP = 0;
    } else {
        if (' ' != LOCATION[0])
            DefineLocationSym(P);
        found = FindOp();
        if (found--) {
            C = opcodes[found].c0de; 
            F = BYTE(C / BYTESIZE);
            C = BYTE(C);
            
            A = Apart();
            I = Ipart();
            F = Fpart(C,F);
            if (!RANGE(C,042,046)) {
	            if (!RANGE(I,0,63)) {
	            	I = BYTE(I); AsmError(EA_TBIGFI);
                }
			}
			w  = MAG(A);  w *= BYTESIZE;
			w += BYTE(I); w *= BYTESIZE;
			w += BYTE(F); w *= BYTESIZE;
			w += C;
            w += SIGN(A);
            MemWrite(P, FULL, w);
            smINC(&P);
            NEEDAWS = 'A';
        } else if (!strcmp(OP, "ORIG")) {
	        w = Wvalue();
	        if (CheckAddr(w, NULL)) {
	        	w = 0; AsmError(EA_INVORG);
            }
            P = w;
        }
        else if (!strcmp(OP, "CON ")) {
            MemWrite(P, FULL, w = Wvalue()); smINC(&P);
            NEEDAWS = 'W';
        } else if (!strcmp(OP, "ALF ")) {
            Byte buf[BYTES];
            for (i = 0; i < BYTES; i++) {
                buf[i] = cr_a2m[CH]; NEXT();
            }
            MemWrite(P, FULL, w = Pack(buf)); smINC(&P);
            ADDRESS[5] = 0;
            NEEDAWS = 'W';
        }
        else if (!strcmp(OP, "END ")) {
            DefineLiterals();
            OLDP = P;
            if (SM_NAN == START) {
                w = Wvalue();
                w = START = SIGN(w) + field(w, FIELD(4,5));
            }
            else
                Warning("START ADDRESS IGNORED");
            NEEDAWS = 'S';
            OPEND = ON;
            if (' ' != LOCATION[0])
            	AsmError(EA_ENDLOC);
        } else
            AsmError(EA_UNKOPC);
    }
    if (!E && CH && ' ' != CH) {
	    TraceA("    EXTRA OP ('%c',%d)", CH, CH);
    	AsmError(EA_XTRAOP);
	}
    if (XH) {
        int xb = localSymIdx(LOCATION, 'B');
        syms[xb-1].N = syms[XH-1].N;
        syms[xb-1].D = syms[XH-1].D;
        syms[XH-1].N = SM_NOADDR;
        syms[XH-1].D = OFF;
        XH = 0;
    }
Out:
    if (TRACEA)
        ShowLocalSyms("AFTER");
    PrintList(needs, w, OLDP, line);
    return ON == E;
}

int Asm(const char *nm)
{
    char line[MAX_LINE+1], path[MAX_LINE+1];
    int n, failed;
    FILE *fd;

    strcpy(path, nm);
    fd = fopen(path, "rt");
    strtoupper(path);
    if (NULL == fd) {
        Error("CANNOT OPEN %s", path);
        return 1;
    }
    Info("PROCESSING %s", path);

    nsyms = 0; nload = 0;
    InitLocalSyms();

    failed = 0;
    P = 0; OPEND = OFF;

    LNO = 0; 
    while (!feof(fd)) {
        if (!fgets(line, sizeof(line), fd))
            break;
        LNO++;
        n = strlen(line);
        while (n && IsCRLF(line[n-1]))
            n--;
        line[n] = 0;
        failed += n ? Assemble(line) : 0;
        if (OPEND)
            break;
    }
    fclose(fd);
    Info("ASSEMBLE %s\n", failed ? "FAILED" : "DONE");

    return 0;
}


/* =============== C O N T R O L  S E C T I O N ============= */

void decode(Word C, Word F)
{
    static char *fpops[] = { "FADD", "FSUB", "FMUL", "FDIV" };
	static char *regnames = "A123456X";
	static char *mnemos[] = {
		/*000*/" NOP ADD SUB MUL DIV \010xxx\011xxxMOVE",
		/*001*/" LDr ",
		/*002*/" LDrN",
		/*003*/" STr ",
		/*004*/" STJ STZ JBUSIOC IN  OUT JRED\012xxx",
		/*005*/"\010JrN JrZ JrP JrNNJrNZJrNPJrE JrO ",
		/*006*/"\004INCrDECrENTrENNr",
		/*007*/" CMPr",

 		/*010*/"\014NUM CHARHLT AND OR  XOR FLOTFIX NEG INT XCH XEQ ",
 		/*011*/"\010SLA SRA SLAXSRAXSLC SRC SLB SRB ",
 		/*012*/"\012JMP JSJ JOV JNOVJL  JE  JG  JGE JNE JLE "
	};
	int nf;
	char *s;
	
	mnemo[0] = '\0';
	mnemo[4] = '\0';
	if (6 == F) {
    	if (RANGE(C,1,4)) {
    	    strcpy(mnemo, fpops[C-1]);
    	    return;
	    } else if (56 == C) {
    	    strcpy(mnemo, "FCMP");
    	    return;
	    }
	}
	s = mnemos[C / 8];
	if (' ' == *s) {
		s++;
		if (4 == strlen(s))
			strcpy(mnemo, s);
		else
			strncpy(mnemo, s + 4 * (C MOD 8), 4);
		if (*mnemo < 'A') {
			s = mnemos[(Byte) *mnemo];
			mnemo[0] = '\0';
		}
	}
	if (0 == mnemo[0]) {
		nf = *s++;
		if (F < nf)
			strncpy(mnemo, s + 4 * F, 4);
		else
			sprintf(mnemo, "U%03o", C);
	}
	s = strchr(mnemo, 'r');
	if (s)
		*s = regnames[C MOD 8];
}

void Status(Word P)
{
	static char *sot = " X", *sci = "<=>", *ssta = "HSWN ";
	Word w, INST, A, I, F, C, M, OP;
	int i;
    double d;
    Toggle FLOATOP;
	
	INST = MemRead(P); w = MAG(INST);
	C = BYTE(w); w /= BYTESIZE;
	F = BYTE(w); w /= BYTESIZE;
	I = BYTE(w); w /= BYTESIZE;
	A = A_MASK & w; if (SIGN(INST)) A += SM_SIGN;
	M = I ? smADD(A, reg[I]) : A;
	decode(C, F);
	
	/*
         1         2         3         4         5         6         7         8         9         A         B         C
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
  LOC FREQ   INSTRUCTION  OP    OPERAND     REGISTER A  REGISTER X  RI1   RI2   RI3   RI4   RI5   RI6   RJ  OV CI   TYME
N1234 1234 +1234 56 78 90 CODE +1234567890 +1234567890 +1234567890 +1234 +1234 +1234 +1234 +1234 +1234 +1234 ? ? 1234567
	*/
	if (0 == (TraceCount++ % 31))
		Print("    LOC FREQ   INSTRUCTION  OP    OPERAND     REGISTER A  REGISTER X   RI1    RI2    RI3    RI4    RI5    RI6    RJ  OV CI   TYME\n");
	Print("%c%c%05o %04d ", ssta[STATE], PLUS(P), MAG(P), freq[P] % 9999);
    aprint4(A); bprint(I); bprint(F); bprint(C);
	Print("%s ", mnemo);
    OP = M; /* 1, 2, 3, 4, 8..23, 56..53 */
    FLOATOP=OFF;

    if ((RANGE(C,1,4) && 6 != F)    /*!FADD/FSUB/FMUL/FDIV*/
        || RANGE(C,8,23)
        || (56 == C && 6 != F)      /*!FCMP*/
        || RANGE(C,57,63))
    {
        GetV(M, F, &OP);
        wprint(OP);
    } else if ((RANGE(C,1,4) && 6 == F)  /*FADD/FSUB/FMUL/FDIV*/
        || (56 == C && 6 == F))     /*FCMP*/
    {
        d = FPToDouble(MemRead(M));
        dprint(d);
        FLOATOP=ON;
    } else if (5 == C && 7 == F) {  /*FIX*/
        d = FPToDouble(rA);
        dprint(d);
        FLOATOP=ON;
    } else {
	    xprint(OP);
        Print("     ");
    }
    if (FLOATOP) dprint(d);
	else wprint(rA);
	wprint(rX);
	for (i = 1; i <= 6; i++)
		xprint(reg[i]);

	xprint(rJ);
	Print("%c %c %09u\n", sot[OT], sci[CI], Tyme);
}



int CheckIdx(int x, Toggle cy)
{
    if (OFF != cy) {
        return MIX_ErrorLoc("RI%d OVERFLOW", x);
	} else if (~(SM_SIGN + IX_MASK) & MAG(reg[x])) {
		return MIX_ErrorLoc("RI%d UNDEFINED", x);
	}
    return 0;
}


Word ConvertNum(Word sum, Word w, int *pscale)
{
	int i, scale;
	
	scale = *pscale;
	for (i = 0; i < 5; i++) {
		sum += scale * (BYTE(w) % 10);
		w /= BYTESIZE; scale *= 10;
	}
	*pscale = scale;
	return sum;
}


Word ToNum(Word a, Word x)
{
	Word sum;
	int scale;
	
	scale = 1;
	sum = ConvertNum(  0, x, &scale);
	sum = ConvertNum(sum, a, &scale);
	return sum;
}


Word ConvertChar(Word *pw, Word a)
{
	int i, d, scale;
	Word w;
	
	w = 0; scale = 0;
	for (i = 0; i < 5; i++) {
		d = 30 + (a % 10); a /= 10;
		w |= (d << scale);
		scale += 6;
	}
	*pw = w;
	return a;
}


void ToChar(Word *pa, Word *px, Word a)
{
	a = ConvertChar(px, a);
	a = ConvertChar(pa, a);
}


/* Vol.1. pp.536 Ex.2.2.2.5 */
int EffectiveAddress(Word loc, Word *ea)
{
    Byte I1, I2;
    Word INST, A, I, F, C;
    Word w;

    Word E, H;
    Word L;
    unsigned N;

    H = 0; L = loc; N = 0;
    Tyme--;
A2:
    if (CheckMemRead(L))
        return 1;
    INST = MemRead(L); w = MAG(INST);
	C = BYTE(w); w /= BYTESIZE;
	F = BYTE(w); w /= BYTESIZE;
	I = BYTE(w); w /= BYTESIZE;
	A = A_MASK & w; if (SIGN(INST)) A += SM_SIGN;

    IGNORE_VALUE(F);
    IGNORE_VALUE(C);

    E = A;
    I1 = L(I); I2 = R(I);
    if (0 < I1 && I1 < 7)
        E = smADD(E, reg[I1]);
    if (0 < I2 && I2 < 7)
        H = smADD(H, reg[I2]);
    if (7 == I1 && 7 == I2) {
        N++; H = 0;
    }
/* A3: */
    if (7 == I1 || 7 == I2) {
        L = E; goto A2;
    } else {
        E = smADD(E, H); H = 0;
    }
/* A4: */
    if (N > 0) {
        L = E; N--; goto A2;
    }

    *ea = E;
    return 0;
}


int Step(void)
{
	Word IR, A, I, F, C, M;
	Word w;
	int cond, x;

    if (CheckFetch(P)) {
        return Stop();
	}
	IR = MemRead(P); w = MAG(IR);
	C = BYTE(w); w /= BYTESIZE;
	F = BYTE(w); w /= BYTESIZE;
	I = BYTE(w); w /= BYTESIZE;
	A = A_MASK & w; if (SIGN(IR)) A += SM_SIGN;
	M = A;
    if (I) {
        if (CONFIG & MIX_INDEX) {
            if (EffectiveAddress(P, &M))
                return 1;
        } else {
            if (I > 6) {
                return MIX_ErrorLoc("I=%02o ILLEGAL INDEX", I);
            }
	        M = smADD(A, reg[I]);
        }
        if (~(SM_SIGN + IX_MASK) & M) {
            return MIX_ErrorLoc("M=%c%010o ILLEGAL ADDRESS\n", PLUS(M), MAG(M));
        }
    }
	switch (C) {
	case 0: /*NOP*/
		break;
	case 1: /*ADD*/
	    if (CheckMemRead(M))
	        return 1;
	    if (6 == F) { /*FADD*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpADD(rA, MemRead(M));
    	    Tyme += 2; TIMER += 2;
    	    return FieldError(F);
	    } else {
            if (GetV(M, F, &w)) {
                return 1;
            }
    		w = smADD(rA, w); if (CY) OT = CY;
    		if (!MAG(w)) w += SIGN(rA);
    		rA = w;
		}
		break;
	case 2: /*SUB*/
	    if (CheckMemRead(M))
	        return 1;
	    if (6 == F) { /*FSUB*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpSUB(rA, MemRead(M));
    	    Tyme += 2; TIMER += 2;
    	    return FieldError(F);
	    } else {
            if (GetV(M, F, &w)) {
                return 1;
            }
    		rA = smSUB(rA, w); if (CY) OT = CY;
        }
		break;
	case 3: /*MUL*/
	    if (CheckMemRead(M))
	        return 1;
	    if (6 == F) { /*FMUL*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpMUL(rA, MemRead(M));
    	    Tyme += 7; TIMER += 7;
    	    return FieldError(F);
	    } else {
            if (GetV(M, F, &w)) {
                return 1;
            }
    		smMUL(&rA, &rX, rA, w);
    		Tyme += 8; TIMER += 8;
		}
		break;
	case 4: /*DIV*/
	    if (CheckMemRead(M))
	        return 1;
	    if (6 == F) { /*FDIV*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpADD(rA, MemRead(M));
    	    Tyme += 9; TIMER += 9;
    	    return FieldError(F);
	    } else {
            if (GetV(M, F, &w)) {
                return 1;
            }
    		smDIV(&rA, &rX, rA, rX, w);
    		Tyme += 10; TIMER += 10;
		}
		break;
	case 5:
		{	Word signA, signX;
		
			signA = SIGN(rA); signX = SIGN(rX);
			switch(F){
			case  0: /*NUM*/
				rA = signA + ToNum(rA, rX);
				break;
			case  1: /*CHAR*/
				ToChar(&rA, &rX, MAG(rA));
				rA += signA; rX += signX;
				break;
			case  2: /*HLT*/
				return Halt();
            case  3: /*AND*/
                if (CheckBinaryOption() || CheckMemRead(M))
                    return 1;
                rA = SIGN(rA) + (MAG(rA) & MAG(MemRead(M)));
                break;
            case  4: /*OR*/
                if (CheckBinaryOption() || CheckMemRead(M))
                    return 1;
                rA = SIGN(rA) + (MAG(rA) | MAG(MemRead(M)));
                break;
            case  5: /*XOR*/
                if (CheckBinaryOption() || CheckMemRead(M))
                    return 1;
                rA = SIGN(rA) + (MAG(rA) ^ MAG(MemRead(M)));
                break;
            case  6: /*FLOT*/
                if (CheckFloatOption())
                    return 1;
                rA = fpFLOT(rA);
                Tyme += 2; TIMER += 2;
                return FieldError(F);
            case  7: /*FIX*/
                if (CheckFloatOption())
                    return 1;
                rA = fpFIX(rA);
                Tyme += 2; TIMER += 2;
                return FieldError(F);
            case  8: /*NEG*/
                if (CheckBinaryOption())
                    return 1;
                rA = SIGN(rA) + MAG(~MAG(rA));
                break;
            case  9: /*INT*/
                if (CheckInterruptOption())
                    return 1;
                ASSERT(IsNormal() || IsControl());
                if (IsNormal()) {
	                SaveSTATE(INT_ADDR_OP);
                } else {
	                RestoreSTATE();
                }
                Tyme++;
                return FieldError(F);
            case 10: /*XCH*/
                if (CheckBinaryOption())
                    return 1;
                w = rA; rA = rX; rX = w;
                break;
            case 11: /*XEQ*/
                if (CheckMasterOption())
                    return 1;
                if (XEQTING) {
                    return MIX_ErrorLoc("NESTED XEQ");
                }
                w = P; P = M; XEQTING = ON; Step(); XEQTING = OFF; P = w;
                break;
			default:
				return FieldError(F);
			};
		}
		break;
	case 6:
		if (SIGN(M)) {
		    return MIX_ErrorLoc("ILLEGAL SIGNED M=%c%05o", PLUS(M), MAG(M));
		} else {
			switch(F){
			case 0: /*SLA*/
				smSLAX(&rA, NULL, rA, 0, M, 0);
				break;
			case 1: /*SRA*/
				smSRAX(&rA, NULL, rA, 0, M, 0);
				break;
			case 2: /*SLAX*/
				smSLAX(&rA, &rX, rA, rX, M, 0);
				break;
			case 3: /*SRAX*/
				smSRAX(&rA, &rX, rA, rX, M, 0);
				break;
			case 4: /*SLC*/
				smSLAX(&rA, &rX, rA, rX, M, 1);
				break;
			case 5: /*SRC*/
				smSRAX(&rA, &rX, rA, rX, M, 1);
				break;
            case 6: /*SLB*/
                if (CheckBinaryOption())
                    return 1;
				smSLB(&rA, &rX, rA, rX, M, 0);
                break;
            case 7: /*SRB*/
                if (CheckBinaryOption())
                    return 1;
				smSRB(&rA, &rX, rA, rX, M, 0);
                break;
			default:
				return FieldError(F);
			}
		}
		break;
	case 7: /*MOVE*/
		if (F) {
            if (CheckMemMove(M, rI1, F))
                return 1;
			MemMove(M, rI1, F);
			rI1 = smADD(rI1, F);
			if (CheckIdx(1, CY))
                return 1;
		}
		break;
	case  8: case  9: case 10: case 11: case 12: case 13: case 14: case 15: /*LDr*/
        if (CheckMemRead(M) || GetV(M, F, &w))
            return 1;
		x = C - 8;
		reg[x] = w;
		if ((0 < x && x < 7) && CheckIdx(x, OFF))
            return 1;
		break;
	case 16: case 17: case 18: case 19: case 20: case 21: case 22: case 23: /*LDrN*/
        if (CheckMemRead(M) || GetV(M, F, &w))
            return 1;
		x = C - 16;
		reg[x] = smNEG(w);
		if ((0 < x && x < 7) && CheckIdx(x, OFF))
            return 1;
		break;
	case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31: case 32: case 33: /*STr, STJ, STZ*/
        if (CheckMemWrite(M))
            return 1;
		MemWrite(M, F, reg[C - 24]);
		break;
	case 34: /*JBUS*/
        if (!RANGE(F,0,MAX_DEVS-1))
            return FieldError(F);
        w = DEVNO(F);
        devOpen(w);
		if (devBusy(w)) {
            rJ = P + 1;
			P = M;
            return 0;
		}
		break;
	case 35: /*IOC*/
	case 36: /*IN*/
    case 37: /*OUT*/
        if (!RANGE(F,0,MAX_DEVS-1))
            return FieldError(F);
        w = DEVNO(F);
		if (devOpen(w))
		    return 0;
        if (devBusy(w)) {
            IdleTyme++;
            if (!IsWaiting())
                WaitFor(devs[w].evt);
            return 0;
        }
        ASSERT(!IsWaiting());
        switch (C) {
        case 35: devIOC(w, M); break;
		case 36: devINP(w, M); break;
		case 37: devOUT(w, M); break;
        }
		break;
	case 38: /*JRED*/
        if (!RANGE(F,0,MAX_DEVS-1))
            return FieldError(F);
        w = DEVNO(F);
        devOpen(w);
		if (!devBusy(w)) {
			rJ = P + 1;
			P = M;
            return 0;
		}
		break;
	case 39:
		cond = 0;
		switch (F) {
		case 0: /*JMP*/ cond = 1; break;
		case 1: /*JSJ*/ cond = 1; break;
		case 2: /*JOV*/ if ( ON == OT) { OT = OFF; cond = 1; } break;
		case 3: /*JNOV*/if (OFF == OT) cond = 1; OT = OFF; break;
		case 4: /*JL*/  cond = LESS == CI; break;
		case 5: /*JE*/	cond = EQUAL == CI; break;
		case 6: /*JG*/	cond = GREATER == CI; break;
		case 7: /*JGE*/	cond = GREATER == CI || EQUAL == CI; break;
		case 8: /*JNE*/	cond = EQUAL != CI; break;
		case 9: /*JLE*/ cond = LESS == CI || EQUAL == CI; break;
		default:
			return FieldError(F);
		}
		if (cond) {
			if (1 != F)
				rJ = P + 1;
			P = M;
            return 0;
		}
		break;
	case 40: case 41: case 42: case 43: case 44: case 45: case 46: case 47:
		w = reg[C - 40]; cond = 0;
		switch (F) {
		case 0: /*JrN*/ cond = MAG(w) && SIGN(w); break;
		case 1: /*JrZ*/ cond = !MAG(w); break;
		case 2: /*JrP*/ cond = MAG(w) && !SIGN(w); break;
		case 3: /*JrNN*/cond = !MAG(w) || !SIGN(w); break;
		case 4: /*JrNZ*/cond = MAG(w); break;
		case 5: /*JrNP*/cond = !MAG(w) || SIGN(w); break;
        case 6: /*JrE*/
            if (CheckBinaryOption())
                return 1;
            cond = 0 == (MAG(w) MOD 2); break;
        case 7: /*JrO*/
            if (CheckBinaryOption())
                return 1;
            cond = 1 == (MAG(w) MOD 2); break;
		default:
			return FieldError(F);
		}
		if (cond) {
			rJ = P + 1;
			P = M;
            return 0;
		}
		break;
	case 48: case 49: case 50: case 51: case 52: case 53: case 54: case 55:
		x = C - 48;
		switch(F){
		case 0: /*INCr*/
			reg[x] = smADD(reg[x], M); if (CY) OT = ON;
			if ((0 < x && x < 7) && CheckIdx(x, CY))
                return 1;
			break;
		case 1: /*DECr*/
			reg[x] = smSUB(reg[x], M); if (CY) OT = ON;
			if ((0 < x && x < 7) && CheckIdx(x, CY))
                return 1;
			break;
		case 2: /*ENTr*/
			reg[x] = M;
			break;
		case 3: /*ENNr*/
            reg[x] = smNEG(M);
			break;
        case 4: /*CPMr*/
            if (CheckMasterOption())
                return 1;
		    w = smSUB(reg[x], M);
		    if (SIGN(w)) CI = LESS;
		    else if (!MAG(w)) CI = EQUAL;
		    else CI = GREATER;
            break;
		default:
			return FieldError(F);
		}
		break;
	case 56: case 57: case 58: case 59: case 60: case 61: case 62: case 63: /*CMPr*/
	    if (CheckMemRead(M))
    	    return 1;
	    if (6 == F) { /*FCMP*/
    	    if (CheckFloatOption())
    	        return 1;
            fpCMP(rA, MemRead(M));
    	    Tyme += 2; TIMER += 2;
    	    return FieldError(F);
	    } else {
            if (GetV(M, F, &w)) {
                return 1;
            }
    		w = smSUB(field(reg[C - 56], F), w);
    		if (SIGN(w)) CI = LESS;
    		else if (!MAG(w)) CI = EQUAL;
    		else CI = GREATER;
		}
		break;
	}
    smINC(&P);
	return 0;
}

void Run(Word p)
{
    Word OLDP;
    unsigned evt;
    int u;
    unsigned startMS;

    startMS = CurrentMS();
	P = p; OLDP = smADD(p, 1); STATE = S_NORMAL;
	while (Running()) {
        if (!IsWaiting()) {
            if (TRACE && OLDP != P)
                Status(P);
        }
        OLDP = P;
        Step();
        DoEvents();
        DoInterrupts();
        if (IsWaiting())
            P = OLDP;
        else {
            (SIGN(OLDP) ? ctlfreq : freq)[MAG(OLDP)]++; InstCount++;
        }
	}
    if (Halted()) {
        /* normal HLT, process I/O events */
        while (EventH) {
            DoEvents(); Tyme++; IdleTyme++;
            DoInterrupts();
        }
        /* wait until all devs are ready */
        evt = 0;
        for (u = 0; u < MAX_DEVS; u++)
            if (!devStuck(u))
                evt = MAX(evt, devs[u].evt);
        if (evt > Tyme) {
            evt -= Tyme;
            Tyme += evt; IdleTyme += evt;
        }
    }
    ElapsedMS = CurrentMS() - startMS;
}

void SaveCore(const char *path, Word *adr, int len, const char *msg)
{
	FILE *fd;
	int n;

	if (NULL == adr)
		return;

    fd = fopen(path, BIN_CREATE);
    if (fd) {
        n = fwrite(adr, sizeof(Word), len, fd);
        if (n != len)
            Error("SAVE %s FAILED", msg);
        fclose(fd);
    } else
        Error("CANNOT SAVE %s", msg);
}

void Finish(void)
{
    int i;
    FILE *fd;

    for (i = 0; i < MAX_DEVS; i++) {
        if (stdout == devs[i].fdout)
            fflush(devs[i].fdout);
        fd = devs[i].fd;
        if (NULL != fd && stdin != fd)
            fclose(fd);
    }

    if (CONFIG & MIX_CORE) {
	    SaveCore(CORE_MEM, mem, MAX_MEM + 1, "CORE");
	    SaveCore(CORE_CTL, ctlmem, MAX_MEM + 1, "CONTROL CORE");
    }

    if (ctlfreq) free(ctlfreq);
    if (ctlmem) free(ctlmem);
    if (freq) free(freq);
    if (mem) free(mem);
}

void InitMemory(void)
{
    int i;

    ctlmem = NULL;
    ctlfreq = NULL;
    
    if (CONFIG & MIX_MASTER) {
        IX_MASK = SM_MASK(15); MAX_MEM = 31999;
        MAX_DEVS = 1 + 64;
    } else {
        IX_MASK = SM_MASK(12); MAX_MEM = 3999;
        MAX_DEVS = 1 + 21;
    }
    if (CONFIG & MIX_INTERRUPT) {
	    ctlmem = malloc((MAX_MEM + 1) * sizeof(Word));
    	ctlfreq = malloc((MAX_MEM + 1) * sizeof(unsigned short));
	    if (NULL == ctlmem || NULL == ctlfreq) {
		    goto ErrOut;
	    }
		for (i = 0; i < MAX_MEM + 1; i++) {
			ctlmem[i] = ctlfreq[i] = 0;
		}
    }
    mem  = malloc((MAX_MEM + 3) * sizeof(Word));
    freq = malloc((MAX_MEM + 3) * sizeof(unsigned short));
    devs = malloc(MAX_DEVS * sizeof(Device));
    if (NULL == mem || NULL == freq || NULL == devs) {
        goto ErrOut;
    }
	for (i = 0; i < MAX_MEM + 3; i++) {
		mem[i] = freq[i] = 0;
	}
	return;
ErrOut:
	Error("NOT ENOUGH MEMORY");
	exit(1);
}

void InitMixToAscii(void)
{
    int i;

    strcpy(m2a, cardcodes[CARDCODE]);
    for (i = 0; i < 256; i++) {
        a2m[i] = cr_a2m[i] = 0;
    }

    if (CARD_MIX == CARDCODE) {
    	for (i = 0; i < 49; i++) {
            cr_m2a[i] = m2a[i];
        }
        cr_m2a[20] = cr_m2a[21] = '?';
        for (i = 49; i < 64; i++) {
            cr_m2a[i] = '?';
        }
    }
	for (i = 0; i < 64; i++) {
		a2m[(int) m2a[i]] = i;
		cr_a2m[(int) cr_m2a[i]] = i;
	}
	if (CARD_MIX == CARDCODE) {
        cr_a2m[' '] = 0; a2m[' '] = 0;
        cr_a2m['?'] = 0; a2m['?'] = 0;
    }
}

void LoadCore(const char *path, Word *adr, int len, const char *msg)
{
	FILE *fd;
	int n;

	if (NULL == adr)
        return;

    fd = fopen(path, "rb");
    if (fd) {
        n = fread(adr, sizeof(Word), len, fd);
        if (n != len)
            Warning("LOAD %s FAILED", msg);
        fclose(fd);
    }
}

void InitCore(void)
{
    if (CONFIG & MIX_CORE) {
        LoadCore(CORE_MEM,    mem, MAX_MEM + 1, "CORE");
        LoadCore(CORE_MEM, ctlmem, MAX_MEM + 1, "CONTROL CORE");
    }
}

void InitOptions(void)
{
    CONFIG = 0;
    LPT = stderr;
    TRACEOP = TRACEIO = OFF; TraceCount = 0; TRACEA = OFF;
    TRANS = LNKLD = DUMP = OFF;
    START = SM_NAN;
    TRANSNM[0] = 0;
    XEQTING = OFF;
    FF = OFF;
    CARDCODE = CARD_MIX;
}

void InitConfig(const char *arg)
{
    while (arg && *arg) {
        switch (*arg++) {
        case 'b': CONFIG |= MIX_BINARY; break;
        case 'c': CONFIG |= MIX_CORE; break;
        case 'f': CONFIG |= MIX_FLOAT; break;
        case 'i': CONFIG |= MIX_INTERRUPT; break;
        case 'm': CONFIG |= MIX_MASTER; break;
        case 'x': CONFIG |= MIX_INDEX; break;
        default:
            Usage();
        }
    }
}

void InitTrace(const char *arg)
{
    while (arg && *arg) {
	    switch (*arg++) {
		case 'i': TRACEIO = ON; break;
		case 'o': TRACEOP = ON; break;
		case 'a': TRACEA  = ON; break;
		default:
		    Usage();
		}
    }
}

void Init(void)
{
	int i;

	InstCount = 0;
	Tyme = 0; IdleTyme = 0;
    ZERO = 0;
	STATE = S_HALT; Halt();

    InitMemory();
    atexit(Finish);

    TRACE = TRACEOP;

    /* --- units --- */
	for (i = 0; i < MAX_DEVS; i++) {
        devs[i].Prio = devType(i);
		devs[i].IntAddr = smNEG(i ? INT_ADDR_DEV_BASE + i : INT_ADDR_RTC);
		devs[i].fd = devs[i].fdout = NULL;
        devs[i].what = DO_NOTHING;
        devs[i].pending = OFF;
        devs[i].evtNext = devs[i].intNext = 0;
    }
    EventH = 0;
    PendingH = 0;
    WaitEvt = 0;

    InitCore();
    InitMixToAscii();
}

void Usage(void)
{
	Print("usage: mix [-c bcfimx][-g [unit]][-3][-ad][-s addr][-t aio][-lpr] file1...\n");
    Print("options:\n");
    Print("    -g [unit]  push GO button on unit (def. card reader)\n");
    Print("    -c bcfimx  MIX config (also from MIXCONFIG env.var):\n");
    Print("                 b - binary MIX\n");
    Print("                 c - core memory (core.mem/core.ctl)\n");
    Print("                 f - floating-point attachment\n");
    Print("                 i - interrupt facility\n");
    Print("                 m - Mixmaster\n");
    Print("                 x - double/indirect-indexing facility\n");
    Print("\n");
    Print("    -3         Stanford MIX/360 card codes\n");
    Print("    -a         assemble only\n");
    Print("    -d         dump non-zero locations\n");
    Print("    -l         punch LNKLD cards\n");
    Print("    -p         punch nonzero locations in TRANS fmt\n");
    Print("    -r         free fmt MIXAL\n");
    Print("    -s address set START address\n");
    Print("    -t aio     enable tracing: Asm,Io,Op (also MIXTRACE env.var)\n");
    Print("\n");
	exit(1);
}


void Go(int d)
{
    int x;

    if (devOpen(d))
        return;

    x = devType(d);

	if (d < 0 || d >= MAX_DEVS || DEV_CP == x || DEV_LP == x || DEV_TT == x)
		Usage();
	devINP(d, 0);
	Tyme = devs[d].evt;
	DoEvents();
	Tyme = 0;
	Run(0);
}

void OverPunch(char *buf, int sign, unsigned w)
{
   if (sign)
        *buf = "~JKLMNOPQR"[w MOD 10];
}

void Stats(FILE *fd, int STRIDE, Word dotrans)
{
    int prev_i, i, j, minj, maxj;
    unsigned emit, prev_emit;
    FILE *LPTSAV;
    int MIN_MEM;

    LPTSAV = LPT; LPT = fd;
    if (!dotrans) {
        nl();
        PrintLPT("CONTENTS OF MIX MEMORY (NONZERO LOCATIONS ONLY)\n");
        nl();
        PrintLPT("LOC        ");
        for (i = 0; i < STRIDE; i++)
            PrintLPT("%d           ", i);

        PrintLPT("%*sFREQUENCY  COUNTS\n", 1 + (6*STRIDE - 17) / 2, " ");
    }

    prev_emit = 0; prev_i = 0;
    MIN_MEM = (CONFIG & MIX_INTERRUPT) ? -MAX_MEM : 0;
    for (i = MIN_MEM; i <= MAX_MEM; i += STRIDE) {
        emit = 0; minj = STRIDE; maxj = 0;
        for (j = 0; (i + j <= MAX_MEM) && (j < STRIDE); j++) {
            Word mag = MemRead(i2w(i + j));
            if (mag) {
                if (j < minj)
                    minj = j;
                maxj = j;
            }
            emit += mag;
        }
        if (!emit) {
            prev_emit = 0; prev_i = 0;
            continue;
        }
        if (!dotrans) {
            if (prev_emit == emit)
                continue;
            if (prev_emit && ((prev_i + STRIDE) != i)) {
                Word a_prev_i = i2w(prev_i), a_i_1 = i2w(i - 1);
                PrintLPT("%c%05o..%c%05o   SAME AS ABOVE\n",
                    PLUS(a_prev_i), MAG(a_prev_i), PLUS(a_i_1), MAG(a_i_1));
            }
        }
        prev_i = i;
        prev_emit = emit;
        if (dotrans) {
            char buf[10 + 1];
            int i_minj = i + minj;
            sprintf(buf, "%-5s%d%04d", TRANSNM, maxj - minj + 1, ABS(i_minj));
            OverPunch(buf + 9, i_minj < 0, ABS(i_minj));
            PrintLPT(buf);
            for (j = minj; j < minj + STRIDE; j++) {
                if (j > maxj)
                    strcpy(buf, "0000000000");
            	else {
                    Word w = MemRead(i2w(i + j));
                    sprintf(buf, "%010d", MAG(w));
                    OverPunch(buf + 9, SIGN(w), MAG(w));
                }
                PrintLPT("%s", buf);
            }
        } else {
            int a;
            PrintLPT("%04o: ", i);
            for (j = 0; j < STRIDE; j++) {
                a = i + j;
                if (i + j <= MAX_MEM)
                    wprint(MemRead(i2w(i + j)));
                else
                    PrintLPT("       ");
            }
            PrintLPT("     ");
            for (j = 0; j < STRIDE; j++) {
                a = i + j;
                if (a <= MAX_MEM)
                    PrintLPT(" %05d", (a < 0 ? ctlfreq : freq)[ABS(a)]);
                else
                    PrintLPT("      ");
            }
        }
        nl();
    }
    if (dotrans) {
        char buf[MAX_LINE + 1 + 1];
        Word adr;
        if (LNKLD)
            PunchLinkLoad(LPT);
        adr = smSUB(dotrans, 1);
        sprintf(buf, "TRANS0%04d%*s\n", adr, 70, " ");
        OverPunch(buf + 9, SIGN(adr), MAG(adr));
        PrintLPT(buf);
	}
    LPT = LPTSAV;
}

void PunchTrans(void)
{
    char buf[16];
    FILE *fout;

    if (0 == TRANSNM[0])
        strcpy(TRANSNM, "core");
    strcpy(buf, TRANSNM);
    strcat(buf, ".tra");
    strtolower(buf);
    if ((fout = fopen(buf, TXT_APPEND))) {
        Stats(fout, 7, smADD(START, 1));
        fclose(fout);
    } else
        Error("CANNOT OPEN %s", buf);
}


#define NASMFILES 32

int main(int argc, char*argv[])
{
	int i, u;
	char *arg;
    Toggle ASMONLY;
    char *asmfiles[NASMFILES];
    int nasmfiles;

    InitOptions();
    InitConfig(getenv("MIXCONFIG"));
    InitTrace(getenv("MIXTRACE"));

    ASMONLY = OFF; u = CARD_READER;
    nasmfiles = 0;

	for (i = 1; i < argc; i++) {
		arg = argv[i];
		if ('-' != *arg) {
            if (nasmfiles >= NASMFILES) {
                Error("TOO MANY ASM FILES");
                exit(1);
            }
            asmfiles[nasmfiles++] = arg;
            continue;
        } 
		switch (arg[1]) {
        case '3': CARDCODE = CARD_MIX360; continue;
        /* case '6': CARDCODE = CARD_DEC026; continue; */
        /* case '9': CARDCODE = CARD_DEC029; continue; */
        case 'c':
			if (i + 1 < argc) {
                InitConfig(argv[++i]);
                continue;
			}
            break;
		case 'g':
            CONFIG |= MIX_PUSHGO;
			if (i + 1 < argc) {
                arg = argv[i+1];
                if (IsDigit(*arg)) {
				    u = atoi(arg); i++;
                }
			}
			continue;
        case 'a': ASMONLY = ON; continue;
        case 'd': DUMP = ON; continue;
        case 'l': LNKLD = ON; continue;
        case 'p': TRANS = ON; continue;
        case 'r': FF = ON; continue;
        case 's':
			if (i + 1 < argc) {
                START = atoi(argv[++i]);
                continue;
            }
            break;
		case 't':
			if (i + 1 < argc) {
                InitTrace(argv[++i]);
                continue;
			}
			break;
		}
		Usage();
	}

	Init();

	for (i = 0; i < nasmfiles; i++) {
		arg = asmfiles[i];
        strncpy(TRANSNM, arg, 5);
        strtoupper(TRANSNM);
	    Asm(arg);
        arg = strchr(TRANSNM, '.');
        if (arg) *arg = 0;
    }

    if (SM_NAN == START)
        START = 0;

    if (TRANS)
        PunchTrans();
    if (CONFIG & MIX_PUSHGO) {
	    Go(DEVNO(u));
    } else if (!ASMONLY)
        Run(START);
    if (DUMP)
        Stats(stderr, 4, 0);
    if (InstCount) {
        double elapsedSeconds   = ElapsedMS / 1000.0;
        double elapsedTySeconds = (Tyme * TYME_BASE) / 1000000.0;
        double mipsRate = (InstCount / elapsedSeconds) / 1000000.0;
        double kipsRate = (InstCount / elapsedTySeconds) / 1000.0;
        Print("%*sTOTAL INSTRUCTIONS EXECUTED:     %09d\n", 30, "", InstCount);
        Print("%*sTOTAL ELAPSED TYME:              %09du (%09d IDLE) (%5.1lf KIPS)\n", 30, "", Tyme, IdleTyme, kipsRate);

        Print("%*sTOTAL ELAPSED TIME:              %9.3lfs                  (%5.1lf MIPS)\n", 30, "", elapsedSeconds, mipsRate);
    }

	return 0;
}

/* vim: set ts=4 sw=4 et: */
