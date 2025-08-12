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
 *  - macro assembly
 *  - B^E table
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
 *  250812AP    internal reorg: replaced sym indexes w/ ptr
 *              local symbol environment for locals and LOCs
 *              EQU sets the value, not defines
 *              detect undefined xF
 *  250811AP    renamed smDADD to smLADD
 *              added LADD, LSUB (F=0:7), LCMP (F=0:7)
 *              added DADD, DSUB, DMUL, DDIV, DCMP (F=1:0) skeletons
 *              added version string
 *              macro expand fixes
 *              added MIXAL error codes
 *              fixed verbose, simplified local symbols
 *  250810AP    macro assembler fixes
 *              own malloc/free
 *				CDC 731 029 code (same ANSI X3.26)
 *				macro expansion skeleton
 *  250809AP    conditional assembly fixes
 *              macro assembler skeleton
 *  250808AP    punched card code selection
 *              conditional assembly: IF,IFC,IFD,ELSE,ENDI
 *  250803AP    fixed SaveSTATE/RestoreSTATE
 *              renamed OT to OV
 *              fixed address check in IN/OUT
 *              machine starts in CONTROL state when interrupt facility installed
 *	250718AP	Tyme measurement fixes, simplified TymeWarp
 *	250716AP	delta-based I/O
 *				64bit InstCount, Tyme, IdleTyme
 *				fixed NTESTS init
 *				added TymeWarp
 *              fixed WAIT state instr. fetch
 *				fixed DoEvents()
 *  250715AP    fixed fpDIV(): need remainder in RX
 *              maintenance mode: number input in octal and 11-punch decimal
 *              always clear CY in fpADD()
 *				fixed TIMER wrap around
 *  250714AP    fixed smCMP (+0 = -0)
 *  250713AP    reuse literal constants option
 *  250711AP    fixed WIN32 cast to unsigned long
 *              random seed only once
 *              dump/read symbols
 *              added TRACEIO to blkRead
 *  250710AP    line numbers in assembly
 *              fixed comment line output
 *              write FP test data in INPUT fmt
 *              WIN32 random generator fixes
 *  250709AP    scaled #tests input
 *              changed failed tests formatting
 *              RoundDouble() rounding overflow fix
 *              fpNORM() clear CY
 *              FCMP fixes
 *  250708AP    binary test data compare
 *              exponent underflow/overflow handling
 *              RoundDouble() fixes
 *  250707AP    fixed fpNORM, uDSUB, smMUL, smDIV, fpDIV
 *              saving test vectors in TestData
 *  250706AP    FADD/FMUL/FIX fixes
 *              fixed sign in fpNORM rounding
 *              fixed float parsing
 *              fixed smLADD
 *              added verbose assemble flag
 *              FADD fix + random FP testing
 *              fixed memory-read access check
 *  250705AP    B^E table
 *              added DoubleToFP/FPToDouble
 *				float AtomicExpr
 *				fpFIX skeleton
 *              fpCMP, fpADD fixes
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
#if defined(WIN32)
# define _CRT_RAND_S
#endif
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#include "version.h"

#if defined(__linux__) || defined(__APPLE__) || defined(__COSMOPOLITAN__)
typedef unsigned long __uint64;
#include <sys/random.h>
#include <sys/time.h>

unsigned CurrentMS(void)
{
    struct timeval tv;

    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1000 + (tv.tv_usec + 500) / 1000;
}

unsigned long GetRandom(void)
{
    return (unsigned long) random();
}

unsigned GetSeed(void)
{
    unsigned seed;

    getentropy(&seed, sizeof(seed));
    return seed;
}

void InitRandom(unsigned seed)
{
    srandom(seed);
}

#elif defined(WIN32)
typedef unsigned long long __uint64;
#include <sys/types.h>
#include <sys/timeb.h>

unsigned CurrentMS(void)
{
    struct _timeb tb;

    _ftime(&tb);
    return tb.time * 1000 + tb.millitm;
}

unsigned GetRandom(void)
{
    unsigned ret;

    rand_s(&ret);
    return ret;
}

unsigned GetSeed(void)
{
    return CurrentMS();
}

void InitRandom(unsigned seed)
{
    srand(seed);
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
# if defined(ASSERT)
#  define EXT_ASSERT ASSERT
#  undef ASSERT
# endif
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
typedef enum {LESS, EQUAL, GREATER} Comparator;
typedef enum {S_HALT, S_STOP, S_WAIT, S_NORMAL, S_CONTROL} MachineState;
#define MIX_BINARY       1
#define MIX_INTERRUPT    2
#define MIX_FLOAT        4
#define MIX_CORE         8
#define MIX_PUSHGO      16
#define MIX_INDEX       32
#define MIX_MASTER     128
Word CONFIG;

static char *sot = " X", *sci = "<=>", *ssta = "HSWNC";


#define CORE_MEM    "core.mem"
#define CORE_CTL    "core.ctl"
unsigned MAX_MEM;
#define ADDR_TRACE  (MAX_MEM+1)
#define TRACE       mem[ADDR_TRACE]
#define TIMER       mem[MAX_MEM+2]

#define ADDR_RTC			10
#define INT_ADDR_RTC		11
#define INT_ADDR_OP			12
#define INT_ADDR_DEV_BASE	20
#define	RTC					ctlmem[ADDR_RTC]

Word reg[10], *mem, *ctlmem, P, IR;
const char **mapsyms;
Toggle OV;
Comparator CI;
Toggle TRANS, LNKLD, DUMP;
#define CARD_MIX    0
#define CARD_MIX360 1
#define CARD_DEC026 2
#define CARD_DEC029 3
#define CARD_SIXBIT 4
#define CARD_BIC    5
#define CARD_CDC731 6
int CARDCODE;
char TRANSNM[5+1];
const char *SYMNM;

#define TYME_BASE   6

/* typedef struct { __uint64 t; } TymeT; */
typedef __uint64 tyme_t;
typedef __uint64 inst_count_t;

tyme_t Tyme, IdleTyme, lastDoEventsTyme;
inst_count_t InstCount;
FILE *LPT;
unsigned TraceCount, ElapsedMS;
unsigned short *freq, *ctlfreq;
MachineState STATE, STATESAV;
int WaitRTI;
Toggle CY;
Toggle TYMEWARP;
Toggle TRACEOP, TRACEIO, TRACEA, VERBOSE;
Toggle XEQTING;
long NTESTS;
Toggle ZLITERALS;
Toggle DOEVENTS;

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


void PrintNorm(const char *msg, int e, Word hi, Word lo);
void MemWrite(Word a, int f, Word w);
Word MemRead(Word a);
int Stop(void);
int StripLine(char *line);


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

typedef enum {DO_NOTHING, DO_IOC, DO_IN, DO_OUT, DO_RDY} EventType;
unsigned EventH, PendingH;

void Schedule(unsigned delta, int u, EventType what, Word M);
void ScheduleINT(unsigned u);

void devError(unsigned u);
int devStuck(unsigned u);
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

/* 31bit */
#define SM_MSB1		(1U << 30)
#define SM_WORD1    SM_MASK(31)
#define MSB1(x)		((x) & SM_MSB1)
#define MAG1(x)		((x) & SM_WORD1)

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

#define ROTATE      1
#define SHIFT       0

#define IS_NEGATIVE(w)  (MAG(w) && SIGN(w))
#define IS_ZERO(w)      (!MAG(w))
#define IS_POSITIVE(w)  (MAG(w) && !SIGN(w))

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
	ptr[1] = epart < 0 ? '-' : '+';
	epart = ABS(epart);
	ptr[2] = epart < 10 ? '0' : '0' + (epart / 10);
	ptr[3] = '0' + (epart % 10);
	ptr[4] = 0;
	PrintLPT("%s%s", buf, ptr+1);
	space();
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

Comparator smCMP(Word x, Word y)
{
    Comparator ret;

    if (!MAG(x) && !MAG(y)) {
        ret = EQUAL;
    } else if (SIGN(x) == SIGN(y)) {
        if (MAG(x) > MAG(y)) {
            ret = SIGN(x) ? LESS : GREATER;
        } else if (MAG(x) < MAG(y)) {
            ret = SIGN(x) ? GREATER : LESS;
        } else {
            ret = EQUAL;
        }
    } else if (SIGN(x)) {
        ret = LESS;
    } else {
        ret = GREATER;
    }
    return ret;
}

void smDEC(Word *pw)
{
    *pw = smSUB(*pw, 1);
}

int ShiftAmount(int shmt, int maxshmt, int circ)
{
    int ret;

    ret = shmt;
    if (circ) {
        ret = shmt MOD maxshmt;
    } else {
        ret = MIN(shmt, maxshmt);
    }
    return ret;
}

Word smSLAX(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;
    int signA, signX;
	
    signA = SIGN(a); a = MAG(a);
    signX = SIGN(x); x = MAG(x);

    shmt = ShiftAmount(shmt, 10, circ);
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

    shmt = ShiftAmount(shmt, 10, circ);
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
	
    shmt = ShiftAmount(shmt, 60, circ);
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

Word uSLB1(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;

    a = MAG1(a); x = MAG(x);

    shmt = ShiftAmount(shmt, 61, circ);
	for (i = 0; i < shmt; i++) {
		sav = MSB1(a);
		a = MAG1(2 * a);
		if (MSB(x))
			a++;
		x = MAG(2 * x);
		if (circ && sav)
			x++;
	}

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
	
    shmt = ShiftAmount(shmt, 60, circ);
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
    if (CY) cy++;
	if (cy) {
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
	    OV = ON;
		*pquo = UNDEF; if (prem) *prem = UNDEF;
        return;
	}
	ma = MAG(a); mx = MAG(x); mv = MAG(v);
    uSLB1(&ma, &mx, ma, mx, 1, SHIFT);
	for (i = 0; i < 30; i++) {
		d = 0;
		if (ma >= mv) {
			d = 1; ma -= mv;
		}
		uSLB1(&ma, &mx, ma, mx, 1, SHIFT); mx += d;
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
#define FP_HALF     (04000000000U)
#define FP_REPRB	(00001000000U)
#define FP_Q		(BYTESIZE >> 1)
#define FP_MIN      (00001000000U)
#define FP_MAX      (07777777777U)
#define IS_NORM(w)  ((w) & 00077000000U)


/* 
 * Vol.2 pp. 234.
 * 
 *      e0 = b^(1-p), e >= 2*e0 / (1 - .5e0)^2 ~ 7.62942363516372e-06
 *
 */
#define FP_EPSILON  (000000200U)

/* e, +/-FFFF0 */
void fpFREXP(Word u, int *eq, Word *f)
{
	*f = SIGN(u) + field(u, FIELD(2,5)) * BYTESIZE;
	*eq = field(u, FIELD(1,1));
}

double FPToDouble2(Word u)
{
    Word uf;
    int ue, sign;
    union {
        __uint64 u;
        double d;
    } ret;

    fpFREXP(u, &ue, &uf);
    ue -= FP_Q;
    /* PrintLPT("(%d, %c%010o)\n", ue, PLUS(uf), MAG(uf)); */
    sign = SIGN(uf); uf = MAG(uf);
    ue *= 6;    /* B^ue */
    if (0 == uf)
        return 0.0;
    /* FFFF0 */
    while (0 == (SM_MSB & uf)) {
        ue--; uf <<= 1;
    }
    ASSERT(SM_MSB & uf);
    ue--; uf = MAG(uf << 1); uf <<= 2;
    ue += 1023;
    ret.u = sign ? 1 : 0; ret.u <<= 11;
    ret.u += ue; ret.u <<= 52;
    ret.u += ((__uint64)uf << 20);
    /* PrintLPT("%lo\n", ret.u); */
    return ret.d;
}

int mymod(int a, int b)
{
    int rat;

    rat = floor((double)a / b);
    return a - b * rat;
}

#define DOUBLE_EPSILON  (7.62942363516372e-06)

Comparator CompareDouble(double u, double v)
{
    Comparator ci;

    ci = LESS;
    if (fabs(u - v) <= DOUBLE_EPSILON) {
        ci = EQUAL;
    } else if (u < v) {
        ci = LESS;
    } else {
        ci = GREATER;
    }
    return ci;
}

#define MIX_DOUBLE_MIN  ((double)1.0e-150)

Word RoundDoubleToFP(double d)
{
    __uint64 frac0;
    int i, e, sign, sav;
    Word w, fract55, fract, fract_lo;

    union {
        double d;
        __uint64 u;
    } ret;

    ret.d = d;
    frac0 = ret.u & 0177777777777777777ULL; ret.u >>= 52;
    e     = ret.u & 03777ULL; ret.u >>= 11;
    sign  = ret.u;

    if (fabs(d) < MIX_DOUBLE_MIN) {
        return (sign ? SM_SIGN : 0);
    }

    frac0 += 1ULL << 52;                                        /* add implicit 1.        */
    fract_lo = frac0 & 037777777ULL;                            /* lower 23 bits          */
    fract_lo <<= 7;
    frac0 >>= 23;
    fract = frac0;
    e -= 1023; e++;
    /* 1.xxxxx1 */
    PrintNorm("F0", e, fract, fract_lo);
    if (0 != mymod(e, 6)) {                                     /* align to multiple of 6 */
        for (i = mymod(e, 6); i < 6; i++) {
            sav = fract & 1;
            fract >>= 1; e++;
            fract_lo >>= 1;
            if (sav)
                fract_lo += SM_MSB;
            PrintNorm("FI", e, fract, fract_lo);
        }
    }
    PrintNorm("FE", e, fract, fract_lo);
    ASSERT(0 == mymod(e, 6));
    e = e DIV 6 + FP_Q;
    if (e < 0) {
        if (VERBOSE)
            Warning("ROUNDDOUBLE: EXPONENT UNDERFLOW");
        return (sign ? SM_SIGN : 0) + FP_MIN;
    } else if (e > BYTESIZE-1) {
        if (VERBOSE)
            Warning("ROUNDDOUBLE: EXPONENT OVERFLOW");
        return (sign ? SM_SIGN : 0) + FP_MAX;
    }
    fract55 = BYTE(fract);
    if ((FP_Q < fract55)
        || (FP_Q == fract55 && fract_lo)
        || (FP_Q == fract55 && (0 == (fract DIV BYTESIZE) MOD 2)))
    {
        fract += 0100U;
        if (fract > SM_WORD) {
            fract = fract DIV BYTESIZE; e++;
        }
    }
    fract = fract DIV BYTESIZE;
    PrintNorm("FW", e, (sign ? SM_SIGN : 0) + fract, 0);
    w = (sign ? SM_SIGN : 0) + (e << 4*6) + fract;
    PrintNorm("FW", e, w, 0);
    return w;
}

double FPToDouble(Word u)
{
    Word f;
    double d;
    int i, sign, e, eq;

    fpFREXP(u, &eq, &f);
    sign = SIGN(f);
    f = MAG(f) DIV BYTESIZE;

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
        if (VERBOSE)
            Warning("DOUBLETOFP: EXPONENT OVERFLOW %d", eq);
        return (sign ? SM_SIGN : 0) + FP_MAX;
    } else if (eq < 0) {
        if (VERBOSE)
            Warning("DOUBLETOFP: EXPONENT UNDERFLOW %d", eq);
        return (sign ? SM_SIGN : 0) + FP_MIN;
    }
    w = 0;
    for (i = 0; i < 4; i++) {
        w = BYTESIZE * w + BYTE(fracs[i]);
    }
    w += BYTE(eq) << (4*6);
    w += sign ? SM_SIGN : 0;
    return w;
}

void PrintNorm(const char *msg, int e, Word hi, Word lo)
{
    if (NTESTS)
        return;

    Print("%s: (%d,%c%010o,%c%010o)\n",
        msg,
        e, PLUS(hi), MAG(hi),
        PLUS(lo), MAG(lo));
}

/* assume |f| < b */
Toggle LASTROUNDED;
Word fpNORM(int e, Word hi_f, Word lo_f)
{
	Word f55, w;
    int cnt;

    cnt = 0;
/*N1*/ PrintNorm("N1",e,hi_f,lo_f);
	if (CY) {
        CY = OFF;
        lo_f = 1;
		goto N4;
	}
	if (!MAG(hi_f)) {
		e = 0; goto N7;
	}
N2: PrintNorm("N2",e,hi_f,lo_f);
	if (MAG(hi_f) >= FP_ONE)
		goto N5;
/*N3*/PrintNorm("N3",e,hi_f,lo_f);
	smSLAX(&hi_f, &lo_f, hi_f, lo_f, 1, SHIFT); e--;
	goto N2;
    cnt = 0;
N4: PrintNorm("N4",e,hi_f,lo_f);
    smSRAX(&hi_f, &lo_f, hi_f, lo_f, 1, ROTATE); e++;
N5: PrintNorm("N5",e,hi_f,lo_f);
    f55 = BYTE(hi_f);
    LASTROUNDED = OFF;
    if (FP_Q < f55
        || (FP_Q == f55 && !IS_ZERO(lo_f))
        || (FP_Q == f55 && 0 == (hi_f DIV BYTESIZE) MOD 2))
    {
        LASTROUNDED = ON;
		hi_f = smADD(hi_f, SIGN(hi_f) + BYTESIZE);
	}
	lo_f = 0;
	if (CY) {
        CY = OFF;
        if (++cnt > 30) {
            Error("N4-N5 LOOP");
            return 0;
        }
        lo_f = 1;
        goto N4;
	}
/*N6*/PrintNorm("N6",e,hi_f,lo_f);
	if (e < 0) {
        if (VERBOSE)
            Warning("NORM: EXPONENT UNDERFLOW");
		OV = ON; CI = GREATER;
        w = SIGN(hi_f) + FP_MIN;
        goto ErrOut;
	} else if (e > BYTESIZE-1) {
        if (VERBOSE)
            Warning("NORM: EXPONENT OVERFLOW");
		OV = ON; CI = LESS;
        w = SIGN(hi_f) + FP_MAX;
        goto ErrOut;
	}
N7: PrintNorm("N7",e,hi_f,lo_f);
    w = SIGN(hi_f) + (MAG(hi_f) DIV BYTESIZE) + (BYTE(e) << (4*6));
ErrOut:
    PrintNorm(" W",e,w,0);
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
        *plo = loX + (SM_MSB << 1) - loY;
        hiY++;
    }
    *phi = hiX - hiY;
}

void smLADD(Word *phi, Word *plo, Word hiX, Word loX, Word hiY, Word loY)
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
        return;
    }        
    uDSUB(&hiZ, &loZ, MAG(hiX), MAG(loX), MAG(hiY), MAG(loY));
    *phi = SIGN(hiX) + hiZ;
    *plo = SIGN(hiX) + loZ;
}

void smLNEG(Word *phi, Word *plo, Word hi, Word lo)
{
    if (SIGN(hi)) {
        *phi = MAG(hi);
        *plo = MAG(lo);
        return;
    }
    *phi = SM_SIGN + MAG(hi);
    *plo = SM_SIGN + MAG(lo);
}

void smLSUB(Word *phi, Word *plo, Word hiX, Word loX, Word hiY, Word loY)
{
    smLNEG(&hiY, &loY, hiY, loY);
    smLADD(phi, plo, hiX, loX, hiY, loY);
}

Word fpADD(Word u, Word v)
{
	Word uf, vf;
	int ue, ve, we;
	Word wf, hi_wf;
	Word w;

    hi_wf = 0;
    CY = OFF;

/*A1.A2*/
	if (MAG(u) < MAG(v))
		SWAP(v, u);
		
	/* u >= v */
	fpFREXP(u, &ue, &uf);
	fpFREXP(v, &ve, &vf);
	PrintNorm(" U", ue, uf, 0);
	PrintNorm(" V", ve, vf, 0);
/*A3*/
	we = ue;
/*A4*/
	if (ue - ve >= FP_P + 2) {
		hi_wf = uf; wf = 0; goto A7;
	}
/*A5*/
	smSRAX(&hi_wf, &wf, vf, SIGN(vf), ue - ve, SHIFT);
	PrintNorm(" U", 0, uf, 0);
	PrintNorm(" V", 0, hi_wf, wf);
	smLADD(&hi_wf, &wf, hi_wf, wf, uf, 0);
	PrintNorm(" W", CY, hi_wf, wf);
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

	PrintNorm(" U", ue, uf, 0);
	PrintNorm(" V", ve, vf, 0);

    we = ue + ve - FP_Q;
    smMUL(&hi_wf, &wf, uf, vf);

    /* 04 46, 31 46 00 00 00 */
    PrintNorm(" W",we,hi_wf,wf);

    w = fpNORM(we, hi_wf, wf);
    return w;
}

Word fpDIV(Word u, Word v)
{
	Word uf, vf;
	int ue, ve, we;
	Word hi_wf, lo_wf;
    Word hi_uf, lo_uf;
	Word w;

    hi_wf = lo_wf = 0;
    hi_uf = lo_uf = 0;
	
	fpFREXP(u, &ue, &uf);
	fpFREXP(v, &ve, &vf);

	PrintNorm(" U", ue, uf, 0);
	PrintNorm(" V", ve, vf, 0);

    we = ue - ve + FP_Q /*+ 1*/;
    /* b^-1 * uf / vf  */
    hi_uf = uf;
    if (LESS != smCMP(MAG(uf), MAG(vf))) {
        smSRAX(&hi_uf, NULL, uf, 0, 1, SHIFT); we++;
    }
    PrintNorm(" U", 0, hi_uf, lo_uf);
    smDIV(&hi_wf, &lo_wf, hi_uf, lo_uf, vf);
    PrintNorm(" W", 0, hi_wf, lo_wf);

    w = fpNORM(we, hi_wf, lo_wf);
    return w;
}

Word fpFLOT(Word u)
{
    return fpNORM(FP_Q + 5, u, 0);
}

void fpCMP(Word u, Word v)
{
    Word uf, vf, a, x;
    int ue, ve, ediff;

    v = smNEG(v);

    if (MAG(u) < MAG(v))
        SWAP(u, v);

	/* u >= v */
	fpFREXP(u, &ue, &uf);
	fpFREXP(v, &ve, &vf);

    PrintNorm(" U", ue, uf, 0);
    PrintNorm(" V", ve, vf, 0);

    ediff = MIN(5, ue - ve);
	smSRAX(&a, &x, vf, SIGN(vf), ediff, SHIFT);
    PrintNorm("AX", ediff, a, x);
	a = smADD(a, uf);
    PrintNorm(" A", 0, a, 0);
	if (CY) {
        x = 1;
        smSRAX(&a, &x, a, x, 1, ROTATE);
        PrintNorm("OV", 0, a, x);
        goto CheckSign;
	} else {
	    CI = smCMP(MAG(a), FP_EPSILON);
        if (0 == NTESTS) {
            PrintLPT("CI=%c\n", sci[CI]);
        }
	    if (GREATER == CI) {
            goto CheckSign;
	    } else if (LESS == CI) {
            CI = EQUAL;
	        return;
        } else if (IS_ZERO(x)) {
            return;
        } else if (IS_POSITIVE(x) && IS_POSITIVE(a)) {
            goto CheckSign;
        } else if (IS_POSITIVE(a)) {
            return;
        }
	}
CheckSign:
	if (SIGN(a))
	    CI = LESS;
	else if (!MAG(a))
	    CI = EQUAL;
	else
	    CI = GREATER;
}

Word fpFIX(Word u)
{
	Word uf;
	Word a, x;
	int ue, e;

	fpFREXP(u, &ue, &uf);
	e = ue - FP_Q;

	if (IS_ZERO(uf))
	    return uf;

	a = SIGN(uf); x = uf;
	if (e > 0)
	    smSLAX(&a, &x, a, x, e, SHIFT);
    else
        smSRAX(&a, &x, a, x, -e, SHIFT);
    PrintNorm(" W", 0, a, x);
    if ((MAG(x) > FP_HALF)
        || (FP_HALF == MAG(x) && 0 == (MAG(a) MOD 2)))
    {
        a = smADD(a, SIGN(a) + 1);
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

int IsRunning(void)
{
    return (S_HALT != STATE && S_STOP != STATE);
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

int IsHalted(void)
{
    return S_HALT == STATE;
}

void Resume(void)
{
	ASSERT(IsWaiting());
    ASSERT(S_NORMAL == STATESAV || S_CONTROL == STATESAV);

    TraceIOLoc("RESUME AT %09llu", Tyme);

    STATE = STATESAV;
    STATESAV = S_HALT;
}

void Wait(unsigned evt)
{
    ASSERT(!IsWaiting());
    ASSERT(S_HALT == STATESAV);

    TraceIOLoc("WAIT AT %09llu DELTA=%u", Tyme, evt);

    STATESAV = STATE;
    STATE = S_WAIT;
}

void SaveSTATE(Word loc)
{
	int i;
	Word w;
	
    ASSERT(CONFIG & MIX_INTERRUPT);
	ASSERT(IsNormal());
	
	for (i = 0; i < 8; i++)
		ctlmem[i + 2] = reg[7-i];

    /* TBD MIXMASTER */
	w  = MAG(P);       w *= BYTESIZE;
	w += FIELD(OV,CI); w *= (BYTESIZE * BYTESIZE);
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
	OV = L(f); CI = R(f);
	P  = A_MASK & w;

	for (i = 0; i < 8; i++)
		reg[7-i] = ctlmem[i + 2];

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


int CheckAccess(Word a, const char *msg)
{
    if (SIGN(a) && IsNormal()) {
        return MIX_ErrorLoc("M=%c%010o ILLEGAL ACCESS %s", PLUS(a), MAG(a), msg);
    }
    return 0;
}

int CheckAddr(Word a, const char *msg)
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

int CheckMemAccess(Word a, const char *msg)
{
    ASSERT(IsNormal() || IsControl());

    if (CheckAccess(a, msg) || CheckAddr(a, msg))
        return 1;
    return 0;
}

int CheckMemRead(Word a)
{
    return CheckMemAccess(a, "MEMORY READ");
}

int CheckFetch(Word a)
{
    ASSERT(IsNormal() || IsControl() || IsWaiting());

    if (CheckAccess(a, "MEMORY FETCH") || CheckAddr(a, "MEMORY FETCH"))
        return 1;

    return 0;
}

void IncTIMER(unsigned diff)
{
	TIMER = SM_WORD & (TIMER + diff);
}

Word MemRead(Word a)
{
    Word signA;

	Tyme++; IncTIMER(1);
    signA = SIGN(a); a = MAG(a);
	return signA ? ctlmem[a] : mem[a];
}

int CheckMemWrite(Word a)
{
    return CheckMemAccess(a, "MEMORY WRITE");
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

    Tyme++; IncTIMER(1);
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

void StrSet(char *s, int ch, int len)
{
    int i;

    s[len] = 0;
    for (i = 0; i < len; i++)
        s[i] = ch;
}

void StrCpy(char *d, const char *s, int len)
{
    int i;

    d[len] = 0;
    for (i = 0; *s; i++) {
        if (len == i)
            break;
        d[i] = *s++;
    }
    for (; i < len; i++) {
        d[i] = ' ';
    }
}

#define MIXMAGIC    0xB6A995F49ULL
size_t dynMem = 0;
char *EmptyStr = "";

void *Malloc(size_t size)
{
    void *p;
    size_t *psize;

    p = malloc(2 * sizeof(size_t) + size);
    if (NULL == p) {
        Error("NOT ENOUGH MEMORY (ALLOCATED: %u, REQUESTED: %u)", dynMem, size);
        exit(1);
    }
    psize = (size_t *)p;
    *psize++ = size;
    *psize++ = MIXMAGIC;
    return (void *)psize;
}

void Free(void *p)
{
    size_t *psize;
    
    if (NULL == p || EmptyStr == (char *)p)
        return;

    psize = (size_t *)p;
    psize--;
    ASSERT(MIXMAGIC == *psize);
    psize--;
    dynMem -= *psize;
    free(psize);
}

char *StrDup(const char *s)
{
    char *d;

    if (NULL == s || EmptyStr == s)
        return EmptyStr;
    
    d = Malloc(strlen(s) + 1);
    strcpy(d, s);
    return d;
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
unsigned MAX_DEVS;
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
/*		                           1         2         3         4         5         6	 */
/*		                 0123456789012345678901234567890123456789012345678901234567890123*/
char    mix_m2a[64+1] = " ABCDEFGHI~JKLMNOPQR|_STUVWXYZ0123456789.,()+-*/=$<>@;:'????????";
char mix360_m2a[64+1] = " ABCDEFGHI~JKLMNOPQR|_STUVWXYZ0123456789.,()+-*/=$<>@;:'\"%&#c!^?";
char dec026_m2a[64+1] = " +-0123456789ABCDEFGHIJKLMNOPQR/STUVWXYZ_=@^'\\?.)]<!:$*[>&;,(\"#%";
char dec029_m2a[64+1] = " &-0123456789ABCDEFGHIJKLMNOPQR/STUVWXYZ:#@'=\"[.<(+^!$*);\\],%_>?";
char cdc731_m2a[64+1] = " &-0123456789ABCDEFGHIJKLMNOPQR/STUVWXYZ:#@'=\"[.<(+!]$*);^\\,%_>?";	/* CDC 731 029 */
char sixbit_m2a[64+1] = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_";
char    bic_m2a[64+1] = "0123456789#@?:>}+ABCDEFGHI.[&(<~|JKLMNOPQR$*-);{ /STUVWXYZ,%!=]\"";


struct {
	int idx;
    const char *opt;
    const char *m2a;
} cardcodes[] = {
    {   CARD_MIX,    "mix",    mix_m2a},
    {CARD_MIX360, "mix360", mix360_m2a},
    {CARD_DEC026, "dec026", dec026_m2a},
    {CARD_DEC029, "dec029", dec029_m2a},
    {CARD_SIXBIT, "sixbit", sixbit_m2a},
    {   CARD_BIC,    "bic",    bic_m2a},
    {CARD_CDC731, "cdc731", cdc731_m2a},
    {CARD_CDC731,   "x326", cdc731_m2a},
    {         -1,     NULL,       NULL}
};
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

#define DT_STUCK    ((unsigned)-1)


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

int devType(unsigned u);
void blkSeek(unsigned u, unsigned pos);
void blkRead(unsigned u, unsigned adr, Byte *cvt);
void blkWrite(unsigned u, unsigned adr, char *cvt);

static char *sio_sched[] = {
	"NOP",
	"IOC",
	"IN",
	"OUT",
	"RDY"
};

static char *sio[] = {
	"DOIO.NOP",
	"DOIO.IOC",
	"DOIO.IN",
	"DOIO.OUT",
	"DOIO.RDY"
};

unsigned PendingEventH;

void EventEnQ(unsigned u, unsigned when)
{
	unsigned i, p;

    i = EventH; p = 0;
    while (i && devs[i-1].when <= when) {
    	p = i;
        i = devs[i-1].evtNext;
    }
    if ((0 == EventH) || (EventH == i)) { /* empty or head */
		if (0 == EventH) {
			lastDoEventsTyme = Tyme;
		}
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
	    Info("TYME=%09llu", Tyme);
	    i = EventH;
	    while (i) {
		    p = i-1;
			Info("%07u LOC=%04o UNO=%02o/%04o %s",
                devs[p].when, devs[p].LOC, UNO(p), devs[p].M, sio_sched[devs[p].what]);
		    i = devs[p].evtNext;
	    }
	    Info("*************************************");
    }
}

void Schedule(unsigned delta, int u, EventType what, Word M)
{
    unsigned when;

    ASSERT(EventH <= MAX_DEVS);

    ASSERT(0 != delta);
    ASSERT(0 == devs[u].evtNext);
    ASSERT(DO_NOTHING == devs[u].what);

    /* unit is busy until delta */
    devs[u].evt = delta;

    /* do actual I/O at delta/2 */
    /* NB. except DO_RDY @ Tyme+delta */
    when = (DO_RDY == what) ? delta : delta / 2;
    devs[u].what = what;
    devs[u].when = when;
    devs[u].LOC = P;
    devs[u].M = M;
    devs[u].S = STATE;

    if (DOEVENTS) {
	    devs[u].evtNext = PendingEventH;
	    PendingEventH = u+1;
	    return;
    }

    EventEnQ(u, when);
}

void ScheduleINT(unsigned u)
{
    unsigned i, p;
    unsigned prio;

    ASSERT(CONFIG & MIX_INTERRUPT);

    ASSERT(PendingH <= MAX_DEVS);

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

void doIO(unsigned u)
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
    DEV_TraceIOLoc(u, M, "%09llu %s", Tyme, sio[what]);
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
	case DO_RDY:
	    if (CONFIG & MIX_INTERRUPT) {
		    /* if I/O initiated in control state and not the RTC */
		    if (u && S_CONTROL == devs[u].S) {
		        if (devs[u].pending) {
		            Warning("UNO=%02o PENDING INTERRUPT", UNO(u));
		        } else {
		            ScheduleINT(u);
		        }
	        }
        }
		return;
    }
    /* if not RTC and not stuck */
    if (u && !devStuck(u)) {
	    Schedule(devs[u].evt, u, DO_RDY, M);
    }
}

unsigned delta1 = 0, maxdelta = 0;
void DoEvents(void)
{
	
	int u, p;
	tyme_t delta;

	ASSERT(EventH);

	if (Tyme == lastDoEventsTyme)
		return;

	if (Tyme < lastDoEventsTyme) {
		/* wrap-around */
		delta = DT_STUCK - lastDoEventsTyme + Tyme + 1;
	} else {
		delta = Tyme - lastDoEventsTyme;
	}
    ASSERT(0 != delta);

	/* TraceIO("DO EVENTS DELTA=%u WHEN=%u", delta, devs[EventH-1].when); */

	/* process events */
	DOEVENTS = ON;
	while (EventH && devs[EventH-1].when <= delta) {
		u = EventH-1;
		EventH = devs[u].evtNext;
		devs[u].evtNext = 0;
		devs[u].evt -= MIN(devs[u].evt, delta);
		doIO(u);
	}
	DOEVENTS = OFF;

	/* lapse delta tyme */
	p = EventH;
	while (p) {
		u = p-1;
		ASSERT(delta < devs[u].when);
		ASSERT(devs[u].when <= devs[u].evt);
		ASSERT(!devStuck(u));
		devs[u].when -= delta;
		devs[u].evt -= delta;
		p = devs[u].evtNext;
	}

	/* insert new events */
	while (PendingEventH) {
		u = PendingEventH-1;
		PendingEventH = devs[u].evtNext;
		devs[u].evtNext = 0;
		EventEnQ(u, devs[u].when);
	}

	lastDoEventsTyme = (0 == EventH) ? 0 : Tyme;
}

void DoInterrupts(void)
{
	int u;

	ASSERT(IsNormal());

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

int devType(unsigned u)
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

void devError(unsigned u)
{
	devs[u].evt = DT_STUCK;
}

int devStuck(unsigned u)
{
    return DT_STUCK == devs[u].evt;
}

int devBusy(unsigned u)
{
	int busy;

	busy = (DT_STUCK == devs[u].evt) || devs[u].evt;
    if (IsWaiting() && !busy) {
		Resume();
    }
    return busy;
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

void blkRead(unsigned u, Word adr, Byte *cvt)
{
	unsigned ret, n;
	int j, x;
    Word mar, w, iobuf[MAX_WORD_BLOCK];
    char tmp[MAX_CHAR_BLOCK * BYTES + 1];
	unsigned i, Blk_size;
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
        StrSet(tmp, ' ', sizeof(tmp)-1);
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

    tmp[n] = 0;
    DEV_TraceIO(u, adr, "BLKREAD BUF='%s'", tmp);
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

void blkDump(unsigned u, Word adr, Toggle bytes)
{
    int j, x;
    unsigned i, Blk_size;
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

void blkWrite(unsigned u, unsigned adr, char *cvt)
{
	unsigned ret, n;
	int j, x;
    Word mar, w;
    char buf[MAX_CHAR_BLOCK * BYTES + 1 + 1];     /* + CR/LF + NUL */
	unsigned i, Blk_size;
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


void blkSeek(unsigned u, unsigned pos)
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


int devOpen(unsigned u)
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


unsigned doIOC(unsigned u,unsigned *pM)
{
	int x;
	int new_pos, old_track, new_track;
	unsigned M;
	
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


void devIOC(unsigned u, unsigned M)
{
    unsigned delta;

    delta = doIOC(u, &M);
    if (!devStuck(u)) {
		if (delta)
			Schedule(delta, u, DO_IOC, M);
    }
}


void devINP(unsigned u, Word M)
{
	int x;
	char *errmsg = NULL;
    unsigned delta;
	
	x = devType(u);
    if (CheckMemAccess(M, "IN BUFFER")
        || CheckMemAccess(M + IOchar[x].blk_size, "IN BUFFER END"))
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
		unsigned new_pos = 0;
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
	if (CheckMemAccess(M, "OUT BUFFER")
        || CheckMemAccess(M + IOchar[x].blk_size, "OUT BUFFER END"))
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
		unsigned new_pos = 0;
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

	{"LADD", MM(01,07)},
	{"LSUB", MM(02,07)},

	{"DADD", MM(01,010)},
	{"DSUB", MM(02,010)},
	{"DMUL", MM(03,010)},
	{"DDIV", MM(04,010)},

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
    {"LCMP", MM(070,07)},
	{"DCMP", MM(070,010)},

    {  NULL, MM(000,00)},
};


#define MAX_LINE 80

char mnemo[5];      /* mnemonic */


typedef enum {SYM_UNDEF, SYM_LOC, SYM_EQU} SYMTYPE;

typedef struct {
    char S[10+1];   /* symbol  */
    SYMTYPE T;      /* type: L - location, E - equ */
    Word N;         /* value   */
    Word A;			/* loader addr */
    Toggle D;       /* defined? */
} SYM;

typedef struct {
    Word B, H, F;
} LOCSYM;

int LNO;            /* line no */
char LINE[MAX_LINE + 1];  /* curr.line */
const char *LOCATION, *OP, *ADDRESS;
int CH;             /* curr.char */
const char *PLN;    /* line ptr */
Toggle E;           /* error? */
char EC[4 + 1];		/* error codes */
int NE;				/* #error codes */
char FREF;			/* future ref. */
Toggle FF;          /* free fmt. */
const char *stok[] = { "ERR", "LOC", "NUM", "FLT", "SYM"};
enum {TOK_ERR, TOK_MTY, TOK_LOC, TOK_NUM, TOK_FLT, TOK_SYM} T; /* token type */
char S[10 + 1];     /* parsed symbol */
Word N;             /* parsed number */
int  B;             /* binary op */
SYM *XS;            /* TOK_SYM FindSym() result */
LOCSYM *XL;         /* TOK_SYM FindSym() result if local (xBHF) */
Toggle UNDSYM;      /* undefined symbol */


#define REQUIRE(ch)  \
    if (ch != CH) { \
        return AsmError(EA_INVCHR); \
    } \
    NEXT();


#define ENSURE(ch)  \
    { if (ch != CH) { \
        AsmError(EA_INVCHR); \
      } \
      if (CH) NEXT(); \
    }

/* available error codes: GJKPQWY */

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
/* scope depth */
#define EA_DEPTH    'H'
/* misplaced/unbalanced struct */
#define EA_MISPLD   'I'
/* symbol/const/literal len */
#define EA_MAXLEN	'L'
/* missing operand (binary), or symbol */
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
/* symbol/macro table full */
#define EA_TABFUL   'Z'
/* linker error */
#define EA_LINKLD	'9'


#define NSYMS 1500
SYM symtab[NSYMS];
int nsymtab;        /* no. of syms */

#define NLOCSYMS 100
SYM loctab[NLOCSYMS];
int loctab0, nloctab;

/* !!! the locals[] stack has the same depth as the MAC stack !!! */
#define MAX_MACSCOPE 10

#define NLOCALS (10 * MAX_MACSCOPE)
LOCSYM locals[NLOCALS];
int locals0;

typedef struct __MBODY {
    struct __MBODY *next;
    char LINE[MAX_LINE+1];
} MBODY;

#define NMACS       100
#define NMACNAME      4
#define NMACARGS     10
struct {
    char   S[NMACNAME+1];    /* name */
    int    nargs;            /* #args    */
    char  *args[NMACARGS];   /* arg name */
    char  *dflt[NMACARGS];   /* defaults */
    MBODY *body;
    Toggle locked;           /* during expand, prevent redefinition */
} macs[NMACS+1];
int nmacs;

struct {
    int    EXPMAC;          /* expanding MAC index */
    MBODY *EXPBDY;          /* current BODY line */
    char  *VALS[NMACARGS];
    int locals0;
    int loctab0, nloctab;
} MEXP[MAX_MACSCOPE+1];     /* current MAC expand */
int MEXPLVL;

int SAVMAC, EXPMAC;
MBODY *SAVBDY,*EXPBDY;

int MACNEST;                /* nested macro definition */

#define MACNARGS    macs[EXPMAC].nargs
#define MACARGS     macs[EXPMAC].args
char *MACVALS[NMACARGS];

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
	
    if (VERBOSE) {
        Info("LINKLOAD");
    }
	while (SM_NOADDR != adr) {
	    if (GetV(adr, FIELD(0, 2), &nadr)) {
		    ret = 1;
		    break;
	    }
	    MemWrite(adr, FIELD(0, 2), w);
	    if (VERBOSE)
            Print(" %c%04o/%c%04o", PLUS(adr), MAG(adr), PLUS(w), MAG(w));
		if (adr == nadr) {
			ret = 1;
			break;
		}
		adr = nadr;
	}
	if (VERBOSE)
	    Print("\n");
	return ret;
}

void OverPunch(char *buf, int sign, unsigned w);

void PunchLinkLoad(FILE *fout)
{
    int i, j, n;
    char buf[8 + 1];
    Word a, w;

    /* LNKLDn  12341234... */
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
    return RANGE(ch,'A','Z') || ('~' == ch) || ('|' == ch) || ('_' == ch);
}

int IsAlphaExceptArrow(int ch)
{
    return ('~' != ch) && IsAlpha(ch);
}

int AsmError(char e)
{
	if (NE < 4)
		EC[NE++] = e;
    E = ON; T = TOK_ERR;
    return 0;
}

int GetDelim(char *s, int delim)
{
    int n;

    n = 0;
    while (CH && (delim != CH)) {
        s[n++] = CH;
        NEXT();
    }
    s[n] = 0;
    return n;
}

const char *SBEG, *SEND;
int GetId(int *ptr_isnum, char *buf, int len)
{
    int n, d;
    int isnum;

    isnum = *ptr_isnum;
    n = 0; SBEG = SEND = NULL;
    while (CH && ((d = IsDigit(CH)) || IsAlpha(CH))) {
        if (!SBEG)
            SBEG = PLN-1;
        isnum = isnum && d;
        if (n < len)
            buf[n] = CH;
        n++; NEXT();
    }
    *ptr_isnum = isnum;
    return n;
}

int GetExtSym(char *s)
{
    int isnum, n;

    if (',' == CH) {
        NEXT();
        s[0] = 0; n = 1;
        TraceA("GETEXTSYM: COMMA n=%d '%s'", n, s);
        return n;
    } else if ('(' == CH) {
        NEXT(); n = GetDelim(s, ')'); ENSURE(')');
        TraceA("GETEXTSYM: () n=%d '%s'", n, s);
        return n;
    }
    isnum = 1;
    n = GetId(&isnum, s, MAX_LINE);
    s[n] = 0;
    TraceA("GETEXTSYM: ID n=%d '%s'", n, s);
    return n;
}

int GetSym(void)
{
    int i, n, d, isnum, isfloat, savn;

    if (CH && '*' == CH) {
        NEXT();
        T = TOK_LOC;
        return 0;
    }
    isnum = 1; isfloat = 0;
    StrSet(S, ' ', sizeof(S)-1);
    n = GetId(&isnum, S, sizeof(S)-1);
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
	    SEND = PLN-2;
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
		/* Print("BUF='%s' D=%g N=%c%010o\n", buf, D, PLUS(N), MAG(N)); */
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

void LockMac(int x)
{
    ASSERT(0 < x && x <= nmacs);
    ASSERT(OFF == macs[x].locked);

    macs[x].locked = ON;
}

void UnLockMac(int x)
{
    ASSERT(0 < x && x <= nmacs);
    ASSERT(ON == macs[x].locked);

    macs[x].locked = OFF;
}

/* find a MACR definition */
int FindMac(const char *S)
{
    int i, found;

    found = 0;
    for (i = 1; i <= nmacs; i++) {
        if (!strcmp(macs[i].S, S)) {
            found = i;
            break;
        }
    }
    TraceA("    FIND.MAC='%s' RESULT=%d", S, found);
    return found;
}

/* find the argument in the current MACRO expansion, return index+1 */
int FindMacArg(const char *s)
{
    int i, found;

    ASSERT(EXPMAC);

    TraceA("    FIND.MACARG MACRO '%s'", macs[EXPMAC].S);
    found = 0;
    for (i = 0; i < MACNARGS; i++) {
        if (!strcmp(MACARGS[i], s)) {
            found = i + 1;
            break;
        }
    }
    TraceA("    FIND.MACARG='%s' RESULT=%d", s, found);
    return found;
}

/* find the argument in the MACRO expansion stack, return 1 if found, d contains the value */
int FindMacSubst(const char *s, char *d)
{
	int i, lvl, nargs, expmac;
	char *val;
	
	i = FindMacArg(s);
	if (i) {
		val = MACVALS[i-1];
		goto Out;
	}
	for (lvl = MEXPLVL; lvl > 0; lvl--) {
		expmac = MEXP[lvl].EXPMAC;
		nargs = macs[expmac].nargs;
		for (i = 0; i < nargs; i++) {
			if (!strcmp(s, macs[expmac].args[i])) {
				val = MEXP[lvl].VALS[i];
				goto Out;
			}
		}
	}
	return 0;
Out:
	strcpy(d, val);
	return 1;
}

SYM *FindSymP(const char *S, SYM *syms, int syms0, int nsyms)
{
    int i;
    SYM *found;

    found = NULL;
    /* Info("FINDSYMP '%s' %d - %d", S, syms0, nsyms); */
    for (i = syms0; i < nsyms; i++) {
        /* Info("FINDSYMP '%s'", syms[i].S); */
        if (!strcmp(syms[i].S, S)) {
            found = &syms[i];
            break;
        }
    }
    TraceA("    FIND.SYM='%s' RESULT=%d", S, found ? 1 : 0);
    if (found)
        TraceA("    N=%d D=%s", w2i(found->N), ONOFF(found->D));
    return found;
}

SYM *FindSym(const char *S)
{
    SYM *found;

    found = FindSymP(S, loctab, loctab0, nloctab);
    if (found) return found;
    return FindSymP(S, symtab, 0, nsymtab);
}

/* literals always in global symbol table */
SYM *FindSymByA(Word w)
{
    int i;
    SYM *found;

    found = NULL;
    for (i = 0; i < nsymtab; i++) {
        if ('=' == symtab[i].S[0] && w == symtab[i].A) {
            found = &symtab[i];
            break;
        }
    }
    TraceA("    FIND.SYM=%d RESULT=%d", w2i(w), found ? 1 : 0);
    if (found)
        TraceA("    A=%d D=%s", w2i(found->A), ONOFF(found->D));
    return found;
}

int IsLocalSym(const char *s)
{
    int ch1, ch2, ch3;

    ch1 = s[0]; ch2 = s[1]; ch3 = s[2];
    return IsDigit(ch1) && ('H' == ch2 || 'B' == ch2 || 'F' == ch2) && (' ' == ch3);
}


int ResolveAdr(Word adr, Word w)
{
    if (SM_NOADDR != adr) {
	    if (nload >= NLOAD) {
            AsmError(EA_TABFUL);
		    return AsmError(EA_LINKLD);
	    }
		LOAD[nload++] = adr;
		LOAD[nload++] = w;
        if (!LNKLD && LinkLoad(adr, w))
            AsmError(EA_LINKLD);
	}
    return 0;
}


SYM *LSYM;	/* last DefineSym() result */
Word LREF;	/* last reference to just defined sym */

int DefineSymIdxP(SYM *syms, int *p_nsyms, int maxlen, const char *S, Word w, Toggle defd, SYMTYPE typ)
{
    int nsyms;

    ASSERT(NULL != S);
    ASSERT(NULL != syms);
    ASSERT(NULL != p_nsyms);

    nsyms = *p_nsyms;

    if (maxlen == nsyms) {
        AsmError(EA_TABFUL);
        return AsmError(EA_INVSYM);
    }
    TraceA("    N=%d", w2i(w));
    TraceA("    D=%s", ONOFF(defd));
    strcpy(syms[nsyms].S, S);
    syms[nsyms].N = w;
    syms[nsyms].A = SM_NOADDR;
    syms[nsyms].D = defd;
    syms[nsyms].T = typ;
    LSYM = &syms[nsyms];
    nsyms++;

    *p_nsyms = nsyms;
    return 0;
}

int DefineSymIdx(SYM *found, const char *S, Word w, Toggle defd, SYMTYPE typ)
{
	Toggle islocal;
	Word adr;

    ASSERT(NULL != found || (NULL == found && NULL != S));

    if (NULL != found && NULL == S)
        S = found->S;
    islocal = IsLocalSym(S);
    ASSERT(!islocal);

    LSYM = NULL;

	adr = SM_NOADDR;
    if (found) {
        /* TAB ARG
         * D 	D	DUPSYM
         * D 	U	ASSERT
         * U 	D	got value
         * U 	U	just another ref in the chain, handle elswhere
         */

        /* defined EQU could be redefined */
        if (SYM_EQU == found->T && found->D) {
            found->N = w;
            return 0;
        }
        if (found->D && defd) {
            found->N = w;
            return AsmError(EA_DUPSYM);
        }
        ASSERT(OFF == found->D && ON == defd);

        /*
         * always overwrite typ
         * --------------------
         * a) may be unknown LOC
         * b) future ref. SYM_LOC which is resolved by SYM_EQU
         *
         */
        found->T = typ;
	    N = found->N;
	    TraceA("    %s '%s' DEFINED %c%05o (CHAIN %c%05o)",
	            "FUTURE.REF", S, PLUS(w), MAG(w), PLUS(N), MAG(N));
	    LREF = adr = found->A = found->N;
	    FREF = 'L';
        found->N = w;
        found->D = ON;
        return ResolveAdr(adr, w);
    }
    return DefineSymIdxP(symtab, &nsymtab, NSYMS, S, w, defd, typ);
}

int DefineSym(const char *S, Word w, Toggle defd, SYMTYPE typ)
{
    SYM *found;

    TraceA("    DEFINE SYM '%s'", S);
    if ('=' == S[0]) {
        LSYM = found = 0;
    } else {
        LSYM = found = FindSym(S);
    }
    return DefineSymIdx(found, S, w, defd, typ);
}

/* always in loctab[] */
int DefineLocalSym(const char *S, SYMTYPE typ)
{
    SYM *found;

    TraceA("    DEFINE LOCAL SYM '%s'", S);
    found = FindSymP(S, loctab, loctab0, nloctab);
    if (found) {
        TraceA("    DUPLICATE LOCAL SYM '%s'", S);
        return AsmError(EA_DUPSYM);
    }
    return DefineSymIdxP(loctab, &nloctab, NLOCSYMS, S, SM_NOADDR, OFF, typ);
}

int localIdx(const char *s)
{
    ASSERT(IsLocalSym(s));

    return locals0 + s[0] - '0';
}

Word getLocal(LOCSYM *p, const char *s)
{
    Word ret;

    ASSERT(NULL != p);

    ret = SM_NOADDR;

    switch (s[1]) {
    case 'B': ret = p->B; break;
    case 'H': ret = p->H; break;
    case 'F': ret = p->F; break;
    default:
        ASSERT(OFF);
    }

    return ret;
}

void setLocal(LOCSYM *p, const char *s, Word w)
{
    ASSERT(NULL != p);

    switch (s[1]) {
    case 'B': p->B = w; break;
    case 'H': p->H = w; break;
    case 'F': p->F = w; break;
    default:
        ASSERT(OFF);
    }
}

int ShowLocal(int i, char typ)
{
    Word adr;
    char s[3];
    Toggle defd;
    LOCSYM *p;

    s[0] = '0' + i;
    s[1] = typ;
    s[2] = 0;

    p = &locals[localIdx(s)];

    adr = getLocal(p, s);
    defd = TONOFF(SM_NOADDR != adr);

    if (defd) {
        space(); dprin(i); prin(s); space();
        wprint(adr);
    }
    return defd;
}

void ShowLocalSyms(const char *msg)
{
    int i, d;

    TraceA("**** LOCALS %s ****", msg);
    for (i = 0; i < 10; i++) {
        d  = ShowLocal(i, 'B');
        d += ShowLocal(i, 'H');
        d += ShowLocal(i, 'F');
        if (d) nl();
    }
    TraceA("****************");
}

void InitLocalSyms(int newbase)
{
    int i;

    locals0 = newbase;
    for (i = locals0; i < locals0 + 10; i++) {
        locals[i].B = SM_NOADDR;
        locals[i].H = SM_NOADDR;
        locals[i].F = SM_NOADDR;
    }
}

void ShowMacDef(int x)
{
    int i;
    MBODY *body;

    Info("*** DEFINE MACRO %s (%d) ***", macs[x].S, x);
    Info("#ARGS: %d", macs[x].nargs);
    for (i = 0; i < macs[x].nargs; i++) {
        char *val = macs[x].dflt[i];
        Info("   %s=%s",macs[x].args[i],val);
    }
    body = macs[x].body;
    if (body) {
        Info("BODY:");
        while (body) {
            Info(body->LINE);
            body = body->next;
        }
    }
    Info("****************");
}

void ShowMacExp(int x)
{
    int i;

    Info("*** EXPAND MACRO %s ***", macs[x].S);
    Info("#ARGS: %d", macs[x].nargs);
    for (i = 0; i < macs[x].nargs; i++) {
        Info("   %s=%s",macs[x].args[i],MACVALS[i]);
    }
    Info("****************");
}

Word AtomicExpr(void)
{
    SYM *found;
    Word ret = 0;
    Toggle localB;

    XL = NULL; XS = NULL;

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
            int x = localIdx(S);
            if ('H' == S[1])
                AsmError(EA_INVSYM);
            localB = TONOFF('B' == S[1]);
            ret = localB ? locals[x].B : locals[x].F;
            if (SM_NOADDR == ret) {
                if (localB)
                    AsmError(EA_UNDBCK);
                UNDSYM = ON;
            }
            TraceA("    ATOMICEXPR=%c%010o", PLUS(ret), MAG(ret));
            XL = &locals[x];
            return ret;
        }
        XS = found = FindSym(S);
        if (found) {
            if (OFF == found->D) {
                if (localB)
                    AsmError(EA_UNDBCK);
                UNDSYM = ON; /* FUTURE.REF */
            } else {
                ret = found->N;
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
        OV = OFF;
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
	        case '/': smDIV(&v, NULL, 0, v, MAG(w) ? w : 1); break;
	        case 'D': /*//*/
                smDIV(&v, NULL, v, 0, MAG(w) ? w : 1);
                break;
	        case ':': v = i2w(8 * w2i(v) + w2i(w)); break;
	        default: break;
	        }
        }
        TraceAV(FLOATARG, "AFTER", 'V', v);
        if (OV)
        	AsmError(EA_OVRFLW);
    }
    TraceAV(FLOATARG, "EXPR", 'V', v);
    return v;
}

Word Apart(void)
{
    Word v = 0;
    const char *LBEG = NULL, *LEND = NULL;
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
	        REQUIRE('=');
            if (LEND - LBEG <= 10) {
                if (UNDSYM)
                    AsmError(EA_UNDSYM);
                StrSet(S, ' ', sizeof(S)-1);
                for (i = 0; i < LEND - LBEG; i++)
                    S[i] = LBEG[i];
                TraceA("    LIT='%s' V=%c%010o", S, PLUS(v), MAG(v));
                UNDSYM = ON; XS = ZLITERALS ? FindSymByA(v) : NULL;
	        } else
	        	AsmError(EA_MAXLEN);
	    }
        if (UNDSYM) {
            if (XS || XL) {
                /* already defined future.ref */
                if (XL) {
                    v = getLocal(XL, S);
                    setLocal(XL, S, P);
                } else {
                    v = XS->N;
                    XS->N = P;
                }
            } else {
                /* future.ref: can be EQU as well */
                DefineSym(S, P, OFF, SYM_LOC);
                if ('=' == S[0]) {
                    ASSERT(NULL != LSYM);
                    LSYM->A = v;
                }
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
        REQUIRE(')');
    }
    if (v != F && !RANGE(C,042,046)) {
	    if (v > 63)
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

const char* GetWord(char *dst, const char *src, int n, int alf)
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

void PrintList(char *needs, Word w, Word OLDP, const char *line)
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

void CheckUndefSyms(SYM *syms, int zsyms, int nsyms)
{
    int i;
    char needs[3];

    NE = 0;
    StrSet(EC, ' ', sizeof(EC)-1);
    FREF = ' ';
    NEEDP = 'P'; NEEDL = 'L';
    for (i = zsyms; i < nsyms; i++) {
        /* unused loctab[] symbols */
        if (SYM_UNDEF == syms[i].T)
            continue;
        if (!syms[i].D) {
            EC[0] = EA_UNDSYM;
            NEEDAWS = 0;
            LREF = syms[i].N;
            PrintList(needs, 0, SM_NOADDR, syms[i].S);
        }
    }
    /* check undefined locals */
    /* iff xF contains an address, then it is unresolved   */
    /*     xB always resolved or undefined when referenced */
    S[1] = 'F'; S[2] = 0;
    for (i = locals0; i < locals0 + 10; i++) {
        if (SM_NOADDR != locals[i].F) {
            S[0] = '0' + i;
            EC[0] = EA_UNDSYM;
            NEEDAWS = 0;
            LREF = locals[i].F;
            PrintList(needs, 0, SM_NOADDR, S);
        }
    }
    EC[0] = ' ';
}

void DefineLiterals(void)
{
    int i;
    char needs[3];
    Word w;

    NE = 0;
    StrSet(EC, ' ', sizeof(EC)-1);
    FREF = ' ';
    NEEDP = 'P'; NEEDL = 'L';
    for (i = 0; i < nsymtab; i++) {
        /* TBD: local symbols */
        if ('=' == symtab[i].S[0]) {
            ASSERT(!symtab[i].D);
            EC[0] = ' ';
            NEEDAWS = 'W';
            w = symtab[i].A;
            DefineSymIdx(&symtab[i], symtab[i].S, P, ON, SYM_LOC);
            PrintList(needs, w, P, symtab[i].S);
            MemWrite(P, FULL, w); smINC(&P);
        }
    }
    EC[0] = ' ';

    CheckUndefSyms(symtab, 0, nsymtab);
}

void DumpSymbols(const char *path)
{
    int i;
    const char *s;
    FILE *fd, *LPTSAV;

    fd = fopen(path, TXT_CREATE);
    if (NULL == fd) {
        Error("CANNOT OPEN %s", path);
        exit(1);
    }
    LPTSAV = LPT; LPT = fd;

    for (i = 0; i < nsymtab; i++) {
        s = symtab[i].S;
        if (!symtab[i].D)
            continue;
        PrintLPT("%c ", symtab[i].T);
        wprint(symtab[i].N);
        PrintLPT("%10s\n", s);
    }
    fclose(LPT);
    LPT = LPTSAV;
}

void ReadSymbols(const char *path)
{
    FILE *fd;
    char line[MAX_LINE + 1], *ptr, typ;
    int n;
    Word w;

    fd = fopen(path, "rt");
    if (NULL == fd) {
        Error("CANNOT OPEN %s", path);
        exit(1);
    }
    while (!feof(fd)) {
        if (!fgets(line, sizeof(line), fd))
            break;
        n = StripLine(line);
        if (!n || n < 13)
            continue;
        ptr = NULL;
        typ = line[0];
        w = strtol(line + 2, &ptr, 8);
        if ('L' != typ || w > MAX_MEM)
            continue;
        /* +1234567890 SYMBOL */
        mapsyms[w] = StrDup(line + 13);
        /* PrintLPT("%c%010o %s\n", PLUS(w), MAG(w), mapsyms[w]); */
    }
    fclose(fd);
}

LOCSYM *XH;
void DefineLocationSym(Word w, SYMTYPE typ)
{
    XH = NULL;
    if (IsLocalSym(LOCATION)) {
        int x = localIdx(LOCATION);
        if ('H' != LOCATION[1])
            AsmError(EA_INVSYM);
        
        if (SM_NOADDR == locals[x].B) {
            locals[x].B = locals[x].H;
        }
        if (SM_NOADDR != locals[x].F) {
            ResolveAdr(locals[x].F, w);
            locals[x].F = SM_NOADDR;
        }
        XH = &locals[x];
        locals[x].H = w;
    } else
        DefineSym(LOCATION, w, ON, typ);
}

#define MAX_IFSCOPE 11
Toggle IFSCOPE[MAX_IFSCOPE];
int IFLVL;
int IFNEST;

int GetRel(char *s)
{
    static char* rels[] = { "EQ", "NE", "LT", "LE", "GT", "GE", NULL };
    int i;

    s[2] = 0;
    for (i = 0; rels[i]; i++) {
        if (!strcmp(s, rels[i]))
            return i;
    }
    AsmError(EA_UNDSYM);
    return 0;
}

void PushMacExp(void)
{
    int i;

    ASSERT(MACNARGS <= NMACARGS);

    if (MEXPLVL == MAX_MACSCOPE) {
        AsmError(EA_DEPTH);
    } else {
        MEXPLVL++;
        MEXP[MEXPLVL].EXPMAC = EXPMAC;
        MEXP[MEXPLVL].EXPBDY = EXPBDY;
        for (i = 0; i < MACNARGS; i++) {
            MEXP[MEXPLVL].VALS[i] = MACVALS[i];
        }
        /* save local env, setup new */
        MEXP[MEXPLVL].loctab0 = loctab0;
        MEXP[MEXPLVL].nloctab = nloctab;
        MEXP[MEXPLVL].locals0 = locals0;

        InitLocalSyms(locals0 + 10);
        loctab0 = nloctab;
    }
}

void PopMacExp(void)
{
    int i;

    if (0 == MEXPLVL) {
        AsmError(EA_DEPTH);
    } else {
        EXPMAC = MEXP[MEXPLVL].EXPMAC;
        EXPBDY = MEXP[MEXPLVL].EXPBDY;
        ASSERT(MACNARGS <= NMACARGS);
        for (i = 0; i < MACNARGS; i++) {
            MACVALS[i] = MEXP[MEXPLVL].VALS[i];
        }
        /* restore local env */
        CheckUndefSyms(loctab, loctab0, nloctab);

        /* Info("POPMACEXP"); */
        loctab0 = MEXP[MEXPLVL].loctab0;
        nloctab = MEXP[MEXPLVL].nloctab;
        locals0 = MEXP[MEXPLVL].locals0;
        --MEXPLVL;
    }
}

int Assemble(const char *line)
{
    Word v = 0, w = 0, OLDP, A, I, F, C;
    int i, j, n, found;
    char needs[3]; /* A|W|S P L */
    char SAV4, SAV10, SAV15;
    int rel;
    Toggle IGNORELOC, COND, DOIFNEST, SKIP;
    Comparator ci;
    MBODY *body;
    char buf[MAX_LINE+1], buf2[MAX_LINE+1];

    if (!FF || '*' == line[0]) {
        strncpy(LINE, line, MAX_LINE);
    } else {
        const char *ptr = line;
        StrSet(LINE, ' ', MAX_LINE);
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

    SAV4 = LINE[NMACNAME]; SAV10 = LINE[10]; SAV15 = LINE[15];
    LINE[10] = 0; LINE[15] = 0;
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
    NE = 0; StrSet(EC, ' ', sizeof(EC)-1); FREF = ' ';
    NEEDAWS = 0; NEEDP = 'P'; NEEDL = 0;
    XH = NULL;
    IGNORELOC = OFF; DOIFNEST = OFF;

	OLDP = P;

	SKIP = TONOFF(OFF == IFSCOPE[IFLVL]);
    TraceA("SKIP=%s",ONOFF(SKIP));
    /* check as macro */
    found = (OFF == SKIP && 0 == SAVMAC) ? FindMac(OP) : 0;
    if (found) {
        if (' ' != LOCATION[0]) {
            DefineLocationSym(P, SYM_LOC);
        }
        PushMacExp();
        LockMac(EXPMAC = found);
        for (i = 0; i < MACNARGS; i++) {
            MACVALS[i] = StrDup(macs[EXPMAC].dflt[i]);
        }
        n = GetExtSym(buf); i = 0; j = 0;
        while (n) {
            j = i;
            TraceA("i=%d arg='%s'", i, buf);
            if ('=' == CH) {
                NEXT();
                j = FindMacArg(buf);
                if (0 == j) {
                    AsmError(EA_UNDSYM);
                } else {
                    j--;
                }
                GetExtSym(buf);
                TraceA("j=%d val='%s'", j, buf);
            }
            if (j < MACNARGS) {
                Free(MACVALS[j]);
                MACVALS[j] = StrDup(buf);
            } else {
                AsmError(EA_XTRAOP);
                break;
            }
            if (',' == CH) NEXT();
            n = GetExtSym(buf);
            i++;
        }
        if (TRACEA) ShowMacExp(EXPMAC);
        EXPBDY = macs[EXPMAC].body;
        goto OpEnd;
    }
    if (!strcmp(OP, "MACR")) { /* NAME MACR P1=V1,...,P10=V10 */
        int nargs = 0;

        IGNORELOC = ON;
        MACNEST++;
        if (0 == SAVMAC) {
            if (' ' == LOCATION[0]) {
                return AsmError(EA_MISSOP);
            }
            LINE[NMACNAME] = 0;
            found = FindMac(LOCATION);
            if (found) {
                if (macs[found].locked) {
                    return AsmError(EA_INVSYM);
                }
                SAVMAC = found;
                SAVBDY = macs[SAVMAC].body;
                while (SAVBDY) {
                    body = SAVBDY->next;
                    Free(SAVBDY);
                    SAVBDY = body;
                }
                macs[SAVMAC].body = NULL;
                for (i = 0; i < macs[SAVMAC].nargs; i++) {
                    Free(macs[SAVMAC].args[i]);
                    Free(macs[SAVMAC].dflt[i]);
                }
            } else {
                if (nmacs == NMACS) {
                    AsmError(EA_TABFUL);
                    return AsmError(EA_INVSYM);
                }
                SAVMAC = ++nmacs;
            }

            StrCpy(macs[SAVMAC].S, LOCATION, NMACNAME);
            LINE[NMACNAME] = SAV4;
            n = GetExtSym(buf);
            while (n) {
                if (NMACARGS == nargs) {
                    return AsmError(EA_MISSOP);
                }
                macs[SAVMAC].args[nargs] = StrDup(buf);
                macs[SAVMAC].dflt[nargs] = EmptyStr;
                if ('=' == CH) {
                    NEXT();
                    GetExtSym(buf);
                    macs[SAVMAC].dflt[nargs] = StrDup(buf);
                }
                nargs++;
                if (' ' != CH) ENSURE(',');
                n = GetExtSym(buf);
            }
            macs[SAVMAC].nargs = nargs;
            macs[SAVMAC].locked = OFF;
            SAVBDY = NULL;
            goto OpEnd;
        }
    } else if (!strcmp(OP, "LOC ")) {
        IGNORELOC = ON;
        if (0 == SAVMAC) {
            if (!EXPMAC) {
                AsmError(EA_UNKOPC);
            } else {
                GetSym();
                while (TOK_MTY != T) {
                    DefineLocalSym(S, SYM_UNDEF);
                    if (' ' != CH) ENSURE(',');
                    GetSym();
                }
                goto OpEnd;
            }
        }
    } else if (!strcmp(OP, "ENDM")) {
        IGNORELOC = ON;
        if (MACNEST) {
            --MACNEST;
            if (0 == MACNEST) {
                if (TRACEA) ShowMacDef(SAVMAC);
                SAVMAC = 0; SAVBDY = NULL;
                goto Out;
            }
        } else {
            AsmError(EA_MISPLD);
        }
    }
    if (SAVMAC) {
        ASSERT(0 < SAVMAC && SAVMAC <= NMACS);
        body = Malloc(sizeof(MBODY));
        body->next = NULL;
        LINE[10] = SAV10; LINE[15] = SAV15;
        TraceA("LINE=%s",LINE);
        StrCpy(body->LINE, LINE, MAX_LINE);
        LINE[10] = 0; LINE[15] = 0;
        if (NULL == SAVBDY) {
            TraceA("NULL SAVBDY");
            macs[SAVMAC].body = body;
        } else {
            SAVBDY->next = body;
        }
        SAVBDY = body;
        goto Out;
    }
    if (!strcmp(OP, "IF  ")) { /* IF rel,expr1,expr2 */
        IGNORELOC = ON;
        if (OFF == SKIP) {
            DOIFNEST = ON;
            GetSym(); rel = GetRel(S);
            ENSURE(',');
            v = Expr(); ENSURE(','); w = Expr();
            ci = smCMP(v,w);
            switch (rel) {
            case 0: /*EQ*/ COND = TONOFF(  EQUAL == ci); break;
            case 1: /*NE*/ COND = TONOFF(  EQUAL != ci); break;
            case 2: /*LT*/ COND = TONOFF(   LESS == ci); break;
            case 3: /*LE*/ COND = TONOFF(GREATER != ci); break;
            case 4: /*GT*/ COND = TONOFF(GREATER == ci); break;
            case 5: /*GE*/ COND = TONOFF(   LESS != ci); break;
            default:
                COND = OFF;
            }
        }
        IFNEST++;
    } else if (!strcmp(OP, "IFC ")) { /* IFC rel,/str1/str2/ */
        IGNORELOC = ON;
        if (OFF == SKIP) {
            DOIFNEST = ON;
            GetSym(); rel = GetRel(S); if (rel > 1) AsmError(EA_UNDSYM);
            ENSURE(',');
            i = CH; NEXT();
            TraceA("IFC DELIM=%c", i);
            GetDelim(buf, i); ENSURE(i); GetDelim(buf2, i); ENSURE(i);
            COND = TONOFF(0 == strcmp(buf, buf2));
            if (1 == rel) { /*NE*/
                COND = COND ? OFF : ON;
            }
        }
        IFNEST++;
    } else if (!strcmp(OP, "IFD ")) { /* IFD sym */
        IGNORELOC = ON;
        if (OFF == SKIP) {
            DOIFNEST = ON;
            GetSym();
            COND = TONOFF(NULL != FindSym(S));
        }
        IFNEST++;
    } else if (!strcmp(OP, "ELSE")) {
        IGNORELOC = ON;
        if (IFNEST) {
            if (IFLVL == IFNEST) {
                IFSCOPE[IFLVL] = IFSCOPE[IFLVL] ? OFF : ON;
            } else {
                AsmError(EA_MISPLD);
            }
        } else {
            AsmError(EA_MISPLD);
        }
    } else if (!strcmp(OP, "ENDI")) {
        IGNORELOC = ON;
        if (IFNEST) {
            if (IFLVL == IFNEST)
                IFLVL--;
            IFNEST--;
        } else {
            AsmError(EA_MISPLD);
        }
    }
    if (SKIP)
        return ON == E;
    if (DOIFNEST) {
        if (IFLVL == MAX_IFSCOPE) {
            AsmError(EA_DEPTH);
        } else {
            IFSCOPE[++IFLVL] = COND;
        }
    }
    if (IGNORELOC)
        goto OpEnd;

    if ('*' == LOCATION[0]) {
        goto Out;
    }
    if (!strcmp(OP, "EQU ")) {
        w = Wvalue();
        if (' ' != LOCATION[0])
            DefineLocationSym(w, SYM_EQU);
        NEEDAWS = 'W'; NEEDP = 0;
    } else {
        if (!IGNORELOC && ' ' != LOCATION[0])
            DefineLocationSym(P, SYM_LOC);
        found = FindOp();
        if (found--) {
            C = opcodes[found].c0de; 
            F = BYTE(C / BYTESIZE);
            C = BYTE(C);
            
            A = Apart();
            I = Ipart();
            F = Fpart(C,F);
            if (!RANGE(C,042,046)) {
	            if (I > 63) {
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
            if (1 < IFNEST) {
                AsmError(EA_DEPTH);
            }
            if (' ' != LOCATION[0])
            	AsmError(EA_ENDLOC);
        } else
            AsmError(EA_UNKOPC);
    }
OpEnd:
    if (!E && CH && ' ' != CH) {
	    TraceA("    EXTRA OP ('%c',%d)", CH, CH);
    	AsmError(EA_XTRAOP);
	}
    if (XH) {
        XH->B = XH->H;
        XH->H = SM_NOADDR;
        XH = NULL;
    }
Out:
    if (TRACEA)
        ShowLocalSyms("AFTER");
    LINE[10] = SAV10; LINE[15] = SAV15;
    if (!strncmp(LINE + 72, "        ", 8)) {
        sprintf(LINE + 72, "L%04d000", LNO);
    }
    PrintList(needs, w, OLDP, LINE);
    return ON == E;
}

int StripLine(char *line)
{
    int n;

    n = strlen(line);
    while (n && IsCRLF(line[n-1]))
        n--;
    line[n] = 0;
    return n;
}

int AppendSubst(char *buf, int buflen, const char *word, int maxlen)
{
	char subst[MAX_LINE+1];
	int found, len;
	const char *val;
	
	found = FindMacSubst(word, subst);
	val = found ? subst : word;
	TraceA("    SUBST: '%s' => '%s'", word, val);
	len = strlen(val);
	if (buflen + len <= maxlen) {
		memcpy(buf + buflen, val, len);
	}
	return buflen + len;
}

void MacExpand(const char *line, char *buf, int maxlen)
{
	int i, j, buflen;
	char word[MAX_LINE+1];
	int ch, inside;
	
	/* indexes: line[i], word[j], buf[buflen] */
	TraceA("    MACEXPAND: START '%s'", line);
	j = 0; inside = 0; buflen = 0;
	for (i = 0; line[i]; i++) {
		ch = line[i];
		if (0 == inside) {
			if (IsAlphaExceptArrow(ch)) {
				inside = 1; j = 0;
			}
		}
		if (inside) {
			if (IsAlphaExceptArrow(ch) || IsDigit(ch)) {
				word[j] = ch; j++;
				ch = 0;
			} else {
				word[j] = 0;
				inside = 0; j = 0;
				buflen = AppendSubst(buf, buflen, word, maxlen);
			}
		}
		if (ch && buflen + 1 <= maxlen) {
			/* append except left arrow */
			if ('~' != ch) {
				buf[buflen++] = ch;
			}
		}
	}
	if (inside) {
		word[j] = 0;
		buflen = AppendSubst(buf, buflen, word, maxlen);
	}
	buf[buflen] = 0;
	TraceA("    MACEXPAND: DONE '%s'", buf);
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

    nsymtab = 0; nload = 0;
    InitLocalSyms(0);
    nloctab = 0; loctab0 = 0;

    /* Scope 0 always ON, IFLVL points to IFCOND */
    IFSCOPE[0] = ON; IFLVL = 0; IFNEST = 0;

    /* macros: 0 is empty, always from 1 */
    nmacs = 0;
    SAVMAC = 0; SAVBDY = NULL;
    EXPMAC = 0; EXPBDY = NULL;
    MACNEST = 0;

    failed = 0;
    P = 0; OPEND = OFF;

    LNO = 0; 
    while (!feof(fd)) {
        /* macro expansion */
        if (EXPBDY) {
            ASSERT(EXPMAC);
            MacExpand(EXPBDY->LINE, line, MAX_LINE);
        } else {
            if (!fgets(line, sizeof(line), fd))
                break;
        }

        n = StripLine(line);
        failed += n ? Assemble(line) : 0;
        if (OPEND)
            break;
        LNO++;

        if (EXPBDY) {
            EXPBDY = EXPBDY->next;
            if (!EXPBDY) {
                UnLockMac(EXPMAC);
                PopMacExp();
            }
        }
    }
    fclose(fd);
    Info("ASSEMBLE %s\n", failed ? "FAILED" : "DONE");

    return 0;
}


/* =============== C O N T R O L  S E C T I O N ============= */

void decode(Word C, Word F)
{
    static char *fpops[]  = { "FADD", "FSUB", "FMUL", "FDIV" };
    static char *dfpops[] = { "DADD", "DSUB", "DMUL", "DDIV" };
    static char *lops[]   = { "LADD", "LSUB" };
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
	unsigned nf;
	char *s;
	
	mnemo[0] = '\0';
	mnemo[4] = '\0';
    switch (F) {
    case 6: /*Fxxx*/
        if (RANGE(C,1,4)) {
            strcpy(mnemo, fpops[C-1]);
            return;
	    } else if (56 == C) {
            strcpy(mnemo, "FCMP");
            return;
        }
        break;
    case 7: /*Lxxx*/
        if (RANGE(C,1,2)) {
            strcpy(mnemo, lops[C-1]);
            return;
        }
        break;
    case 8: /*Dxxx*/
    	if (RANGE(C,1,4)) {
            strcpy(mnemo, dfpops[C-1]);
    	    return;
	    } else if (56 == C) {
            strcpy(mnemo, "DCMP");
    	    return;
	    }
        break;
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

Toggle OLDFLOATOP, FLOATOP;
void Status(Word P)
{
	Word w, INST, A, I, F, C, M, OP;
	int i;
    double d;
    Toggle isfix;
    char fmt;
	
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
    OLDFLOATOP = FLOATOP; FLOATOP=OFF;

    isfix = OFF; fmt = ' ';
    if ((RANGE(C,1,2) && (7 == F))              /*LADD/LSUB*/
        || (RANGE(C,1,4) && (8 == F))           /*DADD/DSUB/DMUL/DDIV*/
        || (56 == C && (7 == F || 8 == F)))     /*LCMP/DCMP*/
    {
        fmt = 'X';
        OP = M;
    } else if ((RANGE(C,1,4) && 6 == F)         /*FADD/FSUB/FMUL/FDIV*/
        || (56 == C && 6 == F))                 /*FCMP*/
    {
        d = FPToDouble(MemRead(M));
        dprint(d);
        FLOATOP=ON;
    } else if (RANGE(C,1,4)
        || RANGE(C,8,23)
        || (56 == C)
        || RANGE(C,57,63))
    {
        GetV(M, F, &OP);
        fmt = 'W';
    } else if (5 == C && 7 == F) {  /*FIX*/
        d = FPToDouble(rA);
        dprint(d);
        FLOATOP=ON;
        isfix = ON;
    } else {
        fmt = 'X';
    }
    if ('X' == fmt && OP <= MAX_MEM && mapsyms[OP]) {
        PrintLPT(mapsyms[OP]); space();
    } else {
        if ('W' == fmt) {
            wprint(OP);
        } else {
	        xprint(OP);
            Print("     ");
        }
    }
    if (OLDFLOATOP || FLOATOP) dprint(FPToDouble(rA));
	else wprint(rA);
	if (isfix) {
	    FLOATOP = OFF;
    } else if (5 == C && 6 == F) {/*FLOT*/
        FLOATOP = ON;
    }
	wprint(rX);
	for (i = 1; i <= 6; i++)
		xprint(reg[i]);

	xprint(rJ);
	Print("%c %c %09u\n", sot[OV], sci[CI], Tyme);
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
	for (i = 0; i < BYTES; i++) {
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
	Word A, I, F, C, M;
	Word w, wlo;
	int cond;
    unsigned x;

    if (IsWaiting()) {
        Tyme++;
    } else {
        if (CheckFetch(P)) {
            return Stop();
	    }
	    IR = MemRead(P);
    }
    w = MAG(IR);
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
	case 1:
	    if (CheckMemRead(M))
	        return 1;
        switch (F) {
        case 06: /*FADD*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpADD(rA, MemRead(M));
			Tyme += 2; IncTIMER(2);
            break;
        case 07: /*LADD*/
            w = MemRead(M);
            smINC(&M);
            if (CheckMemRead(M))
                return 1;
            wlo = MemRead(M);
            smLADD(&rA, &rX, rA, rX, w, wlo);
            if (CY) OV = CY;
            break;
        case 010: /*DADD*/
            if (CheckFloatOption())
                return 1;
            return FieldError(F);
            break;
        default: /*ADD*/
            if (GetV(M, F, &w)) {
                return 1;
            }
            w = smADD(rA, w); if (CY) OV = CY;
    		if (!MAG(w)) w += SIGN(rA);
    		rA = w;
            break;
		}
		break;
	case 2:
	    if (CheckMemRead(M))
	        return 1;
        switch (F) {
	    case 06: /*FSUB*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpSUB(rA, MemRead(M));
			Tyme += 2; IncTIMER(2);
            break;
        case 07: /*LSUB*/
            w = MemRead(M);
            smINC(&M);
            if (CheckMemRead(M))
                return 1;
            wlo = MemRead(M);
            smLSUB(&rA, &rX, rA, rX, w, wlo);
            if (CY) OV = CY;
            break;
        case 010: /*DSUB*/
            if (CheckFloatOption())
                return 1;
            return FieldError(F);
            break;
	    default: /*SUB*/
            if (GetV(M, F, &w)) {
                return 1;
            }
            rA = smSUB(rA, w); if (CY) OV = CY;
            break;
        }
		break;
	case 3:
	    if (CheckMemRead(M))
	        return 1;
        switch (F) {
	    case 06: /*FMUL*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpMUL(rA, MemRead(M));
			Tyme += 7; IncTIMER(7);
            break;
        case 010: /*DMUL*/
            if (CheckFloatOption())
                return 1;
            return FieldError(F);
            break;
	    default: /*MUL*/
            if (GetV(M, F, &w)) {
                return 1;
            }
    		smMUL(&rA, &rX, rA, w);
			Tyme += 8; IncTIMER(8);
            break;
		}
		break;
	case 4:
	    if (CheckMemRead(M))
	        return 1;
        switch (F) {
	    case 06: /*FDIV*/
    	    if (CheckFloatOption())
    	        return 1;
            rA = fpDIV(rA, MemRead(M));
			Tyme += 9; IncTIMER(9);
            break;
        case 010: /*DDIV*/
            if (CheckFloatOption())
                return 1;
            return FieldError(F);
            break;
	    default: /*DIV*/
            if (GetV(M, F, &w)) {
                return 1;
            }
    		smDIV(&rA, &rX, rA, rX, w);
			Tyme += 10; IncTIMER(10);
            break;
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
                Tyme += 2; IncTIMER(2);
                break;
            case  7: /*FIX*/
                if (CheckFloatOption())
                    return 1;
                rA = fpFIX(rA);
                Tyme += 2; IncTIMER(2);
                break;
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
                break;
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
				smSLAX(&rA, NULL, rA, 0, M, SHIFT);
				break;
			case 1: /*SRA*/
				smSRAX(&rA, NULL, rA, 0, M, SHIFT);
				break;
			case 2: /*SLAX*/
				smSLAX(&rA, &rX, rA, rX, M, SHIFT);
				break;
			case 3: /*SRAX*/
				smSRAX(&rA, &rX, rA, rX, M, SHIFT);
				break;
			case 4: /*SLC*/
				smSLAX(&rA, &rX, rA, rX, M, ROTATE);
				break;
			case 5: /*SRC*/
				smSRAX(&rA, &rX, rA, rX, M, ROTATE);
				break;
            case 6: /*SLB*/
                if (CheckBinaryOption())
                    return 1;
				smSLB(&rA, &rX, rA, rX, M, SHIFT);
                break;
            case 7: /*SRB*/
                if (CheckBinaryOption())
                    return 1;
				smSRB(&rA, &rX, rA, rX, M, SHIFT);
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
        if (F > MAX_DEVS-1)
            return FieldError(F);
        w = DEVNO(F);
        devOpen(w);
		if (devBusy(w)) {
			if (TYMEWARP) {
				/* busy loop? */
				if (M == P) {
					if (0 == (MIX_INTERRUPT & CONFIG) && devStuck(w)) {
						/* won't do progress */
						return MIX_ErrorLoc("UNO%02o STUCK", F);
					}
					/* head of EventQ in the near future? */
					if (EventH && (devs[EventH-1].when <= devs[w].when)) {
						unsigned evt = devs[w].when - 1;
						TraceIO("WARP JBUS TYME=%09llu DELTA=%07u", Tyme, evt);
						Tyme += evt;
						InstCount += evt;
					}
				}
			}
            rJ = P + 1;
			P = M;
            return 0;
		}
		break;
	case 35: /*IOC*/
	case 36: /*IN*/
    case 37: /*OUT*/
        if (F > MAX_DEVS-1)
            return FieldError(F);
        w = DEVNO(F);
		if (devOpen(w))
		    return 0;
        if (devBusy(w)) {
            IdleTyme++;
            if (!IsWaiting())
                Wait(devs[w].evt);
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
        if (F > MAX_DEVS-1)
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
		case 2: /*JOV*/ if ( ON == OV) { OV = OFF; cond = 1; } break;
		case 3: /*JNOV*/if (OFF == OV) cond = 1; OV = OFF; break;
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
		case 0: /*JrN*/ cond = IS_NEGATIVE(w); break;
		case 1: /*JrZ*/ cond = IS_ZERO(w); break;
		case 2: /*JrP*/ cond = IS_POSITIVE(w); break;
		case 3: /*JrNN*/cond = !IS_NEGATIVE(w); break;
		case 4: /*JrNZ*/cond = !IS_ZERO(w); break;
		case 5: /*JrNP*/cond = !IS_POSITIVE(w); break;
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
			reg[x] = smADD(reg[x], M); if (CY) OV = ON;
			if ((0 < x && x < 7) && CheckIdx(x, CY))
                return 1;
			break;
		case 1: /*DECr*/
			reg[x] = smSUB(reg[x], M); if (CY) OV = ON;
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
		    CI = smCMP(reg[x], M);
            break;
		default:
			return FieldError(F);
		}
		break;
	case 56: case 57: case 58: case 59: case 60: case 61: case 62: case 63: /*CMPr*/
	    if (CheckMemRead(M))
    	    return 1;
        switch (F) {
	    case 06: /*FCMP*/
    	    if (CheckFloatOption())
    	        return 1;
            fpCMP(rA, MemRead(M));
			Tyme += 2; IncTIMER(2);
            break;
        case 07: /*LCMP*/
            w = MemRead(M);
            smINC(&M);
            if (CheckMemRead(M))
                return 1;
            wlo = MemRead(M);
            smLSUB(&w, &wlo, rA, rX, w, wlo);
            if (!MAG(w) && !MAG(wlo))
                CI = EQUAL;
            else {
                CI = CY ? LESS : GREATER;
            }
            break;
        case 010: /*DCMP*/
            if (CheckFloatOption())
                return 1;
            return FieldError(F);
            break;
	    default:
            if (GetV(M, F, &w)) {
                return 1;
            }
            CI = smCMP(field(reg[C - 56], F), w);
            break;
		}
		break;
	}
    smINC(&P);
	return 0;
}

#define EVENT_SLICE		7

void Run(Word p)
{
    Word OLDP;
    unsigned evt, u;
    unsigned startMS;

    startMS = CurrentMS();
	P = p;
	OLDP = smADD(p, 1);
	while (IsRunning()) {
        if (!IsWaiting()) {
            if (TRACE && OLDP != P)
                Status(P);
        }
        OLDP = P;
        Step();
        if (EventH) {
			DoEvents();
		}
		if (IsNormal() && (WaitRTI || PendingH)) {
			DoInterrupts();
		}
        if (IsWaiting()) {
            P = OLDP;
            if (TYMEWARP) {
	            if (EventH) {
		            /* jump to tyme of 1st event */
					evt = devs[EventH-1].when - 1;
					TraceIO("WARP WAIT TYME=%09llu DELTA=%07u", Tyme, evt);
					Tyme += evt;
					IdleTyme += evt;
				}
			}
		} else if (IsRunning()) {
            (SIGN(OLDP) ? ctlfreq : freq)[MAG(OLDP)]++; InstCount++;
        }
	}
    if (IsHalted()) {
        /* normal HLT, process I/O events */
        while (EventH) {
	        TraceIO("HALTED TYME=%09llu EVENTH",Tyme);
	        evt = devs[EventH-1].when;
            Tyme += evt; IdleTyme += evt;
            DoEvents();
            /* TBD: if halted then INTs cannot be delivered
             * DoInterrupts();
             */
        }
        /* wait until all devs are ready */
        evt = 0;
        for (u = 0; u < MAX_DEVS; u++)
            if (!devStuck(u))
                evt = MAX(evt, devs[u].evt);
        if (evt) {
	        TraceIO("HALTED TYME=%09llu EVT=%u", Tyme, evt);
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
    unsigned i;
    FILE *fd;

    fflush(stderr);

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

    Free(ctlfreq);
    Free(ctlmem);
    Free(freq);
    Free(mem);
}

void InitMemory(void)
{
    unsigned i;

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
	    ctlmem = Malloc((MAX_MEM + 1) * sizeof(Word));
    	ctlfreq = Malloc((MAX_MEM + 1) * sizeof(unsigned short));
		for (i = 0; i < MAX_MEM + 1; i++) {
			ctlmem[i] = ctlfreq[i] = 0;
		}
    }
    mem  = Malloc((MAX_MEM + 3) * sizeof(Word));
    freq = Malloc((MAX_MEM + 3) * sizeof(unsigned short));
    mapsyms = Malloc((MAX_MEM + 3) * sizeof(char*));
    devs = Malloc(MAX_DEVS * sizeof(Device));
	for (i = 0; i < MAX_MEM + 3; i++) {
		mem[i] = freq[i] = 0;
        mapsyms[i] = NULL;
	}
	return;
}

void InitMixToAscii(void)
{
    int i;

    strcpy(m2a, cardcodes[CARDCODE].m2a);
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
    SYMNM = NULL;
    ZLITERALS = OFF;
    NTESTS = 10000L;
    TYMEWARP = OFF;
    DOEVENTS = OFF;
    VERBOSE = OFF;
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
	unsigned i;

	InstCount = 0;
	Tyme = 0; IdleTyme = 0;
    ZERO = 0;
	STATE = S_HALT; Halt();
	FLOATOP = OLDFLOATOP = OFF;

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
    PendingEventH = 0;

    InitCore();
    InitMixToAscii();
}

void Usage(void)
{
	Print("  usage: mix [-c <config>][-g [unit]][-6 <cardcode>][-ad][-s addr][-t aio][-lprvwz][-y symfile] file1...\n");
    Print("version: %s\n", MIX_VERSION);
    Print("options:\n");
    Print("    -g [unit]      push GO button on unit (def. card reader)\n");
    Print("    -c bcfimx      MIX config (also from MIXCONFIG env.var):\n");
    Print("                     b - binary MIX\n");
    Print("                     c - core memory (core.mem/core.ctl)\n");
    Print("                     f - floating-point attachment\n");
    Print("                     i - interrupt facility\n");
    Print("                     m - Mixmaster\n");
    Print("                     x - double/indirect-indexing facility\n");
    Print("\n");
    Print("    -6 bic|cdc731|dec026|dec029|mix|mix360|sixbit|x326\n");
    Print("                   select punched card code (def. mix)\n");
    Print("    -a             assemble only\n");
    Print("    -d             dump non-zero locations\n");
    Print("    -l             punch LNKLD cards\n");
    Print("    -p             punch nonzero locations in TRANS fmt\n");
    Print("    -r             free fmt MIXAL\n");
    Print("    -s address     set START address\n");
    Print("    -t aio         enable tracing: Asm,Io,Op (also MIXTRACE env.var)\n");
    Print("    -v             verbose assembling\n");
    Print("    -w             enable Tyme warp\n");
    Print("    -y symfile     specify symbol file (write/read)\n");
    Print("    -z             reuse literal constants\n");

    Print("\n");
    Print("    -d             dump FP test data\n");
    Print("    -m  N          run FP tests, cache test data\n");
    Print("    -m -N          run FP tests\n");
    Print("    -m  0 u v op   run single FP test: op(u, v) (? is FCMP)\n");
    Print("\n");

    Print("\nMIXAL error codes:\n\n");
    Print("    A   address has wrong syntax            N   invalid number\n");
    Print("    B   backward local sym undefined        O   unknown opcode\n");
    Print("    C   invalid char                        R   location out of range\n");
    Print("    D   duplicate symbol definition         S   invalid sym (eg. 2H in ADDRESS)\n");
    Print("    E   END has non-blank location          T   too big F or I spec\n");
    Print("    F   invalid field spec (3:2)            U   undefined sym other than address\n");
    Print("    H   IF/MACR scope depth                 V   overflow during expr eval\n");
    Print("    I   misplaced ELSE/ENDI/ENDM            X   extra operand\n");
    Print("    L   symbol/const/literal len            Z   symbol/macro table full\n");
    Print("    M   missing expr operand/symbol         9   linker error\n");
    Print("\n");
	exit(1);
}


void Go(unsigned d)
{
    int x;

    if (devOpen(d))
        return;

    x = devType(d);

	if (d >= MAX_DEVS || DEV_CP == x || DEV_LP == x || DEV_TT == x)
		Usage();

	devINP(d, 0);
	lastDoEventsTyme = 0; Tyme = devs[d].when;
	while (EventH) {
		Tyme++; IdleTyme++;
		DoEvents();
	}

	lastDoEventsTyme = Tyme = IdleTyme = 0;
	InstCount = 0;
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
    int MinMem, MaxMem;

	MaxMem = MAX_MEM;
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
    MinMem = (CONFIG & MIX_INTERRUPT) ? -MaxMem : 0;
    for (i = MinMem; i <= MaxMem; i += STRIDE) {
        emit = 0; minj = STRIDE; maxj = 0;
        for (j = 0; (i + j <= MaxMem) && (j < STRIDE); j++) {
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
                if (i + j <= MaxMem)
                    wprint(MemRead(i2w(i + j)));
                else
                    PrintLPT("       ");
            }
            PrintLPT("     ");
            for (j = 0; j < STRIDE; j++) {
                a = i + j;
                if (a <= MaxMem)
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

#define MIN_FP  (00001000000U)

Word myrand(void)
{
    Word w;

    w = GetRandom() & 077777U;
    w <<= 15;
    w += GetRandom() & 077777U;
    if (GetRandom() & 1)
        w += SM_SIGN;
    if (!MAG(w))
        return w;
    while (0 == (w & 0777777U)) {
        w += GetRandom() & 07777U;
    }
    while (!IS_NORM(w)) {
        w = SIGN(w) + MSBYTE(w) + (w & 00000777777U) * BYTESIZE;
    }
    ASSERT(IS_NORM(w));
    return SIGN(w) + MAG(w);
}

Word *TestData;
char *MARG[3];
unsigned SRAND;

void TestFPOp(char op, int bincmp)
{
    int i, cnt, nrounded, nfailed;
    Word u, v, w0, w1;
    double du, dv, dw0, dw1;
    char buf0[32], buf1[32];
    double (*fp2d)(Word);
    Toggle waserr;
    long N;

    waserr = OFF;
    InitRandom(SRAND);
    Info("=== T E S T I N G  F P %c ===", op);
    fp2d = FPToDouble2;
    cnt = nfailed = nrounded = 0;
    N = NTESTS;
    for (i = 0; i < N; i++) {
        LASTROUNDED = OFF;
        if ('/' == op || NULL == TestData) {
            u = myrand();
            v = myrand();
        } else {
            u = TestData[2*cnt    ];
            v = TestData[2*cnt + 1];
        }
        du = fp2d(u);
        dv = fp2d(v);
        dw0 = 0.0;
        switch (op) {
        case '+': dw0 = du + dv; break;
        case '-': dw0 = du - dv; break;
        case '*': dw0 = du * dv; break;
        case '/':
            if (!MAG(v)) {
                N++;
                continue;
            }
            dw0 = du / dv;
            break;
        case '?':
            w0 = CompareDouble(du, dv);
            break;
        }
        if ('?' != op) {
            w0 = RoundDoubleToFP(dw0);
        }
        w1 = 0;
        OV = OFF;
        switch (op) {
        case '+': w1 = fpADD(u, v); break;
        case '-': w1 = fpSUB(u, v); break;
        case '*': w1 = fpMUL(u, v); break;
        case '/':
            w1 = fpDIV(u, v);
            break;
        case '?':
            fpCMP(u, v); w1 = CI;
            break;
        }
        if ('/' == op && NULL != TestData) {
            TestData[2*cnt    ] = u;
            TestData[2*cnt + 1] = v;
        }
        cnt++;
        if (0 == (cnt & SM_MASK(17))) {
            if (waserr) {
                waserr = OFF;
                nl();
            }
            PrintLPT("*");
        }
        if (LASTROUNDED)
            nrounded++;
        if (bincmp || '?' == op) {
            if (w0 == w1)
                continue;
            dw0 = fp2d(w0);
            dw1 = fp2d(w1);
        } else {
            dw0 = fp2d(w0);
            dw1 = fp2d(w1);
            sprintf(buf0, "%.5e", dw0);
            sprintf(buf1, "%.5e", dw1);
            if (!strcmp(buf0, buf1))
                continue;
        }
        nfailed++;
        nl(); PrintLPT("0 "); wprint(u); wprint(v);
        if ('?' == op) {
            PrintLPT("%c / DU(%+.5e) %c DV(%+.5e) = W0(%c) W1(%c) %.5e", op, du, op, dv, sci[w0], sci[w1], fabs(du - dv));
        } else {
            PrintLPT("%c / DU(%+.5e) %c DV(%+.5e) = W0(%+.5e) W1(%+.5e) %.5e", op, du, op, dv, dw0, dw1, fabs(dw0 - dw1));
        }
        waserr = ON;
    }
    nl();
    Info("  Count: #%d", cnt);
    Info(" Failed: #%d", nfailed);
    Info("Rounded: #%d", nrounded);
}

/* +/-1234567890 octal */
/* 123456789_    decimal, 11-punch */
Word StrToW(const char *s)
{
    char tmp[MAX_LINE + 1], *ptr;
    long w;
    int sign, base;

    strcpy(tmp, s);
    sign = 0; base = 8;
    if ('+' == *tmp || '-' == *tmp) {
        base = 8;
    } else if (10 == strlen(tmp)) {
        ptr = strchr("0123456789~JKLMNOPQR", tmp[9]);
        if (ptr && !IsDigit(*ptr)) {
            sign = 1; tmp[9] = *(ptr - 10);
        }
        base = 10;
    } else {
        Error("UNKNOWN NUMBER FORMAT '%s'", s);
        return SM_SIGN + SM_WORD;
    }
    w = strtol(&tmp[0], &ptr, base);
    return (SM_WORD & w) + (sign ? SM_SIGN : 0);
}

void utoa(char *buf, int len, unsigned u)
{
    int i;

    for (i = len-1; i >= 0; i--) {
        buf[i] = u % 10 + '0'; u /= 10;
    }
}

void TestFP(void)
{
    const int bincmp = 1;

    SRAND = GetSeed();
    Info("SRAND=%u", SRAND);
    if (DUMP) {
        int i, j;

        InitRandom(SRAND);
        LPT = fopen("fptst.dek", "w+");
        if (NULL == LPT) {
            Error("CANNOT OPEN FPTST.DEK");
            exit(1);
        }
        for (i = 0; i < NTESTS; i++) {
            char buf[MAX_LINE + 1];
            Word u, v, ret[7];

            u = myrand();
            v = myrand();

            ret[0] = u;
            ret[1] = v;
            ret[2] = fpADD(u, v);
            ret[3] = fpSUB(u, v);
            ret[4] = fpMUL(u, v);
            ret[5] = fpDIV(u, v);
            fpCMP(u, v); ret[6] = i2w(CI - 1);

            sprintf(buf, "INPUT1    ");
            for (j = 1; j <= 7; j++) {
                Word w = ret[j-1];
                char *ptr = buf + 10 * j;
                /* sprintf(ptr , "%010d", MAG(w)); */
                utoa(ptr, 10, MAG(w));
                OverPunch(ptr + 9, SIGN(w), MAG(w));
            }
            buf[MAX_LINE] = 0;
            PrintLPT("%s\n", buf);
        }
        PrintLPT("INPUT0    %*s\n", 70, " ");
        fclose(LPT);
        exit(0);
    }
    LASTROUNDED = OFF;
    if (0 == NTESTS) {
        char op;
        Word u, v, w0, w1;
        double du, dv, dw0;

        VERBOSE = ON;
        op = *MARG[2];
        u = StrToW(MARG[0]);
        v = StrToW(MARG[1]);
        du = FPToDouble2(u);
        dv = FPToDouble2(v);
        dw0 = 0.0; w1 = 0;
        switch (op) {
        case '+':
            dw0 = du + dv;
            w1 = fpADD(u, v);
            break;
        case '-':
            dw0 = du - dv;
            w1 = fpSUB(u, v);
            break;
        case '*':
            dw0 = du * dv;
            w1 = fpMUL(u, v);
            break;
        case '/':
            dw0 = du / dv;
            w1 = fpDIV(u, v);
            break;
        case '?':
            w0 = CompareDouble(du, dv);
            fpCMP(u, v); w1 = CI;
            break;
        }
        PrintLPT("DU(%.5e) %c DV(%.5e) = ", du, op, dv);
        if ('?' == op) {
            PrintLPT("W0(%c) W1(%c) %.5e\n", sci[w0], sci[w1], fabs(du - dv));
        } else {
            w0 = RoundDoubleToFP(dw0);
            PrintLPT("DW(%.5e)\n", dw0);
            PrintLPT(" FP W0=%.5e\n", FPToDouble(w0));
            PrintLPT("MIX W1=%.5e\n", FPToDouble(w1));
        }
        exit(0);
    }
    TestData = NULL;
    if (NTESTS > 0) {
        TestData = (Word*)Malloc(NTESTS * 2 *sizeof(Word));
    } else {
        Info("TESTDATA NOT CACHED");
        NTESTS = -NTESTS;
    }

    TestFPOp('/', bincmp);
    TestFPOp('*', bincmp);
    TestFPOp('-', bincmp);
    TestFPOp('+', bincmp);
    /* TestFPOp('?', bincmp); */

    exit(0);
}

long satol(const char *s)
{
    int n;
    long scale;
    char tmp[MAX_LINE+1];

    strncpy(tmp, s, MAX_LINE);

    scale = 1L;
    n = strlen(tmp);
    switch (tmp[n-1]) {
    case 'k': scale = 1000L; break;
    case 'M': scale = 1000000L; break;
    }
    if (1L != scale)
        tmp[n-1] = 0;
    return scale * atol(tmp);
}


#define NASMFILES 32

int main(int argc, char*argv[])
{
	int i, j, u;
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
        case '6':
			if (i + 1 < argc) {
    			arg = argv[++i];
                for (j = 0; cardcodes[j].opt; j++) {
                    if (0 == strcasecmp(cardcodes[j].opt, arg)) {
                        CARDCODE = cardcodes[j].idx;
                        break;
                    }
                }
                if (cardcodes[j].opt)
                    continue;
			}
			break;
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
        case 'm':
			if (i + 1 < argc) {
                arg = argv[++i];
                NTESTS = satol(arg);
			}
            if (0 == NTESTS) {
                for (j = 0; j < 3; j++) {
                    MARG[j] = "";
			        if (i + 1 < argc) {
                        arg = argv[++i];
                        MARG[j] = arg;
			        } else {
                        Usage();
                    }
                }
            }
            TestFP();
			continue;
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
        case 'v': VERBOSE = ON; continue;
        case 'w': TYMEWARP = ON; continue;
        case 'y':
            if (i + 1 < argc) {
                SYMNM = argv[++i];
                continue;
            }
            break;
		case 'z': ZLITERALS = ON; continue;
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
    if (SYMNM) {
        if (nasmfiles)
            DumpSymbols(SYMNM);
        else
            ReadSymbols(SYMNM);
    }

    if (SM_NAN == START)
        START = 0;

    if (TRANS)
        PunchTrans();

	/* TBD: when interrupt facility is installed
	 *		the STATE should be S_CONTROL?
     */
    STATE = (CONFIG & MIX_INTERRUPT) ? S_CONTROL : S_NORMAL;
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
        Print("%*sTOTAL INSTRUCTIONS EXECUTED:     %09llu\n", 30, "", InstCount);
        Print("%*sTOTAL ELAPSED TYME:              %09lluu (%09lluu IDLE) (%5.1lf KIPS)\n", 30, "", Tyme, IdleTyme, kipsRate);

        Print("%*sTOTAL ELAPSED TIME:              %9.3lfs                  (%5.1lf MIPS)\n", 30, "", elapsedSeconds, mipsRate);
    }
	return 0;
}

/* vim: set ts=4 sw=4 et: */
