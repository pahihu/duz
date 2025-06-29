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
 *
 *  TODO
 *  ====
 *  - assembler: free fmt
 *  - simplify disassembler mnemonics
 *
 *  Instruction extensions
 *  ======================
 *  - standard: 1.3.1 pp.120, 1.3.2 pp.141
 *  - binary: AND OR XOR NEG XCH SLB SRB JrE JrO (Ex.2.5-28 pp.454) [f]
 *  - float: FLOT FIX FADD FSUB FMUL FDIV FCMP 4.2.x
 *  - interrupt: INT Ex.1.4.4-18 pp.224 [d]
 *  - master: XEQ CPMr Ex.1.3.1-25 pp.139 (solution pp.504) [cgh]
 *  - double/indirect-indexing: Ex.2.2.2-5 (solution pp.536) [b]
 *  - real-time clock [e]
 *  
 *
 *  History:
 *  ========
 *  250629AP    reworked options, Knuth or Stanford MIX/360 charset
 *              fixed save CORE
 *              added SLB, SRB, JrE, JrO, CPMr
 *              disable nested XEQ
 *              added F check for I/O ops
 *              added Mixmaster: 32KW memory + 64 I/O devices
 *              added double/indirect-addressing
 *  250628AP    fixed DEV_TT handling
 *              fixed JNE 'F' error
 *              report undefined syms
 *              added AND, OR, XOR, NEG, XCH, XEQ
 *              added CONFIG options: bcfgimx
 *  250627AP    wait for I/O completion on HLT
 *              asm error handling
 *              fixed LNKLD card
 *              fixed smMPY, smDIV, simplified ENTr, fixed shift
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
unsigned MAX_MEM;
#define TRACE       mem[MAX_MEM+1]
#define TIMER       mem[MAX_MEM+2]

Word reg[10], *mem, P;
Toggle OT;
enum {LESS, EQUAL, GREATER} CI;
Toggle TRANS, LNKLD, DUMP, STAN;
char TRANSNM[5+1];

FILE *LPT;
unsigned Tyme, IdleTyme, InstCount, TraceCount;
unsigned short *freq;
MachineState STATE, STATESAV;
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



/* ============== S M  A R I T H M E T I C ================== */

#define SM_MSB		(1U << 29)
#define SM_NAN      (1U << 30)
#define SM_SIGN		(1U << 31)
#define SM_MASK(x)	((1U << (x)) - 1)
#define A_MASK      SM_MASK(12)
unsigned IX_MASK;
#define SM_MINUS1	(SM_SIGN + 1U)

#define SIGN(x)		(SM_SIGN & (x))
#define	SM_WORD		SM_MASK(30)
#define MSB(x)		((x) & SM_MSB)
#define MAG(x)		((x) & SM_WORD)
#define R(x)		((x) & 07)
#define L(x)		R((x) >> 3)
#define FIELD(x,y)	((R(x) << 3) + R(y))
#define	FULL		FIELD(0,5)
#define	BYTE(x)		((x) & 077)
#define BYTES		5
#define TRACK(x)    BYTE((x) >> 6)
#define PLUS(x)     (SIGN(x) ? '-' : '+')
#define ONOFF(x)    ((x) ? "ON ": "OFF")
#define ASONOFF(x)  ((x) ? ON : OFF)



/* ================= L P T  O U T P U T ==================== */

void nl(void)
{
    fprintf(LPT, "\n");
}

void space(void)
{
	fprintf(LPT, " ");
}

void spaces(int n)
{
	int i;

	for (i = 0; i < n; i++)
		space();
}

void bprint(Byte w)
{
	fprintf(LPT, "%02o ", w);
}

void aprin(Word w)
{
	fprintf(LPT, "%05o", MAG(w));
}

void aprint(Word w)
{
	aprin(w); space();
}

void wprint(Word w)
{
	fprintf(LPT, "%c%010o ", PLUS(w), MAG(w));
}

void xprin(Word w)
{
	fprintf(LPT, "%c%05o", PLUS(w), MAG(w));
}

void xprint(Word w)
{
    xprin(w); space();
}

void aprint4(Word w)
{
	fprintf(LPT, "%c%04o ", PLUS(w), MAG(w));
}

void dprin(int d)
{
    fprintf(LPT, "%d ", d);
}

void cprin(int ch)
{
	fprintf(LPT, "%c", ch);
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
	Word z, tmp;

	CY=OFF;
	if (SIGN(x) == SIGN(y)) {
		z = MAG(x) + MAG(y);
		if (z > SM_WORD) {
			z = MAG(z); CY = ON;
		}
		return SIGN(x) + z;
	}
	if (SIGN(y)) {
		tmp = y; y = x; x = tmp;
	}
		
	if (MAG(y) >= MAG(x))
		return MAG(y) - MAG(x);
	return SIGN(x) + MAG(x) - MAG(y);
}

Word smSUB(Word x, Word y)
{
	return smADD(x, smNEG(y));
}

Word smSLAX(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;
	
    shmt = shmt % 60;
	for (i = 0; i < shmt; i++) {
		sav = MSB(a);
		a = MAG(a << 1);
		if (MSB(x))
			a++;
		x = MAG(x << 1);
		if (circ && sav)
			x++;
	}
	*pa = a;
	if (px) *px = x;
	return a;
}

Word smSRAX(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
{
	int i, sav;
	
    shmt = shmt % 60;
	for (i = 0; i < shmt; i++) {
		sav = 1 & x;
		x >>= 1;
		if (1 & a)
			x += SM_MSB;
		a >>= 1;
		if (circ && sav)
			a += SM_MSB;
	}
	*pa = a;
	if (px) *px = x;
	return a;
}


#define LO(x)	(077777 & (x))
#define HI(x)	LO((x) >> 15)

/* result sign is both a/x, is algebraic: +/+ or -/- is + */
void smMPY(Word *pa, Word *px, Word a, Word x)
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
    smSLAX(&ma, &mx, ma, mx, 1, 0);
	for (i = 0; i < 30; i++) {
		d = 0;
		if (ma >= mv) {
			d = 1; ma -= mv;
		}
		smSLAX(&ma, &mx, ma, mx, 1, 0); mx += d;
	}
    ma >>= 1;
    /* rX: ma - rem, rA: mx - quo */
	if (SIGN(a) != SIGN(v))
		mx += SM_SIGN;

	ma += SIGN(a);
    
	*pquo = mx;
	if (prem) *prem = ma;
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

    if (TRACEIO)
        fprintf(stderr, "-I-MIX: LOC=%04o AWAKE AT %07u\n", P, Tyme);

    STATE = STATESAV;
    STATESAV = S_HALT;
    WaitEvt = 0;
}

void WaitFor(unsigned evt)
{
    ASSERT(evt > Tyme);
    ASSERT(0 == WaitEvt);
    ASSERT(S_HALT == STATESAV);

    if (TRACEIO)
        fprintf(stderr, "-I-MIX: LOC=%04o WAIT UNTIL %07u\n", P, evt);

    WaitEvt = evt;
    STATESAV = STATE;
    STATE = S_WAIT;
}

int Waiting(void)
{
    return S_WAIT == STATE;
}

int Halted(void)
{
    return S_HALT == STATE;
}


int CheckBinary(void)
{
    if (0 == (CONFIG & MIX_BINARY)) {
        fprintf(stderr, "-E-MIX: NOT A BINARY MIX\n");
        return 1;
    }
    return 0;
}

int CheckFloat(void)
{
    if (0 == (CONFIG & MIX_FLOAT)) {
        fprintf(stderr, "-E-MIX: NO FLOATING POINT ATTACHMENT INSTALLED \n");
        return 1;
    }
    return 0;
}

int CheckInterrupt(void)
{
    if (0 == (CONFIG & MIX_INTERRUPT)) {
        fprintf(stderr, "-E-MIX: NO INTERRUPT FACILITY INSTALLED\n");
        return 1;
    }
    return 0;
}

int CheckMaster(void)
{
    if (0 == (CONFIG & MIX_MASTER)) {
        fprintf(stderr, "-E-MIX: NOT A MIXMASTER\n");
        return 1;
    }
    return 0;
}


/* =================== M E M O R Y ========================== */


int CheckAddr(Word a, char *msg)
{
	int ret;
	
	ret = 0;
	if (SIGN(a) || MAG(a) > MAX_MEM+2) {
		ret = 1;
		if (msg) {
			fprintf(stderr, "-E-MIX: LOC=%04o M=%c%010o INV.MEMORY ADDRESS %s\n", P, PLUS(a), MAG(a), msg);
        	ret = Stop();
    	}
	}
    return ret;
}

#define CheckMemRead(a)     CheckAddr(a, "MEMORY READ")

Word MemRead(Word a)
{
	Tyme++; TIMER++;
	return mem[MAG(a)];
}

#define CheckMemWrite(a)    CheckAddr(a, "MEMORY WRITE")

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
	Tyme++; TIMER++;
    a = MAG(a);
    mem[a] = WriteField(mem[a], f, w);
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

	src = MAG(src); dst = MAG(dst);
		
	for (i = 0; i < n; i++)
		mem[dst++] = mem[src++];
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
	FILE *fd, *fdout;
	unsigned pos;
	unsigned max_pos;
	unsigned evt;
} Device;

#define IO_SLOTS   21
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
char knuth_m2a[64+1] = " ABCDEFGHI~JKLMNOPQR|_STUVWXYZ0123456789.,()+-*/=$<>@;:'????????";
char  stan_m2a[64+1] = " ABCDEFGHI~JKLMNOPQR|_STUVWXYZ0123456789.,()+-*/=$<>@;:'\"%&#c!^?";
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

#define	DEV_MT 0
#define	DEV_DK 1
#define DEV_DR 2
#define	DEV_CR 3
#define	DEV_CP 4
#define	DEV_LP 5
#define DEV_TT 6
#define	DEV_PT 7

#define CARD_READER  16
#define LP_BLOCK 24
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
	{    "tape", BIN_RWRITE,   NULL,   NULL, 17280, 100, 0,  3056,    3056,    0,     859 },    /* DEV_MT */
	{    "disk", BIN_RWRITE,   NULL,   NULL,  4096, 100, 0,   833,     833, 6666,       0 },    /* DEV_DK */
    {    "drum", BIN_RWRITE,   NULL,   NULL,   512, 100, 0,   333,    1333, 2666,       0 },    /* DEV_DR */
	{  "reader", TXT_RDONLY, cr_a2m,   NULL,     0,  16, 0, 50000,       0,    0,       0 },    /* DEV_CR */
	{   "punch", TXT_APPEND,   NULL, cr_m2a,     0,  16, 1,     0,  100000,    0,       0 },    /* DEV_CP */
	{ "printer", TXT_APPEND,   NULL,    m2a,     0,  24, 1,     0,   21054,    0,     833 },    /* DEV_LP */
    {      NULL,       NULL,    a2m,    m2a,     0,  14, 1,     0, 1166667,    0, 1166667 },    /* DEV_TT */
	{   "ptape", TXT_RWRITE,    a2m,    m2a,  1707,  14, 0, 23333,  116667,    0,   23333 },    /* DEV_PT */
};


typedef enum {DO_NOTHING, DO_IOC, DO_IN, DO_OUT} EventType;

typedef struct __Event {
    EventType what;
    unsigned when;
    unsigned LOC;
    unsigned M;
    int next;
} Event;
Event *events;
int EventH;

int devIdx(int u);
void blkSeek(int u, unsigned pos);
void blkRead(int u, unsigned adr, Byte *cvt);
void blkWrite(int u, unsigned adr, char *cvt);


static char *sio[] = {
	"DOIO.NOP",
	"DOIO.IOC",
	"DOIO.IN",
	"DOIO.OUT"
};

void doIO(int u)
{
    Word M, LOC;
    int x;

    ASSERT(EventH != u+1);
    ASSERT(0 == events[u].next);
    ASSERT(DO_NOTHING != events[u].what);

    LOC = events[u].LOC;
    M = events[u].M;
    x = devIdx(u);

	if (TRACEIO)
    	fprintf(stderr, "-I-MIX: %07u LOC=%04o UNO=%02o/%04o %s\n", Tyme, LOC, u, M, sio[events[u].what]);
    	
    switch (events[u].what) {
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
    }
    events[u].what = DO_NOTHING;
}

void DoEvents(void)
{
	int u;
	
	while (EventH && events[EventH-1].when <= Tyme) {
		u = EventH-1;
		EventH = events[u].next;
		events[u].next = 0;
		doIO(u);
	}
}


void Schedule(unsigned delta, int u, EventType what, Word M)
{
    int i, p;
    unsigned when;

    ASSERT(0 <= EventH && EventH <= MAX_DEVS);

    ASSERT(0 == events[u].next);
    ASSERT(DO_NOTHING == events[u].what);

    /* unit is busy until delta */
    devs[u].evt = Tyme + delta;

    /* do actual I/O at delta/2 */
    when = Tyme + delta / 2;
    events[u].what = what;
    events[u].when = when;
    events[u].LOC = P;
    events[u].M = M;

    i = EventH;
    while (i && events[i-1].when <= when) {
    	p = i;
        i = events[i-1].next;
    }
    if ((0 == EventH) || (EventH == i)) { /* empty or head */
        events[u].next = EventH;
        EventH = u+1;
    } else if (0 == events[p-1].next) /* tail */
        events[p-1].next = u+1;
    else { /* middle */
        events[p-1].next = u+1;
        events[u].next = i;
    }
    
    if (TRACEIO) {
	    fprintf(stderr, "-I-MIX: *********** SCHEDULED I/O ***********\n");
	    i = EventH;
	    while (i) {
		    p = i-1;
		   	fprintf(stderr, "-I-MIX: %07u LOC=%04o UNO=%02o/%04o %s\n",
		   		events[p].when, events[p].LOC, p, events[p].M, sio[events[p].what]);
		    i = events[p].next;
	    }
	    fprintf(stderr, "-I-MIX: *************************************\n");
    }
}


int devIdx(int u)
{
    u = u % IO_SLOTS;

    if (u <= 15)
        return u > 13 ? 3: u / 8;
    return u - 13;
}

Word Pack(Byte *buf, int offs)
{
	Word w;
	int i;
	
	w = 0;
	for (i = 0; i < BYTES; i++)
		w = (w << 6) + buf[offs + i];
	return w;
}


void UnPack(Word w, Byte *buf, int offs)
{
	int i;
	
	for (i = BYTES; i >= 1; i--) {
		buf[offs + i - 1] = BYTE(w);
		w >>= 6;
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
    if (Waiting()) {
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

void blkRead(int u, unsigned adr, Byte *cvt)
{
	unsigned ret, n;
	int i, j, x;
    char tmp[LP_BLOCK * BYTES + 1];
	Byte buf[LP_BLOCK * BYTES + 1];
	unsigned Blk_size;
    char c, *ptr;
	
	x = devIdx(u);
	Blk_size = IOchar[x].blk_size;
	
	devs[u].pos++;
	if (NULL == cvt) {
		ret = fread(&mem[adr], sizeof(Word), Blk_size, devs[u].fd);
		if (ret != Blk_size)
			goto ErrOut;
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


    // tmp[n] = '\0';
    // fprintf(stderr,"{%s}\n", tmp);
	for (i = 0; i < n; i++)
		buf[i] = cvt[(int) tmp[i]];

	j = 0;
	for (i = 0; i < Blk_size; i++) {
		mem[adr + i] = Pack(buf, j);
		j += BYTES;
	}
	return;
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o BLKREAD FAILED\n", P, u, adr);
	devError(u);
}

void blkDump(int u, unsigned adr)
{
    int i, j, n, x;
    unsigned Blk_size;
	Byte tmp[LP_BLOCK * BYTES];

    if (!TRACEIO)
        return;

	x = devIdx(u);
	Blk_size = IOchar[x].blk_size;

	n = Blk_size * BYTES;
	j = 0;
	for (i = 0; i < Blk_size; i++) {
		UnPack(mem[adr + i], tmp, j);
		j += BYTES;
	}
    fprintf(stderr, "-I-MIX: UNO=%02o/%04o BLK.OUT BUF=", u, adr);
	for (i = 0; i < n; i++) {
        fprintf(stderr, " %02o", tmp[i]);
		// buf[i] = cvt[tmp[i]];
    }
    fprintf(stderr, "\n");
}

void blkWrite(int u, unsigned adr, char *cvt)
{
	unsigned ret, n;
	int i, j, x;
	Byte tmp[LP_BLOCK * BYTES];
    char buf[LP_BLOCK * BYTES + 1 + 1];     /* + CR/LF + NUL */
	unsigned Blk_size;
    FILE *fd;
	
	x = devIdx(u);
	Blk_size = IOchar[x].blk_size;
    fd = devs[u].fdout ? devs[u].fdout : devs[u].fd;
	
	devs[u].pos++;
	if (NULL == cvt) {
		ret = fwrite(&mem[adr], sizeof(Word), Blk_size, fd);
		if (ret != Blk_size)
			goto ErrOut;
		return;
	}

	n = Blk_size * BYTES;
	j = 0;
	for (i = 0; i < Blk_size; i++) {
		UnPack(mem[adr + i], tmp, j);
		j += BYTES;
	}
	for (i = 0; i < n; i++) {
		buf[i] = cvt[tmp[i]];
    }

    if (IOchar[x].cr) {
        buf[n++] = '\n';
    }
    buf[n] = 0;

	ret = fwrite(buf, sizeof(char), n, fd);
    if (TRACEIO)
        fprintf(stderr, "-I-MIX: UNO=%02o/%04o BLKWRITE BUF='%s'\n", u, adr, buf);
	if (ret == n) {
        fflush(fd);
		return;
    }
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o BLKWRITE FAILED\n", P, u, adr);
	devError(u);
}


void blkSeek(int u, unsigned pos)
{
	int x;
	
	if (pos != devs[u].pos) {
		x = devIdx(u);
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
		
	x = devIdx(u);

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
    if (TRACEIO) {
        fprintf(stderr, "-I-MIX: LOC=%04o UNO=%02o INIT %s\n", P, u, devname);
    }
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
		fprintf(stderr,"-E-MIX: %s init failed", devname);
		devError(u);
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
	
    if (TRACEIO)
	    fprintf(stderr, "-I-MIX: LOC=%04o UNO=%02o/%04o OP.IOC\n", P, u, M);

	x = devIdx(u);	
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
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.IOC UNSUPPORTED\n", P, u, M);
	devError(u);
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
	
	x = devIdx(u);
    if (CheckAddr(M, "IN BUFFER")
        || CheckAddr(M + IOchar[x].blk_size, "IN BUFFER END"))
    {
        Stop();
        return;
    }
	
	M = MAG(M);
	delta = 0;
	
    if (TRACEIO)
	    fprintf(stderr, "-I-MIX: LOC=%04o UNO=%02o/%04o OP.IN\n", P, u, M);

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
        	delta += IOchar[x].rot_tyme * Diff(Tyme & 63, BYTE(rX)) / 64.0;
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
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.IN %s\n", P, u, M, errmsg);
	devError(u);
}


void devOUT(Word u,Word M)
{
	int x;
	char *errmsg = NULL;
    unsigned delta;
	
	x = devIdx(u);
	if (CheckAddr(M, "OUT BUFFER")
        || CheckAddr(M + IOchar[x].blk_size, "OUT BUFFER END"))
    {
        Stop();
        return;
    }
	
	M = MAG(M);
	delta = 0;

    if (TRACEIO)
	    fprintf(stderr, "-I-MIX: LOC=%04o UNO=%02o/%04o OP.OUT\n", P, u, M);

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
            delta += IOchar[x].rot_tyme * Diff(Tyme & 63, BYTE(rX)) / 64.0;
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
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.OUT %s\n", P, u, M, errmsg);
	devError(u);
}

#define MM(c,f)	(((f) << 6) + (c))
static struct {
	char *nm;
	int  c0de;
} opcodes[] = {
	{"NOP ", MM(00,05)},
	{"ADD ", MM(01,05)},
	{"SUB ", MM(02,05)},
	{"MUL ", MM(03,05)},
	{"DIV ", MM(04,05)},

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
char *stok[] = { "ERR", "LOC", "NUM", "SYM"};
enum {TOK_ERR, TOK_MTY, TOK_LOC, TOK_NUM, TOK_SYM} T; /* token type */
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
    fprintf(stderr, "-E-MIX: LOC=%04o F=%02o INVALID FIELD\n", P, F);
    return Stop();
}

int GetV(Word M, Byte F, Word *ret)
{
    if (L(F) > 5 || R(F) > 5 || L(F) > R(F))
        return FieldError(F);
    if (CheckMemRead(M)) {
        return 1;
    }
    *ret = field(MemRead(M),F);
    return 0;
}

int LinkLoad(Word adr, Word w)
{
	Word nadr;
	int ret = 0;
	
    fprintf(stderr, "-I-MIX: LINKLOAD");
	while (SM_MINUS1 != adr) {
	    if (GetV(adr, FIELD(0, 2), &nadr)) {
		    ret = 1;
		    break;
	    }
	    MemWrite(adr, FIELD(0, 2), w);
        fprintf(stderr, " %04o/%04o", MAG(adr), MAG(w));
		if (adr == nadr) {
			ret = 1;
			break;
		}
		adr = nadr;
	}
	fprintf(stderr, "\n");
	return ret;
}

void PunchLinkLoad(FILE *fout)
{
    int i, j, n;

    // LNKLDn  12341234...
    for (i = 0; i < nload; i += 18) {
        n = MIN(nload - i, 18);
        fprintf(fout, "LNKLD%d  ", n / 2);
        for (j = 0; j < 18; j += 2) {
            if (j < n) {
                fprintf(fout, "%04d%04d", LOAD[i + j], LOAD[i + j + 1]);
                LinkLoad(LOAD[i + j], LOAD[i + j + 1]);
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
    int i, n, d, isnum;

    if (CH && '*' == CH) {
        NEXT();
        T = TOK_LOC;
        return 0;
    }
    n = 0;
    isnum = 1;
    MemSet(S, ' ', sizeof(S)-1);
    while (CH && ((d = IsDigit(CH)) || IsAlpha(CH))) {
        isnum = isnum && d;
        if (n < 10)
            S[n] = CH;
        n++; NEXT();
    }
    if (n > 9)
        AsmError(EA_MAXLEN);
    if (0 == n)
        T = TOK_MTY;
    else if (isnum) {
        N = 0;
        for (i = 0; i < n; i++)
            N = 10 * N + S[i] - '0';
        T = TOK_NUM;
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
    if (TRACEA) {
        fprintf(stderr, "-I-MIX:     FIND.SYM='%s' RESULT=%d\n", S, found);
        if (found)
            fprintf(stderr, "-I-MIX:     N=%d D=%s\n", w2i(syms[found-1].N), ONOFF(syms[found-1].D));
    }
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

	adr = SM_MINUS1;
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
        if (TRACEA) {
	        N = syms[found-1].N;
	        fprintf(stderr, "-I-MIX:     %s '%s' DEFINED %c%05o (CHAIN %c%05o)\n",
	        	islocal ? " LOCAL.SYM" : "FUTURE.REF", S, PLUS(w), MAG(w), PLUS(N), MAG(N));
        }
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
        if (SM_MINUS1 != adr) {
	        if (nload >= NLOAD) {
		        fprintf(stderr, "-E-MIX: LOADER TABLE FULL\n");
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
        fprintf(stderr, "-E-MIX: SYMBOL TABLE FULL\n");
        return AsmError(EA_DUPSYM);
    }
    if (TRACEA) {
        fprintf(stderr, "-I-MIX:     N=%d\n", w2i(w));
        fprintf(stderr, "-I-MIX:     D=%s\n", ONOFF(defd));
    }
    strcpy(syms[nsyms].S, S);
    syms[nsyms].N = w;
    syms[nsyms].A = SM_MINUS1;
    syms[nsyms].D = defd;
    nsyms++;
    LSYM = nsyms;
    return 0;
}

int DefineSym(char *S, Word w, Toggle defd)
{
    int found;

    if (TRACEA)
        fprintf(stderr, "-I-MIX:     DEFINE SYM '%s'\n", S);
    LSYM = found = FindSym(S);
    return DefineSymIdx(found, S, w, defd);
}

void InitLocals(char typ)
{
    int i;

    MemSet(S, ' ', sizeof(S)-1);
    for (i = 0; i < 10; i++) {
        S[0] = '0' + i;
        S[1] = typ;
        DefineSym(S, 'B' == typ ? SM_NAN : SM_MINUS1, OFF);
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

    fprintf(stderr, "-I-MIX: **** LOCALS %s ****\n", msg);
    for (i = 0; i < 10; i++) {
        d  = ShowLocal(i);
        d += ShowLocal(i + 10);
        d += ShowLocal(i + 20);
        if (d) nl();
    }
    fprintf(stderr, "-I-MIX: ****************\n");
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
    if (TRACEA)
    	fprintf(stderr, "-I-MIX:     ATOMICEXPR S='%s' T=%s\n", S, stok[T]);
    switch (T) {
    case TOK_ERR:
        break;
    case TOK_MTY:
        break;
    case TOK_LOC:
        ret = P;
        break;
    case TOK_NUM:
        ret = N;
        break;
    case TOK_SYM:
        localB = OFF;
        if (IsLocalSym(S)) {
            if ('H' == S[1])
                AsmError(EA_INVSYM);
            localB = ASONOFF('B' == S[1]);
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
    if (TRACEA)
    	fprintf(stderr, "-I-MIX:     ATOMICEXPR=%c%010o\n", PLUS(ret), MAG(ret));
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

Word Expr(void)
{
    Word v = 0, w = 0;

    if (TRACEA)
    	fprintf(stderr, "-I-MIX:     EXPR '%c%s'\n", CH, PLN);
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
	if (TRACEA)
		fprintf(stderr, "-I-MIX:     V=%c%010o\n", PLUS(v), MAG(v));
    while (IsBinOp()) {
		if (TRACEA)
			fprintf(stderr, "-I-MIX:     B=%c\n", B);
		if (UNDSYM) AsmError(EA_UNDSYM);
        w = AtomicExpr();
        if (TOK_MTY == T)
            AsmError(EA_MISSOP);
		if (TRACEA)
			fprintf(stderr, "-I-MIX:     W=%c%010o\n", PLUS(w), MAG(w));
        OT = OFF;
        switch (B) {
        case '+': v = smADD(v, w); break;
        case '-': v = smSUB(v, w); break;
        case '*': smMPY(&w, &v, w, v); break;
        case '/': smDIV(&v, NULL, 0, v, MAG(w) ? w : 1); break;
        case 'D': smDIV(&v, NULL, v, 0, MAG(w) ? w : 1); break; /*//*/
        case ':': v = i2w(8 * w2i(v) + w2i(w)); break;
        default:
            break;
        }
		if (TRACEA)
			fprintf(stderr, "-I-MIX:     AFTER V=%c%010o\n", PLUS(v), MAG(v));
        if (OT)
        	AsmError(EA_OVRFLW);
    }
	if (TRACEA)
		fprintf(stderr, "-I-MIX:     EXPR V=%c%010o\n", PLUS(v), MAG(v));
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
            if (TRACEA)
                fprintf(stderr, "-I-MIX:     EXPR LBEG=%s\n", LBEG);
	    }
        v = Expr();
	    if (LBEG) {
	        /* =123= */
	        LEND = PLN-1;
            if (TRACEA)
                fprintf(stderr, "-I-MIX:     EXPR LEND=%c%s\n", CH, LEND);
	        ENSURE('=');
            if (LEND - LBEG <= 10) {
                if (UNDSYM)
                    AsmError(EA_UNDSYM);
		        *LEND = 0;
                MemSet(S, ' ', sizeof(S)-1);
                for (i = 0; i < LEND - LBEG; i++)
                    S[i] = LBEG[i];
                if (TRACEA)
                    fprintf(stderr, "-I-MIX:     LIT='%s' V=%c%010o\n", S, PLUS(v), MAG(v));
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
                v = SM_MINUS1;
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
	    if (!RANGE(v,0,63)) {
	        v = F; AsmError(EA_TBIGFI);
        }
	    if (L(v) > R(v)) {
	        v = F; AsmError(EA_INVFLD);
        }
    }
    return v;
}

Word Wvalue(void)
{
    Word v = 0, w = 0;
    int f;

    if (TRACEA)
    	fprintf(stderr, "-I-MIX:     WVALUE\n");
    w = Expr();
    f = Fpart(0, 05);
    if (TRACEA)
    	fprintf(stderr, "-I-MIX:     W=%c%010o F=%o\n", PLUS(w), MAG(w), f);
    v = WriteField(v, f, w);
    if (TRACEA)
    	fprintf(stderr, "-I-MIX:     V=%c%010o\n", PLUS(v), MAG(v));
    while (!E && ',' == CH) {
        NEXT();
        w = Expr(); f = Fpart(0, 05);
	    if (TRACEA)
	    	fprintf(stderr, "-I-MIX:     W=%c%010o F=%o\n", PLUS(w), MAG(w), f);
        v = WriteField(v, f, w);
	    if (TRACEA)
	    	fprintf(stderr, "-I-MIX:     V=%c%010o\n", PLUS(v), MAG(v));
    }
    if (TRACEA)
    	fprintf(stderr, "-I-MIX:     WVALUE=%c%010o\n", PLUS(v), MAG(v));
    return v;
}

char* GetWord(char *dst, char *src, int n)
{
    int i, ch;

    ch = *src;
    while (ch && (';' != ch) && (ch  < 32))
        ch = *src++;
    for (i = 0; i < n; i++) {
        ch = *src;
        if (';' != ch && ch > 32) {
            *dst++ = ch; src++;
        }
    }
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
    fprintf(stderr, "%s%c", EC, FREF);
    i = 0;
    if (NEEDAWS) {
	   	if ('A' == NEEDAWS) {
            INST = w; w = MAG(w);
	        C = BYTE(w); w >>= 6;
	        F = BYTE(w); w >>= 6;
	        I = BYTE(w); w >>= 6;
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
    fprintf(stderr, "|%s\n", line);
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
            DefineSym(syms[i].S, P, ON);
            PrintList(needs, w, P, syms[i].S);
            MemWrite(P, FULL, w); P++;
        } else if (!syms[i].D) {
            EC[0] = EA_UNDSYM;
            NEEDAWS = 0;
            LREF = syms[i].N;
            PrintList(needs, w, SM_MINUS1, syms[i].S);
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
        if (SM_MINUS1 != syms[xf-1].N) {
            DefineSymIdx(xf, NULL, w, ON);
            syms[xf-1].N = SM_MINUS1;
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
        char *ptr = line;
        if (' ' != *ptr)
            ptr = GetWord(&LINE[0], ptr, 10);
        ptr = GetWord(&LINE[11], ptr, 4);
        ptr = GetWord(&LINE[16], ptr, MAX_LINE - 14);
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
        fprintf(stderr, "-I-MIX: LINE=%04d\n", LNO);
        fprintf(stderr, "-I-MIX:     LOCATION='%s'\n", LOCATION);
        fprintf(stderr, "-I-MIX:     OP='%s'\n", OP);
        fprintf(stderr, "-I-MIX:     ADDRESS='%s'\n", ADDRESS);
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
            F = BYTE(C >> 6);
            C = BYTE(C);
            
            A = Apart();
            I = Ipart();
            F = Fpart(C,F);
            if (!RANGE(C,042,046)) {
	            if (!RANGE(I,0,6)) {
	            	I = 0; AsmError(EA_TBIGFI);
                }
			}
            MemWrite(P, FULL, w = SIGN(A) + (MAG(A) << 18) + (BYTE(I) << 12) + (BYTE(F) << 6) + C); P++;
            NEEDAWS = 'A';
        } else if (!strcmp(OP, "ORIG")) {
	        w = Wvalue();
	        if (CheckAddr(w, NULL)) {
	        	w = 0; AsmError(EA_INVORG);
            }
            P = w;
        }
        else if (!strcmp(OP, "CON ")) {
            MemWrite(P, FULL, w = Wvalue()); P++;
            NEEDAWS = 'W';
        } else if (!strcmp(OP, "ALF ")) {
            Byte buf[5];
            for (i = 0; i < 5; i++) {
                buf[i] = cr_a2m[CH]; NEXT();
            }
            MemWrite(P, FULL, w = Pack(buf, 0)); P++;
            ADDRESS[5] = 0;
            NEEDAWS = 'W';
        }
        else if (!strcmp(OP, "END ")) {
            DefineLiterals();
            OLDP = P;
            if (SM_MINUS1 == START)
                START = w = field(Wvalue(), FIELD(4,5));
            else
                fprintf(stderr, "-W-MIX: START ADDRESS IGNORED\n");
            NEEDAWS = 'S';
            OPEND = ON;
            if (' ' != LOCATION[0])
            	AsmError(EA_ENDLOC);
        } else
            AsmError(EA_UNKOPC);
    }
    if (!E && CH && ' ' != CH) {
	    if (TRACEA)
	    	fprintf(stderr, "-I-MIX:     EXTRA OP ('%c',%d)\n", CH, CH);
    	AsmError(EA_XTRAOP);
	}
    if (XH) {
        int xb = localSymIdx(LOCATION, 'B');
        syms[xb-1].N = syms[XH-1].N;
        syms[xb-1].D = syms[XH-1].D;
        syms[XH-1].N = SM_MINUS1;
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
    char *ptr, line[MAX_LINE+1], path[MAX_LINE+1];
    int n, failed;
    FILE *fd;

    strcpy(path, nm);
    fd = fopen(path, "rt");
    strtoupper(path);
    if (NULL == fd) {
        fprintf(stderr, "-E-MIX: CANNOT OPEN %s\n", path);
        return 1;
    }
    fprintf(stderr, "-I-MIX: PROCESSING %s\n", path);

    nsyms = 0; nload = 0;
    InitLocalSyms();

    failed = 0;
    P = 0; OPEND = OFF;

    LNO = 1; ptr = fgets(line, sizeof(line), fd);
    while (ptr && !feof(fd)) {
        n = strlen(line);
        while (n && IsCRLF(line[n-1]))
            n--;
        line[n] = 0;
        failed += n ? Assemble(line) : 0;
        if (OPEND)
            break;
        LNO++; ptr = fgets(line, sizeof(line), fd);
    }
    fclose(fd);
    fprintf(stderr, "-I-MIX: ASSEMBLE %s\n\n", failed ? "FAILED" : "DONE");

    return 0;
}


/* =============== C O N T R O L  S E C T I O N ============= */

void decode(Word C, Word F)
{
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
	s = mnemos[C / 8];
	if (' ' == *s) {
		s++;
		if (4 == strlen(s))
			strcpy(mnemo, s);
		else
			strncpy(mnemo, s + 4 * (C & 07), 4);
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
		*s = regnames[C & 07];
}

void Status(Word P)
{
	static char *sot = " X", *sci = "<=>", *ssta = "HSWN ";
	Word w, INST, A, I, F, C, M, OP;
	int i;
	
	P = MAG(P);
	INST = mem[P];
    w = MAG(INST);
	C = BYTE(w); w >>= 6;
	F = BYTE(w); w >>= 6;
	I = BYTE(w); w >>= 6;
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
		fprintf(stderr, "   LOC FREQ   INSTRUCTION  OP    OPERAND     REGISTER A  REGISTER X   RI1    RI2    RI3    RI4    RI5    RI6    RJ  OV CI   TYME\n");
	fprintf(stderr, "%c%05o %04d ", ssta[STATE], P, freq[P] % 9999);
    aprint4(A); bprint(I); bprint(F); bprint(C);
	fprintf(stderr, "%s ", mnemo);
    OP = M; /* 1, 2, 3, 4, 8..23, 56..53 */

#if 0
    xprin(A);
    i = 0;
    if (I) fprintf(stderr, ",%d", I);
    else i += 2;
    j = 5;
    if (F) {
        if ((RANGE(C,1,4) || RANGE(C,8,33) || RANGE(C,56,63))
                && ((32 == C && 2 != F) || (32 != C && 5 != F)))
        {
            fprintf(stderr,"(%d:%d)", L(F), R(F)); j = 0;
        } else if (RANGE(C,34,38)) {
            fprintf(stderr, "(%0o) ", F); j = 0;
        }
    }
    i += j;
    if (i)
        fprintf(stderr, "%*s", i, " ");
#endif

    if (RANGE(C,1,4) || RANGE(C,8,23) || RANGE(C,56,63)) {
        GetV(M, F, &OP);
        wprint(OP);
    } else {
	    xprint(OP);
        fprintf(stderr, "     ");
    }
	wprint(rA); wprint(rX);
	for (i = 1; i <= 6; i++)
		xprint(reg[i]);

	xprint(rJ);
	fprintf(stderr, "%c %c %07u\n", sot[OT], sci[CI], Tyme);
}



int CheckIdx(int x, Toggle cy)
{
    if (OFF != cy) {
        fprintf(stderr,"-E-MIX: LOC=%04o RI%d OVERFLOW\n", P, x);
		return Stop();
	} else if (~(SM_SIGN + IX_MASK) & MAG(reg[x])) {
		fprintf(stderr,"-E-MIX: LOC=%04o RI%d UNDEFINED\n", P, x);
		return Stop();
	}
    return 0;
}


Word ConvertNum(Word sum, Word w, int *pscale)
{
	int i, scale;
	
	scale = *pscale;
	for (i = 0; i < 5; i++) {
		sum += scale * (BYTE(w) % 10);
		w >>= 6; scale *= 10;
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
    INST = MemRead(L);
    w = MAG(INST);
	C = BYTE(w); w >>= 6;
	F = BYTE(w); w >>= 6;
	I = BYTE(w); w >>= 6;
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

    if (CheckMemRead(P)) {
        return Stop();
	}
	IR = MemRead(P);
	w = IR;
	C = BYTE(w); w >>= 6;
	F = BYTE(w); w >>= 6;
	I = BYTE(w); w >>= 6;
	A = A_MASK & w; if (SIGN(IR)) A += SM_SIGN;
    if (I) {
        if (CONFIG & MIX_INDEX) {
            if (EffectiveAddress(P, &M))
                return 1;
        } else {
            if (I > 6) {
                fprintf(stderr, "-E-MIX: LOC=%04o I=%02o ILLEGAL INDEX\n", P, I);
                return Stop();
            }
	        M = smADD(A, reg[I]);
        }
        if (~(SM_SIGN + IX_MASK) & M) {
            fprintf(stderr, "-E-MIX: LOC=%04o M=%c%010o ILLEGAL ADDRESS\n", P, PLUS(M), MAG(M));
            return Stop();
        }
    }
	switch (C) {
	case 0: /*NOP*/
		break;
	case 1: /*ADD*/
        if (GetV(M, F, &w))
            return 1;
		w = smADD(rA, w); if (CY) OT = CY;
		if (!MAG(w)) w += SIGN(rA);
		rA = w;
		break;
	case 2: /*SUB*/
        if (GetV(M, F, &w))
            return 1;
		rA = smSUB(rA, w); if (CY) OT = CY;
		break;
	case 3: /*MUL*/
        if (GetV(M, F, &w))
            return 1;
		smMPY(&rA, &rX, rA, w);
		Tyme += 9; TIMER += 9;
		break;
	case 4: /*DIV*/
        if (GetV(M, F, &w))
            return 1;
		smDIV(&rA, &rX, rA, rX, w);
		Tyme += 11; TIMER += 11;
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
                if (CheckBinary() || CheckMemRead(M))
                    return 1;
                rA = SIGN(rA) + (MAG(rA) & MAG(MemRead(M)));
                break;
            case  4: /*OR*/
                if (CheckBinary() || CheckMemRead(M))
                    return 1;
                rA = SIGN(rA) + (MAG(rA) | MAG(MemRead(M)));
                break;
            case  5: /*XOR*/
                if (CheckBinary() || CheckMemRead(M))
                    return 1;
                rA = SIGN(rA) + (MAG(rA) ^ MAG(MemRead(M)));
                break;
            case  6: /*FLOT*/
                if (CheckFloat())
                    return 1;
                return FieldError(F);
            case  7: /*FIX*/
                if (CheckFloat())
                    return 1;
                return FieldError(F);
            case  8: /*NEG*/
                if (CheckBinary())
                    return 1;
                rA = SIGN(rA) + MAG(~MAG(rA));
                break;
            case  9: /*INT*/
                if (CheckInterrupt())
                    return 1;
                return FieldError(F);
            case 10: /*XCH*/
                if (CheckBinary())
                    return 1;
                w = rA; rA = rX; rX = w;
                break;
            case 11: /*XEQ*/
                if (CheckMaster())
                    return 1;
                if (XEQTING) {
                    fprintf(stderr, "-E-MIX: LOC=%04o NESTED XEQ\n", P);
                    return Stop();
                }
                w = P; P = M; XEQTING = ON; Step(); XEQTING = OFF; P = w;
                break;
			default:
				return FieldError(F);
			};
		}
		break;
	case 6:
		{	Word signA, signX;

			if (SIGN(M)) {
				fprintf(stderr, "-E-MIX: LOC=%04o ILLEGAL SIGNED M=%c%05o\n", P, PLUS(M), MAG(M));
				return Stop();
			} else {
				signA = SIGN(rA); signX = SIGN(rX);
				switch(F){
				case 0: /*SLA*/
					rA = signA + smSLAX(&rA, NULL, MAG(rA), 0, 6*M, 0);
					break;
				case 1: /*SRA*/
					rA = signA + smSRAX(&rA, NULL, MAG(rA), 0, 6*M, 0);
					break;
				case 2: /*SLAX*/
					smSLAX(&rA, &rX, MAG(rA), MAG(rX), 6*M, 0);
					rA += signA; rX += signX;
					break;
				case 3: /*SRAX*/
					smSRAX(&rA, &rX, MAG(rA), MAG(rX), 6*M, 0);
					rA += signA; rX += signX;
					break;
				case 4: /*SLC*/
					smSLAX(&rA, &rX, MAG(rA), MAG(rX), 6*M, 1);
					rA += signA; rX += signX;
					break;
				case 5: /*SRC*/
					smSRAX(&rA, &rX, MAG(rA), MAG(rX), 6*M, 1);
					rA += signA; rX += signX;
					break;
                case 6: /*SLB*/
                    if (CheckBinary())
                        return 1;
					smSLAX(&rA, &rX, MAG(rA), MAG(rX), M, 0);
                    rA += signA; rX += signX;
                    break;
                case 7: /*SRB*/
                    if (CheckBinary())
                        return 1;
					smSRAX(&rA, &rX, MAG(rA), MAG(rX), M, 0);
                    rA += signA; rX += signX;
                    break;
				default:
					return FieldError(F);
				}
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
        if (GetV(M, F, &w))
            return 1;
		x = C - 8;
		reg[x] = w;
		if ((0 < x && x < 7) && CheckIdx(x, OFF))
            return 1;
		break;
	case 16: case 17: case 18: case 19: case 20: case 21: case 22: case 23: /*LDrN*/
        if (GetV(M, F, &w))
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
        devOpen(F);
		if (devBusy(F)) {
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
		if (devOpen(F))
			return 0;
        if (devBusy(F)) {
            IdleTyme++;
            if (!Waiting())
                WaitFor(devs[F].evt);
            return 0;
        }
        ASSERT(!Waiting());
        switch (C) {
        case 35: devIOC(F, M); break;
		case 36: devINP(F, M); break;
		case 37: devOUT(F, M); break;
        }
		break;
	case 38: /*JRED*/
        devOpen(F);
        if (!RANGE(F,0,MAX_DEVS-1))
            return FieldError(F);
		if (!devBusy(F)) {
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
            if (CheckBinary())
                return 1;
            cond = 0 == (MAG(w) & 1); break;
        case 7: /*JrO*/
            if (CheckBinary())
                return 1;
            cond = 1 == (MAG(w) & 1); break;
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
            if (CheckMaster())
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
        if (GetV(M, F, &w))
            return 1;
		w = smSUB(field(reg[C - 56], F), w);
		if (SIGN(w)) CI = LESS;
		else if (!MAG(w)) CI = EQUAL;
		else CI = GREATER;
		break;
	}
    P++;
	return 0;
}


void Run(Word p)
{
    Word OLDP;
    unsigned evt;
    int u;

	P = p; OLDP = p + 1; STATE = S_NORMAL;
	while (Running()) {
        if (!Waiting()) {
            if (TRACE && OLDP != P)
                Status(P);
        }
        OLDP = P;
        Step();
        DoEvents();
        if (Waiting())
            P = OLDP;
        else {
            freq[OLDP]++; InstCount++;
        }
	}
    if (Halted()) {
        /* normal HLT, process I/O events */
        while (EventH) {
            DoEvents(); Tyme++; IdleTyme++;
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
}

void Finish(void)
{
    int n, i;
    FILE *fd;

    for (i = 0; i < MAX_DEVS; i++) {
        if (stdout == devs[i].fdout)
            fflush(devs[i].fdout);
        if (stdin != devs[i].fd)
            fclose(devs[i].fd);
    }

    if ((NULL != mem) && (CONFIG & MIX_CORE)) {
        fd = fopen(CORE_MEM, BIN_CREATE);
        if (fd) {
            n = fwrite(mem, sizeof(Word), MAX_MEM + 1, fd);
            if (n != MAX_MEM + 1)
                fprintf(stderr, "-E-MIX: SAVE CORE FAILED\n");
            fclose(fd);
        } else
            fprintf(stderr, "-E-MIX: CANNOT SAVE CORE\n");

    }

    if (freq) free(freq);
    if (mem) free(mem);
}

void InitMemory(void)
{
    int i;

    /* --- memory --- */
    if (CONFIG & MIX_MASTER) {
        IX_MASK = SM_MASK(15); MAX_MEM = 31999;
        MAX_DEVS = 64;
    } else {
        IX_MASK = SM_MASK(12); MAX_MEM = 3999;
        MAX_DEVS = 21;
    }
    mem  = malloc((MAX_MEM + 3) * sizeof(Word));
    freq = malloc((MAX_MEM + 3) * sizeof(unsigned short));
    devs = malloc(MAX_DEVS * sizeof(Device));
    events = malloc(MAX_DEVS * sizeof(Event));
    if (NULL == mem || NULL == freq || NULL == devs || events == NULL) {
        exit(1);
    }
	for (i = 0; i < MAX_MEM + 3; i++) {
		mem[i] = freq[i] = 0;
	}
}

void InitMixToAscii(void)
{
    int i;

    strcpy(m2a, STAN ? stan_m2a : knuth_m2a);
    for (i = 0; i < 256; i++) {
        a2m[i] = cr_a2m[i] = 0;
    }

	for (i = 0; i < 49; i++) {
        cr_m2a[i] = m2a[i];
    }
    cr_m2a[20] = cr_m2a[21] = '?';
    for (i = 49; i < 63; i++) {
        cr_m2a[i] = '?';
    }

	for (i = 0; i < 64; i++) {
		a2m[(int) m2a[i]] = i;
		cr_a2m[(int) cr_m2a[i]] = i;
	}
    cr_a2m[' '] = 0; cr_a2m['?'] = 0;
    if (!STAN) {
        a2m[' '] = 0;
        a2m['?'] = 0;
    }
}

void InitCore(void)
{
    int n;
    FILE *fd;

    fd = fopen(CORE_MEM, "rb");
    if (fd) {
        n = fread(mem, sizeof(Word), MAX_MEM + 1, fd);
        if (n != MAX_MEM + 1)
            fprintf(stderr, "-W-MIX: LOAD CORE FAILED\n");
        fclose(fd);
    }
}

void InitOptions(void)
{
    CONFIG = 0;
    LPT = stderr;
    TRACEOP = TRACEIO = OFF; TraceCount = 0; TRACEA = OFF;
    TRANS = LNKLD = DUMP = STAN = OFF;
    START = SM_MINUS1;
    TRANSNM[0] = 0;
    XEQTING = OFF;
    FF = OFF;
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
		devs[i].fd = devs[i].fdout = NULL;
        events[i].what = DO_NOTHING;
        events[i].next = 0;
    }
    EventH = 0;
    WaitEvt = 0;

    if (CONFIG & MIX_CORE)
        InitCore();
    InitMixToAscii();
}

void Usage(void)
{
	fprintf(stderr, "usage: mix [-bcfgimx][-6ad][-s addr][-t aio][-lpr] file1...\n");
    fprintf(stderr, "options:\n");
    fprintf(stderr, "    -b         install binary MIX\n");
    fprintf(stderr, "    -c         core memory (core.mem)\n");
    fprintf(stderr, "    -f         install floating-point attachment\n");
    fprintf(stderr, "    -g [unit]  push GO button on unit (def. card reader)\n");
    fprintf(stderr, "    -i         install interrupt facility\n");
    fprintf(stderr, "    -m         Mixmaster\n");
    fprintf(stderr, "    -x         install double/indirect-indexing facility\n");

    fprintf(stderr, "    -6         use Stanford MIX/360 charset\n");
    fprintf(stderr, "    -a         assemble only\n");
    fprintf(stderr, "    -d         dump non-zero locations\n");
    fprintf(stderr, "    -l         punch LNKLD cards\n");
    fprintf(stderr, "    -p         punch nonzero locations in TRANS fmt\n");
    fprintf(stderr, "    -r         free fmt MIXAL\n");
    fprintf(stderr, "    -s address set START address\n");
    fprintf(stderr, "    -t aio     enable tracing: Asm,Io,Op\n");
	exit(1);
}


void Go(int u)
{
    int x;

    x = devIdx(u);

	if (u < 0 || u >= MAX_DEVS || DEV_CP == x || DEV_LP == x || DEV_TT == x)
		Usage();
	devINP(u, 0);
	Tyme = devs[u].evt;
	DoEvents();
	Tyme = 0;
	Run(0);
}


void Stats(FILE *fd, int STRIDE, int dotrans)
{
    int prev_i, i, j, minj, maxj;
    unsigned emit, prev_emit;
    FILE *LPTSAV;

    LPTSAV = LPT; LPT = fd;
    if (!dotrans) {
        nl();
        fprintf(LPT, "CONTENTS OF MIX MEMORY (NONZERO LOCATIONS ONLY)\n");
        nl();
        fprintf(LPT, "LOC        ");
        for (i = 0; i < STRIDE; i++)
            fprintf(LPT, "%d           ", i);

        fprintf(LPT, "%*sFREQUENCY  COUNTS\n", 1 + (6*STRIDE - 17) / 2, " ");
    }

    prev_emit = 0; prev_i = 0;
    for (i = 0; i <= MAX_MEM; i += STRIDE) {
        emit = 0; minj = STRIDE; maxj = 0;
        for (j = 0; (i + j <= MAX_MEM) && (j < STRIDE); j++) {
            Word mag = mem[i + j];
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
            if (prev_emit && ((prev_i + STRIDE) != i))
                fprintf(LPT, "%04o..%04o   SAME AS ABOVE\n", prev_i, i - 1);
        }
        prev_i = i;
        prev_emit = emit;
        if (dotrans) {
            char buf[11];
            fprintf(LPT, "%-5s%d%04d", TRANSNM, maxj - minj + 1, i + minj);
            for (j = minj; j < minj + STRIDE; j++) {
                if (j > maxj)
                    strcpy(buf, "0000000000");
            	else {
                    Word w = mem[i + j];
                    sprintf(buf, "%010d", MAG(w));
                    if (SIGN(w))
                        buf[9] = "~JKLMNOPQR"[MAG(w) % 10];
                }
                fprintf(LPT, "%s", buf);
            }
        } else {
            fprintf(LPT, "%04o: ", i);
            for (j = 0; j < STRIDE; j++)
                wprint(mem[i + j]);
            fprintf(stderr, "     ");
            for (j = 0; j < STRIDE; j++)
                fprintf(LPT, " %05d", freq[i + j]);
        }
        nl();
    }
    if (dotrans) {
        if (LNKLD)
            PunchLinkLoad(LPT);
        fprintf(LPT, "TRANS0%04d%*s\n", dotrans - 1, 70, " ");
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
        Stats(fout, 7, START+1);
        fclose(fout);
    } else
        fprintf(stderr, "-E-MIX: CANNOT OPEN %s\n", buf);
}


#define NASMFILES 32

int main(int argc, char*argv[])
{
	int i, j, u;
	char *arg;
    Toggle ASMONLY, GO;
    char *asmfiles[NASMFILES];
    int nasmfiles;

    InitOptions();
    ASMONLY = OFF; GO = OFF; u = CARD_READER;
    nasmfiles = 0;
    
	for (i = 1; i < argc; i++) {
		arg = argv[i];
		if ('-' != *arg) {
            if (nasmfiles >= NASMFILES) {
                fprintf(stderr, "-E-MIX: TOO MANY ASM FILES\n");
                exit(1);
            }
            asmfiles[nasmfiles++] = arg;
            continue;
        } 
		switch (arg[1]) {
        case 'b': CONFIG += MIX_BINARY; continue;
        case 'c': CONFIG += MIX_CORE; continue;
        case 'f': CONFIG += MIX_FLOAT; continue;
		case 'g':
            CONFIG += MIX_PUSHGO;
            GO = ON;
			if (i + 1 < argc) {
                arg = argv[i+1];
                if (IsDigit(*arg)) {
				    u = atoi(arg); i++;
                }
			}
			continue;
        case 'i': CONFIG += MIX_INTERRUPT; continue;
        case 'm': CONFIG += MIX_MASTER; continue;
        case 'x': CONFIG += MIX_INDEX; continue;

        case '6': STAN = ON; continue;
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
				arg = argv[++i];
				for (j = 0; j < strlen(arg); j++) {
					switch (arg[j]) {
					case 'i': TRACEIO = ON; break;
					case 'o': TRACEOP = ON; break;
					case 'a': TRACEA = ON; break;
					default:
						Usage();
					}
				}
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

    if (SM_MINUS1 == START)
        START = 0;

    if (TRANS)
        PunchTrans();
    if (GO) {
        if (0 == devOpen(u))
	        Go(u);
    } else if (!ASMONLY)
        Run(START);
    if (DUMP)
        Stats(stderr, 4, 0);
    if (InstCount) {
        fprintf(stderr, "                              TOTAL INSTRUCTIONS EXECUTED:     %08d\n", InstCount);
        fprintf(stderr, "                              TOTAL ELAPSED TYME:              %08d (%d IDLE)\n", Tyme, IdleTyme);
    }

	return 0;
}

/* vim: set ts=4 sw=4 et: */
