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
 *  - do I/O at half time
 *  - wait on busy device
 *  + rename checks, implement throw or check everything before op
 *  
 *
 *  History:
 *  ========
 *  250619AP    changed CS/RUN to STATE
 *              added Schedule(), wait on busy device
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

#if defined(NDEBUG)
# define ASSERT(x)
#else
# define ASSERT(x)  __assert(__LINE__,__FUNCTION__,""#x)
void __assert(int lno, const char *fun, const char *x)
{
    fprintf(stderr, "%s:%d: %s failed\n", fun, lno, x);
    fflush(stderr);
    exit(1);
}
#endif

char *strtolower(char *s)
{
    char *p;
    if ((p = s)) {
        while ((*p = tolower(*p)))
            p++;
    }
    return s;
}


char *strtoupper(char *s)
{
    char *p;
    if ((p = s)) {
        while ((*p = toupper(*p)))
            p++;
    }
    return s;
}

typedef unsigned int Word;
typedef unsigned char Byte;
typedef enum {OFF, ON} Toggle;
typedef enum {S_HALT, S_WAIT, S_NORMAL, S_CONTROL} MachineState;

#define MAX_MEM		4001
#define TRACE       mem[4000]
#define TIMER       mem[4001]

Word reg[10], mem[MAX_MEM+1], P;
Toggle OT;
enum {LESS, EQUAL, GREATER} CI;
Toggle TRANS;
char TRANSNM[5+1];

FILE *LPT;
unsigned int Tyme, IdleTyme, InstCount;
unsigned short freq[MAX_MEM+1];
MachineState STATE, STATESAV;
Toggle CY;

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
#define SM_SIGN		(1U << 31)
#define SM_MASK(x)	((1U << (x)) - 1)
#define IX_MASK		07777

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
	if (SIGN(y)) {
		Word tmp = y; y = x; x = tmp;
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
	
	for (i = 0; i < shmt; i++) {
		sav = 1 & x;
		x >>= 1;
		if (1 & a)
			x += SM_MSB;
		a >>= 1;
		if (circ && sav) {
			a += SM_MSB;
		}
	}
	*pa = a;
	*px = x;
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
	hi = smADD(hi, HI(mid1) >> 15);
	hi = smADD(hi, HI(mid2) >> 15);
	if (SIGN(a) != SIGN(x)) {
		hi += SM_SIGN;
		lo += SM_SIGN;
	}
	*pa = hi;
	*px = lo;
}

/* sign is a */
void smDIV(Word *pquo, Word *prem, Word a, Word x, Word v)
{
	Word ma, mx, mv;
	int i, d;
	
	ma = MAG(a); mx = MAG(x); mv = MAG(v);
	for (i = 0; i < 30; i++) {
		d = 0;
		if (ma >= mv) {
			d = 1; ma -= mv;
		}
		smSLAX(&ma, &mx, ma, mx, 1, 0); mx += d;
	}
	if (SIGN(a) != SIGN(v))
		ma += SM_SIGN;
	mx = SIGN(a) + MAG(mx);
	*pquo = mx;
	*prem = ma;
}



/* ============== M A C H I N E  S T A T E ================== */

int Halt(void)
{
    STATESAV = STATE;
    STATE = S_HALT;
    return 1;
}

int Running(void)
{
    return S_HALT != STATE;
}

void Awake(void)
{
    ASSERT(S_NORMAL == STATESAV || S_CONTROL == STATESAV);

    STATE = STATESAV;
    STATESAV = S_HALT;
}

void Wait(void)
{
    ASSERT(S_HALT == STATESAV);

    STATESAV = STATE;
    STATE = S_WAIT;
}

int Waiting(void)
{
    return S_WAIT == STATE;
}



/* =================== M E M O R Y ========================== */


int CheckAddr(Word a, char *msg)
{
	if (SIGN(a) || MAG(a) > MAX_MEM) {
		fprintf(stderr, "-E-MIX: LOC=%04o M=%04o INV.MEMORY ADDRESS %s\n", P, w2i(a), msg);
        return Halt();
	}
    return 0;
}

#define CheckMemRead(a)     CheckAddr(a, "MEMORY READ")

Word MemRead(Word a)
{
	Tyme++; TIMER++;
	return mem[MAG(a)];
}

#define CheckMemWrite(a)    CheckAddr(a, "MEMORY WRITE")

void MemWrite(Word a,int f,Word w)
{
	int sign, l, r, shmt;
	unsigned mask;
	Word v;
	
	Tyme++; TIMER++;
	if (FULL == f) {
		mem[MAG(a)] = w;
		return;
	}
	l = L(f); r = R(f);
    sign = 0;
	if (0 == l)
        sign = ++l;
	mask = SM_MASK(6 * (r - l + 1));
	shmt = 6 * (5 - r);
	v = mask & w;
	v <<= shmt; mask <<= shmt;
	if (sign) {
		mask += SM_SIGN;
		v += SIGN(w);
	}
	w = mem[MAG(a)];
	mem[MAG(a)] = v + (mask & w);
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
		
	if ((src <= dst && dst < src + n) || (dst <= src && src < dst + n)) {
		for (i = n-1; i >= 0; i--) {
			mem[dst + i] = mem[src + i];
		}
	} else {
		for (i = 0; i < n; i++)
			mem[dst++] = mem[src++];
	}
}



/* ================== I N P U T / O U T P U T =============== */

typedef struct __Device {
	FILE *fd, *fdout;
	unsigned pos;
	unsigned max_pos;
	unsigned evt;
} Device;

#define MAX_DEVS	21
Device devs[MAX_DEVS];

/* 
Input/Output
	t	0<=t<8	100
	d	8<=d<15	100
	cr	16		16
	cp	17		16
	lp	18		24
	tt	19		14
	
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
/*		                     1         2         3         4         5         6	 */
/*		           0123456789012345678901234567890123456789012345678901234567890123*/
char    m2a[64] = " ABCDEFGHI~JKLMNOPQR[#STUVWXYZ0123456789.,()+-*/=$<>@;:'        ";
char cr_m2a[64] = " ABCDEFGHI~JKLMNOPQR  STUVWXYZ0123456789.,()+-                  ";
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

#define DEV_MT	 7
#define DEV_DR  13
#define DEV_DK	15
#define DEV_CR	16
#define DEV_CP	17
#define DEV_LP	18
#define DEV_PT	19
#define DEV_TT  20

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
	{    "tape", BIN_RWRITE,   NULL,   NULL, 17280, 100, 0,  3056,    3056,    0,     859 },
	{    "disk", BIN_RWRITE,   NULL,   NULL,  4096, 100, 0,   833,     833, 6666,       0 },
    {    "drum", BIN_RWRITE,   NULL,   NULL,   512, 100, 0,   333,    1333, 2666,       0 },
	{  "reader", TXT_RDONLY, cr_a2m,   NULL,     0,  16, 0, 50000,       0,    0,       0 },
	{   "punch", TXT_APPEND,   NULL, cr_m2a,     0,  16, 1,     0,  100000,    0,       0 },
	{ "printer", TXT_APPEND,   NULL,    m2a,     0,  24, 1,     0,   21054,    0,     833 },
	{   "ptape", TXT_RWRITE,    a2m,    m2a,  1707,  14, 0, 23333,  116667,    0,   23333 },
    {      NULL,       NULL,    a2m,    m2a,     0,  14, 1,     0, 1166667,    0, 1166667 },
};


typedef enum {DO_NOTHING, DO_IOC, DO_IN, DO_OUT} EventType;

struct __Event {
    EventType what;
    unsigned when;
    unsigned M;
    int next;
} events[MAX_DEVS];
int EventH;

int devIdx(int u);
void blkSeek(int u, unsigned pos);
void blkRead(int u, unsigned adr, Byte *cvt);
void blkWrite(int u, unsigned adr, char *cvt);


void doIO(int u)
{
    Word M;
    int x;

    ASSERT(EventH != u+1);
    ASSERT(0 == events[u].next);
    ASSERT(DO_NOTHING != events[u].what);

    M = events[u].M;
    x = devIdx(u);

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
    events[u].M = M;

    if (EventH) {
        i = EventH-1;
        while (0 != events[i].next && events[i].when <= when) {
            p = i;
            i = events[i].next;
        }
    }
    if ((0 == EventH) || (EventH-1 == i)) { /* empty or head */
        events[u].next = EventH;
        EventH = u+1;
    } else if (0 == events[i].next) { /* tail */
        events[i].next = u;
    } else { /* middle */
        events[p].next = u;
        events[u].next = i;
    }
}


int devIdx(int u)
{
    if (u <= DEV_DK)
        return u >= DEV_DR ? 2: u / 8;
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
    return (DT_STUCK == devs[u].evt) || (Tyme < devs[u].evt);
}


void blkRead(int u, unsigned adr, Byte *cvt)
{
	unsigned ret, n;
	int i, j, x;
    char tmp[LP_BLOCK * BYTES];
	Byte buf[LP_BLOCK * BYTES];
	unsigned Blk_size;
    char c;
	
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
    while ('\n' == c || '\r' == c) {
        c = getc(devs[u].fd);
    }
    if ('\n' != c && '\r' != c)
        ungetc(c, devs[u].fd);

	n = Blk_size * BYTES;
	ret = fread(tmp, sizeof(char), n, devs[u].fd);
	if (ret != n)
	    goto ErrOut;

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


void blkWrite(int u, unsigned adr, char *cvt)
{
	unsigned ret, n;
	int i, j, x;
	Byte tmp[LP_BLOCK * BYTES];
    char buf[LP_BLOCK * BYTES + 1];     /* terminal CR/LF */
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
	for (i = 0; i < n; i++)
		buf[i] = cvt[tmp[i]];
    if (IOchar[x].cr)
        buf[n++] = '\n';

	ret = fwrite(buf, sizeof(char), n, fd);
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
	
	x = devIdx(u);
    if (stdin != devs[u].fd) {
        unsigned blk_size = IOchar[x].blk_size * (u > DEV_DK ? 5 : sizeof(Word));
	    fseek(devs[u].fd, pos * blk_size, SEEK_SET);
    }
	devs[u].pos = pos;
}


int devOpen(int u)
{
	int x;
	char devname[32];
	FILE *fd;
	
	if (NULL != devs[u].fd)
		return 0;
		
	x = devIdx(u);
	if (u <= DEV_DK)
		sprintf(devname,"%s%d.img", IOchar[x].name, u);
	else
		strcpy(devname, IOchar[x].name);

	devs[u].evt = 0;
	devs[u].pos = 0;
	devs[u].max_pos = 0;
	fd = fopen(devname, IOchar[x].fam);
	if (fd) {
		if (u <= DEV_MT|| DEV_CR == u) {
			fseek(fd, 0, SEEK_END);
			devs[u].max_pos = ftell(fd) / IOchar[x].blk_size;
			fseek(fd, 0, SEEK_SET);
		}	
	} else {
		/* auto-create tape/disk/ptape */
		if (u <= DEV_DK || DEV_PT == u)
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


unsigned doIOC(int u,int M)
{
	int x;
	unsigned new_pos, old_track, new_track;
	
	x = devIdx(u);	
	if (!M) {
		if (u <= DEV_MT) {
			M = devs[u].pos;
            new_pos = 0;
		} else if (u <= DEV_DK) {
            new_pos = MAG(rX) % IOchar[x].max_pos;
            new_track = TRACK(new_pos);
            old_track = TRACK(devs[u].pos);
			M = Diff(old_track, new_track);
		} else if (DEV_LP == u) {
			fprintf(devs[u].fd,"\f");
			devs[u].pos %= LP_LINES;
			M = (LP_LINES - devs[u].pos);
            new_pos = 0;
		} else if (DEV_PT == u) {
			M = devs[u].pos;
            new_pos = 0;
		} else
            goto ErrOut;
        blkSeek(u, new_pos);
		return IOchar[x].seek_tyme * M;
	}
	if (u <= DEV_MT) {
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
        blkSeek(u, new_pos);
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

    delta = doIOC(u, M);
    if (!devStuck(u))
        Schedule(delta, u, DO_IOC, M);
}


void devIN(int u, Word M)
{
	int x;
	char *errmsg = NULL;
    Byte *cvt = NULL;
	
	x = devIdx(u);
	CheckAddr(M, "IN BUFFER");
	CheckAddr(M + IOchar[x].blk_size, "IN BUFFER END");
	
	M = MAG(M);

	devs[u].evt = Tyme;
	if (u <= DEV_MT) {
		if (devs[u].pos >= devs[u].max_pos) {
			errmsg = "EOT"; goto ErrOut;
		}
	} else if (u <= DEV_DK) {
		unsigned delta = doIOC(u, 0);
        devs[u].evt += delta + IOchar[x].rot_tyme * Diff(Tyme & 63, BYTE(rX)) / 64.0;
	} else if (DEV_CR == u) {
        cvt = cr_a2m;
	} else if (DEV_PT == u) {
        cvt = a2m;
	} else {
		errmsg = "UNSUPPORTED"; goto ErrOut;
	}
    blkRead(u, MAG(M), cvt);
	devs[u].evt += IOchar[x].in_tyme;
	return;
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.IN %s\n", P, u, M, errmsg);
	devError(u);
}


void devOUT(Word u,Word M)
{
	int x;
	char *errmsg = NULL;
    char *cvt = NULL;
	
	x = devIdx(u);
	CheckAddr(M, "OUT BUFFER");
	CheckAddr(M + IOchar[x].blk_size, "OUT BUFFER END");
	
	M = MAG(M);

	devs[u].evt = Tyme;
	if (u <= DEV_MT) {
		if (devs[u].pos >= IOchar[x].max_pos) {
			errmsg = "MAG.TAPE FULL"; goto ErrOut;
		}
	    devs[u].max_pos = devs[u].pos;
	} else if (u <= DEV_DK) {
		unsigned delta = doIOC(u, 0);
        if (!devStuck(u)) {
            devs[u].evt += delta + IOchar[x].rot_tyme * Diff(Tyme & 63, BYTE(rX)) / 64.0;
		    blkWrite(u, M, NULL);
        }
	} else if (DEV_CR < u) {
        if (DEV_PT == u) {
		    if (devs[u].pos >= IOchar[x].max_pos) {
			    errmsg = "PAPER TAPE FULL"; goto ErrOut;
		    }
        }
		cvt = DEV_CP == u ? cr_m2a : m2a;
	} else {
		errmsg = "UNSUPPORTED"; goto ErrOut;
	}
    blkWrite(u, M, cvt);
	devs[u].evt += IOchar[x].out_tyme;
	return;
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.OUT %s\n", P, u, M, errmsg);
	devError(u);
}


void Lapse(unsigned t)
{
    unsigned dt;

    if (Tyme < t) {
        dt = t - Tyme;
        IdleTyme += dt; TIMER += dt;
        Tyme = t;
    }
}


void nl(void)
{
    fprintf(LPT, "\n");
}

void bprint(Byte w)
{
	fprintf(LPT, "%02o ", w);
}


void wprint(Word w)
{
	fprintf(LPT, "%c%010o ", SIGN(w) ? '-' : '+', MAG(w));
}


void xprin(Word w)
{
	fprintf(LPT, "%c%04o", SIGN(w) ? '-' : '+', MAG(w));
}

void xprint(Word w)
{
    xprin(w); fprintf(LPT, " ");
}



char mnemo[5];

void decode(Word C, Word F)
{
	static char *regnames = "A123456X";
	static char *mnemos[] = {
		" NOP ADD SUB MUL DIV \010xxx\011xxxMOVE",
		" LDr ",
		" LDrN",
		" STr ",
		" STJ STZ JBUSIOC IN  OUT JRED\012xxx",
		"\006JrN JrZ JrP JrNNJrNZJrNP",
		"\004INCrDECrENTrENNr",
		" CMPr",
 		"\003NUM CHARHLT ",
 		"\006SLA SRA SLAXSRAXSLC SRC ",
 		"\012JMP JSJ JOV JNOVJL  JE  JG  JGE JNE JLE "
	};
	int nf;
	char *s;
	
	mnemo[0] = '\0';
	s = mnemos[C / 8];
	if (' ' == *s) {
		s++;
		if (4 == strlen(s))
			strcpy(mnemo, s);
		else {
			strncpy(mnemo, s + 4 * (C & 07), 4);
			mnemo[4] = '\0';
		}
		if (*mnemo < 'A') {
			s = mnemos[(Byte) *mnemo];
			mnemo[0] = '\0';
		}
	}
	if (0 == mnemo[0]) {
		nf = *s++;
		if (F < nf) {
			strncpy(mnemo, s + 4 * F, 4);
			mnemo[4] = '\0';
		} else {
			sprintf(mnemo, "U%03o", C);
		}
	}
	s = strchr(mnemo, 'r');
	if (s)
		*s = regnames[C & 07];
}


int GetV(Word M, Byte F, Word *ret)
{
    if (CheckMemRead(M))
        return 1;
    *ret = field(MemRead(M),F);
    return 0;
}

#define GETV    field(MemRead(M),F)
#define RANGE(x,lo,hi)  (lo <= (x) && (x) <= hi)

void Status(Word P)
{
	static char *sot = " X", *sci = "LEG", *ssta = "HWN ";
	Word w, INST, A, I, F, C, M, OP;
	int i;
	
	P = MAG(P);
	INST = mem[P];
    w = MAG(INST);
	C = BYTE(w); w >>= 6;
	F = BYTE(w); w >>= 6;
	I = BYTE(w); w >>= 6;
	A = IX_MASK & w; if (SIGN(INST)) A += SM_SIGN;
	M = I ? smADD(A, reg[I]) : A;
	decode(C, F);
	
	/*
         1         2         3         4         5         6         7         8         9         A         B         C
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
  LOC FREQ   INSTRUCTION  OP    OPERAND     REGISTER A  REGISTER X  RI1   RI2   RI3   RI4   RI5   RI6   RJ  OV CI   TYME
N1234 1234 +1234 56 78 90 CODE +1234567890 +1234567890 +1234567890 +1234 +1234 +1234 +1234 +1234 +1234 +1234 ? ? 1234567
	*/
	if (0 == (InstCount % 31))
		fprintf(stderr,"  LOC FREQ   INSTRUCTION  OP    OPERAND     REGISTER A  REGISTER X  RI1   RI2   RI3   RI4   RI5   RI6   RJ  OV CI   TYME\n");
	fprintf(stderr, "%c%04o %04d ", ssta[STATE], P, freq[P] % 9999);
    xprint(A); bprint(I); bprint(F); bprint(C);
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
        fprintf(stderr, "      ");
    }
	wprint(rA); wprint(rX);
	for (i = 1; i <= 6; i++)
		xprint(reg[i]);
	xprint(rJ);
	fprintf(stderr, "%c %c %07u\n", sot[OT], sci[CI], Tyme);
}


#define UNDEF	rand()

int UndefinedOp(Word C, Word F)
{
	fprintf(stderr,"-E-MIX: LOC=%04o C=%02o F=%02o OP.UNDEFINED\n", P, C, F);
    return Halt();
}


int CheckIdx(int x, Toggle cy)
{
    if (OFF != cy) {
        fprintf(stderr,"-E-MIX: LOC=%04o RI%d OVERFLOW\n", P, x);
		return Halt();
	} else if (~IX_MASK & MAG(reg[x])) {
		fprintf(stderr,"-E-MIX: LOC=%04o RI%d UNDEFINED\n", P, x);
		return Halt();
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
	
	w = *pw; scale = 0;
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


int Step(void)
{
	Word OLDP, IR, C, F, I, A, M;
	Word w;
	int cond, x;

    OLDP = P;
    if (CheckMemRead(P))
        return 1;
	IR = MemRead(P); freq[P++]++;
	w = IR;
	C = BYTE(w); w >>= 6;
	F = BYTE(w); w >>= 6;
	I = BYTE(w); w >>= 6;
	A = IX_MASK & w; if (SIGN(IR)) A += SM_SIGN;
	M = I ? smADD(A, reg[I]) : A;
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
		rA = smSUB(rA, GETV); if (CY) OT = CY;
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
		if (!MAG(w) || MAG(rA) >= MAG(w)) {
			OT = ON;
			rA = UNDEF; rX = UNDEF;
		} else
			smDIV(&rA, &rX, rA, rX, w);
		Tyme += 11; TIMER += 11;
		break;
	case 5:
		{	Word signA, signX;
		
			signA = SIGN(rA); signX = SIGN(rX);
			switch(F){
			case 0: /*NUM*/
				rA = signA + ToNum(rA, rX);
				break;
			case 1: /*CHAR*/
				ToChar(&rA, &rX, rA);
				rA += signA; rX += signX;
				break;
			case 2: /*HLT*/
				return Halt();
			default:
				return UndefinedOp(C, F);
			};
		}
		break;
	case 6:
		{	Word signA, signX;

			if (SIGN(M)) {
				return UndefinedOp(C, F);
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
				default:
					return UndefinedOp(C, F);
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
        devOpen(F);
		if (devBusy(F)) {
            rJ = P;
            if (OLDP == M) {
                Lapse(devs[F].evt);
            }
            else
			    P = M;
		}
		break;
	case 35: /*IOC*/
		if (devOpen(F))
			return 0;
        Lapse(devs[F].evt);
		devIOC(F, M);
		break;
	case 36: /*IN*/
		if (devOpen(F))
			return 0;
		Lapse(devs[F].evt);
		devIN(F, M);
		break;
	case 37: /*OUT*/
		if (devOpen(F))
			return 0;
		Lapse(devs[F].evt);
		devOUT(F, M);
		break;		
	case 38: /*JRED*/
        devOpen(F);
		if (!devBusy(F)) {
			rJ = P;
			P = M;
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
			return UndefinedOp(C, F);
		}
		if (cond) {
			if (1 != F)
				rJ = P;
			P = M;
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
		default:
			return UndefinedOp(C, F);
		}
		if (cond) {
			rJ = P;
			P = M;
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
			if (!MAG(M))
				reg[x] += SIGN(IR);
			break;
		case 3: /*ENNr*/
			reg[x] = smNEG(M);
			break;
		default:
			return UndefinedOp(C, F);
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
    InstCount++;
	return 0;
}


void Run(Word p)
{
	P = p; STATE = S_NORMAL;
	while (Running()) {
		if (!Waiting() && TRACE)
			Status(P);
		Step();
	}
}

void Init(void)
{
	int i;

    LPT = stderr;
	InstCount = 0;
	Tyme = 0; IdleTyme = 0;
    ZERO = 0;
	STATE = S_HALT; Halt();
    TRACE = OFF; TRANS = OFF;
    for (i = 0; i < sizeof(TRANSNM); i++)
        TRANSNM[i] = 0;

	for (i = 0; i <= MAX_MEM; i++)
		freq[i] = 0;

	for (i = 0; i < 256; i++)
		a2m[i] = cr_a2m[i] = ' ';

	for (i = 0; i < 64; i++) {
		a2m[(int) m2a[i]] = i;
		cr_a2m[(int) cr_m2a[i]] = i;
	}
    a2m[' '] = cr_a2m[' '] = 0;

	for (i = 0; i < MAX_DEVS; i++) {
		devs[i].fd = devs[i].fdout = NULL;
        events[i].what = DO_NOTHING;
        events[i].next = 0;
    }
    EventH = 0;

    devs[DEV_TT].fd = stdin;
    devs[DEV_TT].fdout = stdout;
}


void Finish(void)
{
    int i;

    for (i = 0; i < MAX_DEVS; i++) {
        if (stdout == devs[i].fdout)
            fflush(devs[i].fdout);
        if (DEV_PT == i)
            continue;
        if (devs[i].fd)
            fclose(devs[i].fd);
    }
}


void Usage(void)
{
	fprintf(stderr, "usage: mix [-g dev][-t][-x name]\n");
    fprintf(stderr, "options:\n");
    fprintf(stderr, "    -g unit    push GO button on unit\n");
    fprintf(stderr, "    -t         enable tracing\n");
    fprintf(stderr, "    -x name    print nonzero locations in TRANS fmt\n");
	exit(1);
}


void Go(int u)
{
	if (u < 0 || u >= MAX_DEVS || DEV_CP == u || DEV_LP == u || DEV_TT == u)
		Usage();
	devIN(u, 0);
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
        for (j = 0; (i + j < 4000) && (j < STRIDE); j++) {
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
    if (dotrans)
        fprintf(LPT, "TRANS0%04d%*s\n", dotrans - 1, 70, " ");
    else {
        fprintf(LPT, "                              TOTAL INSTRUCTIONS EXECUTED:     %08d\n", InstCount);
        fprintf(LPT, "                              TOTAL ELAPSED TYME:              %08d (%d IDLE)\n", Tyme, IdleTyme);
    }
    LPT = LPTSAV;
}


int main(int argc, char*argv[])
{
	int i, u;
	char *arg, buf[16];
    FILE *fout;

	u = DEV_CR;
	Init();
	for (i = 1; i < argc; i++) {
		arg = argv[i];
		if ('-' != *arg)
			Usage();
		switch (arg[1]) {
		case 'g':
			if (i + 1 < argc)
				u = atoi(argv[++i]);
			else
				Usage();
			break;
		case 't':
			TRACE = ON;
			break;
        case 'x':
            if (i + 1 < argc) {
                strncpy(TRANSNM, argv[++i], 5);
                strtoupper(TRANSNM);
            }
            else
                Usage();
            TRANS = ON;
            break;
		default:
			Usage();
		}
	}

    if (0 == devOpen(u))
	    Go(u);

    Stats(stderr, 4, 0);
    if (TRANS) {
        strcpy(buf, TRANSNM);
        strcat(buf, ".tra");
        strtolower(buf);
        if ((fout = fopen(buf, TXT_APPEND))) {
            Stats(fout, 7, 1);
            fclose(fout);
        } else
            fprintf(stderr, "-E-MIX: CANNOT OPEN %s\n", buf);
    }
    Finish();

	return 0;
}

/* vim: set ts=4 sw=4 et: */
