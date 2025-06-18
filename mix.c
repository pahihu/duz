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
 *  History:
 *  ========
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

#define MAX_MEM		4001
#define TRACE       mem[4000]
#define TIMER       mem[4001]

Word reg[10], mem[MAX_MEM+1], P;
Toggle OT;
Toggle CS;
enum {LESS, EQUAL, GREATER} CI;
Toggle TRANS;
char TRANSNM[5+1];

FILE *LPT;
unsigned int Tyme, IdleTyme, InstCount;
unsigned short freq[MAX_MEM+1];
Toggle RUN, CY;

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

/* SM arithmetic */

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

Word sm_neg(Word x)
{
	return SIGN(x) ? MAG(x) : SM_SIGN + x;
}

Word sm_add(Word x, Word y)
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

Word sm_sub(Word x, Word y)
{
	return sm_add(x, sm_neg(y));
}

Word sm_slax(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
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

Word sm_srax(Word *pa, Word *px, Word a, Word x, int shmt, int circ)
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
void sm_mul(Word *pa, Word *px, Word a, Word x)
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
	
	lo = sm_add(lo, LO(mid1) << 15);
	if (CY) cy++;
    lo = sm_add(lo, LO(mid2) << 15);
	if (CY) {
        cy++;
        hi = sm_add(hi, cy);
    }
	hi = sm_add(hi, HI(mid1) >> 15);
	hi = sm_add(hi, HI(mid2) >> 15);
	if (SIGN(a) != SIGN(x)) {
		hi += SM_SIGN;
		lo += SM_SIGN;
	}
	*pa = hi;
	*px = lo;
}

/* sign is a */
void sm_div(Word *pquo, Word *prem, Word a, Word x, Word v)
{
	Word ma, mx, mv;
	int i, d;
	
	ma = MAG(a); mx = MAG(x); mv = MAG(v);
	for (i = 0; i < 30; i++) {
		d = 0;
		if (ma >= mv) {
			d = 1; ma -= mv;
		}
		sm_slax(&ma, &mx, ma, mx, 1, 0); mx += d;
	}
	if (SIGN(a) != SIGN(v))
		ma += SM_SIGN;
	mx = SIGN(a) + MAG(mx);
	*pquo = mx;
	*prem = ma;
}

/* Memory */
#define THROW

void chk_adr(Word a, char *msg)
{
	if (SIGN(a) || MAG(a) > MAX_MEM) {
		fprintf(stderr, "-E-MIX: LOC=%04o M=%04o INV.MEMORY ADDRESS %s\n", P, w2i(a), msg);
		RUN = OFF;
		THROW;
	}
}

Word mem_read(Word a)
{
	chk_adr(a, "MEMORY READ");
	Tyme++; TIMER++;
	return mem[MAG(a)];
}

void mem_write(Word a,int f,Word w)
{
	int sign, l, r, shmt;
	unsigned mask;
	Word v;
	
	chk_adr(a, "MEMORY WRITE");
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

void mem_move(Word src, Word dst, int n)
{
	int i;

	chk_adr(src, "MOVE SOURCE");
	chk_adr(dst, "MOVE DEST.");
	chk_adr(sm_add(src, n - 1), "MOVE SOURCE END");
	chk_adr(sm_add(dst, n - 1), "MOVE DEST.END");
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
/*			                            1         2         3         4         5         6	 */
/*			                  0123456789012345678901234567890123456789012345678901234567890123*/
char    mix2asc[NMIXCHARS] = " ABCDEFGHI~JKLMNOPQR[#STUVWXYZ0123456789.,()+-*/=$<>@;:'        ";
char cr_mix2asc[NMIXCHARS] = " ABCDEFGHI~JKLMNOPQR  STUVWXYZ0123456789.,()+-                  ";
Byte asc2mix[NASCCHARS], cr_asc2mix[NASCCHARS];


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
	unsigned max_pos;
	unsigned blk_size;
	unsigned in_time;
	unsigned out_time;
    unsigned rot_time;
	unsigned seek_time;
} IOchar[] = {
    /*     name         fam    max  blk     in      out   rot     seek */
	{    "tape", BIN_RWRITE, 17280, 100,  3056,    3056,    0,     859 },
	{    "disk", BIN_RWRITE,  4096, 100,   833,     833, 6666,       0 },
    {    "drum", BIN_RWRITE,   512, 100,   333,    1333, 2666,       0 },
	{  "reader", TXT_RDONLY,     0,  16, 50000,       0,    0,       0 },
	{   "punch", TXT_APPEND,     0,  16,     0,  100000,    0,       0 },
	{ "printer", TXT_APPEND,     0,  24,     0,   21054,    0,     833 },
	{   "ptape", TXT_RWRITE,  1707,  14, 23333,  116667,    0,   23333 },
    {      NULL,       NULL,     0,  14,     0, 1166667,    0, 1166667 },
};

int devx(int u)
{
    if (u <= DEV_DK)
        return u >= DEV_DR ? 2: u / 8;
    return u - 13;
}

Word pack(Byte *buf, int offs)
{
	Word w;
	int i;
	
	w = 0;
	for (i = 0; i < BYTES; i++)
		w = (w << 6) + buf[offs + i];
	return w;
}


void unpack(Word w, Byte *buf, int offs)
{
	int i;
	
	for (i = BYTES; i >= 1; i--) {
		buf[offs + i - 1] = BYTE(w);
		w >>= 6;
	}
}

void dev_error(int u)
{
	devs[u].evt = (unsigned)-1;
}


void blk_read(int u, unsigned adr, Byte cvt_a2m[NASCCHARS])
{
	unsigned ret, n;
	int i, j, x;
    char tmp[LP_BLOCK * BYTES];
	Byte buf[LP_BLOCK * BYTES];
	unsigned Blk_size;
    char c;
	
	x = devx(u);
	Blk_size = IOchar[x].blk_size;
	
	devs[u].pos++;
	if (NULL == cvt_a2m) {
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
		buf[i] = cvt_a2m[(int) tmp[i]];

	j = 0;
	for (i = 0; i < Blk_size; i++) {
		mem[adr + i] = pack(buf, j);
		j += BYTES;
	}
	return;
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o BLKREAD FAILED\n", P, u, adr);
	dev_error(u);
}


void blk_write(int u, unsigned adr, char cvt_m2a[NMIXCHARS])
{
	unsigned ret, n;
	int i, j, x;
	Byte tmp[LP_BLOCK * BYTES];
    char buf[LP_BLOCK * BYTES + 1];     /* terminal CR/LF */
	unsigned Blk_size;
    FILE *fd;
	
	x = devx(u);
	Blk_size = IOchar[x].blk_size;
    fd = devs[u].fdout ? devs[u].fdout : devs[u].fd;
	
	devs[u].pos++;
	if (NULL == cvt_m2a) {
		ret = fwrite(&mem[adr], sizeof(Word), Blk_size, fd);
		if (ret != Blk_size)
			goto ErrOut;
		return;
	}

	n = Blk_size * BYTES;
	j = 0;
	for (i = 0; i < Blk_size; i++) {
		unpack(mem[adr + i], tmp, j);
		j += BYTES;
	}
	for (i = 0; i < n; i++)
		buf[i] = cvt_m2a[tmp[i]];
    buf[n] = '\n';

	ret = fwrite(buf, sizeof(char), n + 1, fd);
	if (ret == n + 1) {
        fflush(fd);
		return;
    }
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o BLKWRITE FAILED\n", P, u, adr);
	dev_error(u);
}


int dev_open(int u)
{
	int x;
	char devname[32];
	FILE *fd;
	
	if (NULL != devs[u].fd)
		return 0;
		
	x = devx(u);
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
		dev_error(u);
		return 1;
	}
	devs[u].fd = fd;
	return 0;
}


void dev_seek(int u, unsigned pos)
{
	int x;
	
	x = devx(u);
    if (stdin != devs[u].fd)
	    fseek(devs[u].fd, pos * IOchar[x].blk_size, SEEK_SET);
	devs[u].pos = pos;
}


unsigned diff(unsigned a, unsigned b)
{
    return a > b ? a - b : b - a;
}


void dev_ioc(int u,int M)
{
	int x;
	unsigned new_pos, old_track, new_track;
	
	x = devx(u);	
	if (!M) {
		if (u <= DEV_MT) {
			M = devs[u].pos;
			dev_seek(u,0);
		} else if (u <= DEV_DK) {
            new_pos = MAG(rX) % IOchar[x].max_pos;
            new_track = TRACK(new_pos);
            old_track = TRACK(devs[u].pos);
			M = diff(old_track, new_track);
			dev_seek(u, new_pos);
		} else if (DEV_LP == u) {
			fprintf(devs[u].fd,"\f");
			devs[u].pos %= LP_LINES;
			M = (LP_LINES - devs[u].pos);
			devs[u].pos = 0;
		} else if (DEV_PT == u) {
			M = devs[u].pos;
			dev_seek(u, 0);
		} else {
			fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.IOC UNSUPPORTED\n", P, u, M);
			dev_error(u);
			return;
		}
		devs[u].evt = Tyme + IOchar[x].seek_time * M;
		return;
	}
	if (u <= DEV_MT) {
		if (M > 0) {
			if (M > devs[u].max_pos - devs[u].pos)
				M = devs[u].max_pos - devs[u].pos;
			dev_seek(u, devs[u].pos + M);
		}
		else {
			M = -M;
			if (M > devs[u].pos)
				M = devs[u].pos;
			dev_seek(u, devs[u].pos - M);
		}
		devs[u].evt = Tyme + IOchar[x].in_time * M;
	}
}

typedef enum {DO_IOC, DO_IN, DO_OUT} EventType;

struct __Event {
    EventType what;
    unsigned when;
    Word M;
    int next;
} events[MAX_DEVS];
int EventH;


void dev_schedule(unsigned when, EventType what, int u, Word M)
{
    int i, p;

    ASSERT(0 == events[u].next);
    ASSERT(DO_NOTHING == events[u].what);

    events[u].what = what;
    events[u].when = when;
    events[u].M = M;

    i = EventH;
    while (0 != events[i].next && events[i].when <= when) {
        p = i;
        i = events[i].next;
    }
    if (EventH == i) { /* head */
        events[u].next = EventH;
        EventH = u;
    } else if (0 == events[i].next) { /* tail */
        events[i].next = u;
    } else { /* middle */
        events[p].next = u;
        events[u].next = i;
    }
}


void dev_in(int u, Word M)
{
	int x;
	char *errmsg;
	
	x = devx(u);
	chk_adr(M, "IN BUFFER");
	chk_adr(M + IOchar[x].blk_size, "IN BUFFER END");
	
	M = MAG(M);

	devs[u].evt = Tyme;
	if (u <= DEV_MT) {
		if (devs[u].pos < devs[u].max_pos) {
			blk_read(u, MAG(M), NULL);
		} else {
			errmsg = "EOT"; goto ErrOut;
		}
	} else if (u <= DEV_DK) {
		dev_ioc(u, 0);
        devs[u].evt += IOchar[x].rot_time * diff(Tyme & 63, BYTE(rX)) / 64.0;
		blk_read(u, MAG(M), NULL);
	} else if (DEV_CR == u) {
		blk_read(u, MAG(M), cr_asc2mix);
	} else if (DEV_PT == u) {
		blk_read(u, MAG(M), asc2mix);
	} else {
		errmsg = "UNSUPPORTED"; goto ErrOut;
	}
	devs[u].evt += IOchar[x].in_time;
	return;
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.IN %s\n", P, u, M, errmsg);
	dev_error(u);
}


void dev_out(Word u,Word M)
{
	int x;
	char *errmsg;
	
	x = devx(u);
	chk_adr(M, "OUT BUFFER");
	chk_adr(M + IOchar[x].blk_size, "OUT BUFFER END");
	
	M = MAG(M);

	devs[u].evt = Tyme;
	if (u <= DEV_MT) {
		if (devs[u].pos > IOchar[x].max_pos) {
			errmsg = "MAG.TAPE FULL"; goto ErrOut;
		}
		blk_write(u, M, NULL);
	    devs[u].max_pos = devs[u].pos;
	} else if (u <= DEV_DK) {
		dev_ioc(u, 0);
        devs[u].evt += IOchar[x].rot_time * diff(Tyme & 63, BYTE(rX)) / 64.0;
		blk_write(u, M, NULL);
	} else if (DEV_CR < u) {
		blk_write(u, M, u == DEV_CP ? cr_mix2asc : mix2asc);
    } else if (DEV_PT == u) {
		if (devs[u].pos > IOchar[x].max_pos) {
			errmsg = "PAPER TAPE FULL"; goto ErrOut;
		}
	} else {
		errmsg = "UNSUPPORTED"; goto ErrOut;
	}
	devs[u].evt += IOchar[x].out_time;
	return;
ErrOut:
	fprintf(stderr, "-E-MIX: LOC=%04o UNO=%02o/%04o OP.OUT %s\n", P, u, M, errmsg);
	dev_error(u);
}


void lapse(unsigned t)
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


#define GETV    field(mem_read(M),F)
#define RANGE(x,lo,hi)  (lo <= (x) && (x) <= hi)

void status(Word P)
{
	static char *sot = " X", *sci = "LEG", *ssta = "N ";
	Word w, INST, A, I, F, C, M, OP;
	int i;
	
	P = MAG(P);
	INST = mem[P];
    w = MAG(INST);
	C = BYTE(w); w >>= 6;
	F = BYTE(w); w >>= 6;
	I = BYTE(w); w >>= 6;
	A = IX_MASK & w; if (SIGN(INST)) A += SM_SIGN;
	M = I ? sm_add(A, reg[I]) : A;
	decode(C, F);
	
	/*
         1         2         3         4         5         6         7         8         9         A         B         C
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
  LOC FREQ   INSTRUCTION  OP    OPERAND     REGISTER A  REGISTER X  RI1   RI2   RI3   RI4   RI5   RI6   RJ  OV CI   TYME
N1234 1234 +1234 56 78 90 CODE +1234567890 +1234567890 +1234567890 +1234 +1234 +1234 +1234 +1234 +1234 +1234 ? ? 1234567
	*/
	if (0 == (InstCount % 31))
		fprintf(stderr,"  LOC FREQ   INSTRUCTION  OP    OPERAND     REGISTER A  REGISTER X  RI1   RI2   RI3   RI4   RI5   RI6   RJ  OV CI   TYME\n");
	fprintf(stderr, "%c%04o %04d ", ssta[CS], P, freq[P] % 9999);
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
        OP = GETV;
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

void undefined(Word C, Word F)
{
	fprintf(stderr,"-E-MIX: LOC=%04o C=%02o F=%02o OP.UNDEFINED\n", P, C, F);
    RUN = OFF;
}


void chk_ix(int x, Toggle cy)
{
	if (OFF != cy) {
		fprintf(stderr,"-E-MIX: LOC=%04o RI%d OVERFLOW\n", P, x);
		RUN = OFF;
	} else if (~IX_MASK & MAG(reg[x])) {
		fprintf(stderr,"-E-MIX: LOC=%04o RI%d UNDEFINED\n", P, x);
		RUN = OFF;
	}
}


Word cvt_num(Word sum, Word w, int *pscale)
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


Word to_num(Word a, Word x)
{
	Word sum;
	int scale;
	
	scale = 1;
	sum = cvt_num(  0, x, &scale);
	sum = cvt_num(sum, a, &scale);
	return sum;
}


Word cvt_char(Word *pw, Word a)
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


void to_char(Word *pa, Word *px, Word a)
{
	a = cvt_char(px, a);
	a = cvt_char(pa, a);
}


int step(void)
{
	Word OLDP, IR, C, F, I, A, M;
	Word w;
	int cond, x;

    OLDP = P;
	IR = mem_read(P); freq[P++]++;
	w = IR;
	C = BYTE(w); w >>= 6;
	F = BYTE(w); w >>= 6;
	I = BYTE(w); w >>= 6;
	A = IX_MASK & w; if (SIGN(IR)) A += SM_SIGN;
	M = I ? sm_add(A, reg[I]) : A;
	switch (C) {
	case 0: /*NOP*/
		break;
	case 1: /*ADD*/
		w = sm_add(rA, GETV); if (CY) OT = CY;
		if (!MAG(w)) w += SIGN(rA);
		rA = w;
		break;
	case 2: /*SUB*/
		rA = sm_sub(rA, GETV); if (CY) OT = CY;
		break;
	case 3: /*MUL*/
		sm_mul(&rA, &rX, rA, GETV);
		Tyme += 9; TIMER += 9;
		break;
	case 4: /*DIV*/
		w = GETV;
		if (!MAG(w) || MAG(rA) >= MAG(w)) {
			OT = ON;
			rA = UNDEF; rX = UNDEF;
		} else
			sm_div(&rA, &rX, rA, rX, w);
		Tyme += 11; TIMER += 11;
		break;
	case 5:
		{	Word signA, signX;
		
			signA = SIGN(rA); signX = SIGN(rX);
			switch(F){
			case 0: /*NUM*/
				rA = signA + to_num(rA, rX);
				break;
			case 1: /*CHAR*/
				to_char(&rA, &rX, rA);
				rA += signA; rX += signX;
				break;
			case 2: /*HLT*/
				RUN = OFF; break;
			default:
				undefined(C, F);
			};
		}
		break;
	case 6:
		{	Word signA, signX;

			if (SIGN(M)) {
				undefined(C, F);
			} else {
				signA = SIGN(rA); signX = SIGN(rX);
				switch(F){
				case 0: /*SLA*/
					rA = signA + sm_slax(&rA, NULL, MAG(rA), 0, 6*M, 0);
					break;
				case 1: /*SRA*/
					rA = signA + sm_srax(&rA, NULL, MAG(rA), 0, 6*M, 0);
					break;
				case 2: /*SLAX*/
					sm_slax(&rA, &rX, MAG(rA), MAG(rX), 6*M, 0);
					rA += signA; rX += signX;
					break;
				case 3: /*SRAX*/
					sm_srax(&rA, &rX, MAG(rA), MAG(rX), 6*M, 0);
					rA += signA; rX += signX;
					break;
				case 4: /*SLC*/
					sm_slax(&rA, &rX, MAG(rA), MAG(rX), 6*M, 1);
					rA += signA; rX += signX;
					break;
				case 5: /*SRC*/
					sm_srax(&rA, &rX, MAG(rA), MAG(rX), 6*M, 1);
					rA += signA; rX += signX;
					break;
				default:
					undefined(C, F);
				}
			}
		}
		break;
	case 7: /*MOVE*/
		if (F) {
			mem_move(M, rI1, F);
			rI1 = sm_add(rI1, F);
			chk_ix(1, CY);
		}
		break;
	case  8: case  9: case 10: case 11: case 12: case 13: case 14: case 15: /*LDr*/
		x = C - 8;
		reg[x] = GETV;
		if (0 < x && x < 7) chk_ix(x, OFF);
		break;
	case 16: case 17: case 18: case 19: case 20: case 21: case 22: case 23: /*LDrN*/
		x = C - 16;
		reg[x] = sm_neg(GETV);
		if (0 < x && x < 7) chk_ix(x, OFF);
		break;
	case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31: case 32: case 33: /*STr, STJ, STZ*/
		mem_write(M, F, reg[C - 24]);
		break;
	case 34: /*JBUS*/
        dev_open(F);
		if (Tyme < devs[F].evt) {
            rJ = P;
            if (OLDP == M)
                lapse(devs[F].evt);
            else
			    P = M;
		}
		break;
	case 35: /*IOC*/
		if (dev_open(F))
			return 0;
        lapse(devs[F].evt);
		dev_ioc(F, M);
		break;
	case 36: /*IN*/
		if (dev_open(F))
			return 0;
		lapse(devs[F].evt);
		dev_in(F, M);
		break;
	case 37: /*OUT*/
		if (dev_open(F))
			return 0;
		lapse(devs[F].evt);
		dev_out(F, M);
		break;		
	case 38: /*JRED*/
        dev_open(F);
		if (devs[F].evt <= Tyme) {
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
			undefined(C, F);
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
			undefined(C, F);
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
			reg[x] = sm_add(reg[x], M); if (CY) OT = ON;
			if (0 < x && x < 7) chk_ix(x, CY);
			break;
		case 1: /*DECr*/
			reg[x] = sm_sub(reg[x], M); if (CY) OT = ON;
			if (0 < x && x < 7) chk_ix(x, CY);
			break;
		case 2: /*ENTr*/
			reg[x] = M;
			if (!MAG(M))
				reg[x] += SIGN(IR);
			break;
		case 3: /*ENNr*/
			reg[x] = sm_neg(M);
			break;
		default:
			undefined(C, F);
		}
		break;
	case 56: case 57: case 58: case 59: case 60: case 61: case 62: case 63: /*CMPr*/
		w = sm_sub(field(reg[C - 56], F), GETV);
		if (SIGN(w)) CI = LESS;
		else if (!MAG(w)) CI = EQUAL;
		else CI = GREATER;
		break;
	}
    InstCount++;
	return 1;
}


void run(Word p)
{
	P = p; RUN = ON;
	while (RUN) {
		if (TRACE)
			status(P);
		step();
	}
}

void init(void)
{
	int i;

    LPT = stderr;
	InstCount = 0;
	Tyme = 0; IdleTyme = 0;
    ZERO = 0;
	RUN = OFF; TRACE = OFF;
    CS = OFF;
    TRANS = OFF;
    for (i = 0; i < sizeof(TRANSNM); i++)
        TRANSNM[i] = 0;

	for (i = 0; i <= MAX_MEM; i++)
		freq[i] = 0;

	for (i = 0; i < NASCCHARS; i++)
		asc2mix[i] = cr_asc2mix[i] = ' ';

	for (i = 0; i < NMIXCHARS; i++) {
		asc2mix[(int) mix2asc[i]] = i;
		cr_asc2mix[(int) cr_mix2asc[i]] = i;
	}
    asc2mix[' '] = cr_asc2mix[' '] = 0;

	for (i = 0; i < MAX_DEVS; i++)
		devs[i].fd = devs[i].fdout = NULL;

    devs[DEV_TT].fd = stdin;
    devs[DEV_TT].fdout = stdout;
}


void finish(void)
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


void usage(void)
{
	fprintf(stderr, "usage: mix [-g dev][-t][-x name]\n");
    fprintf(stderr, "options:\n");
    fprintf(stderr, "    -g unit    push GO button on unit\n");
    fprintf(stderr, "    -t         enable tracing\n");
    fprintf(stderr, "    -x name    print nonzero locations in TRANS fmt\n");
	exit(1);
}


void go(int u)
{
	if (u < 0 || u >= MAX_DEVS || DEV_CP == u || DEV_LP == u || DEV_TT == u)
		usage();
	dev_in(u, 0);
	run(0);
}


void stats(FILE *fd, int STRIDE, int dotrans)
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
        fprintf(LPT, "TRANS0%04d%*s", dotrans - 1, 70, " ");
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
	init();
	for (i = 1; i < argc; i++) {
		arg = argv[i];
		if ('-' != *arg)
			usage();
		switch (arg[1]) {
		case 'g':
			if (i + 1 < argc)
				u = atoi(argv[++i]);
			else
				usage();
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
                usage();
            TRANS = ON;
            break;
		default:
			usage();
		}
	}

    if (0 == dev_open(u))
	    go(u);
    stats(stderr, 4, 0);
    if (TRANS) {
        strcpy(buf, TRANSNM);
        strcat(buf, ".tra");
        strtolower(buf);
        if ((fout = fopen(buf, TXT_APPEND))) {
            stats(fout, 7, 1);
            fclose(fout);
        } else
            fprintf(stderr, "-E-MIX: CANNOT OPEN %s\n", buf);
    }
    finish();

	return 0;
}

/* vim: set ts=4 sw=4 et: */
