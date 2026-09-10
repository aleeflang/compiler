#include "lang/keywords.h"
#include "af8/af8_utf8.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * alifc — علیف compiler  (.alif AF8 → .afb assembly)
 * Does not link the VM. afas / alif.exe are unchanged.
 */

#define ALIF_EXT        ".alif"
#define AFB_EXT         ".afb"
#define MAX_IDENT       64
#define MAX_SYMS        128
#define VAR_RAM_LIMIT   512

enum {
    TK_EOF, TK_SEMI, TK_EQ, TK_EQEQ, TK_NE, TK_LT, TK_GT, TK_LE, TK_GE,
    TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH, TK_PERCENT,
    TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE,
    TK_NUM, TK_IDENT,
    TK_SHURU, TK_KHATAM, TK_ADAD, TK_HARF, TK_AGAR, TK_WARNA, TK_JABTAK,
    TK_LIKHO, TK_PARHO, TK_AUR, TK_YA, TK_NAHIN, TK_SACH, TK_JHOOT
};

enum {
    N_NUM, N_ID, N_BOOL, N_BIN, N_UN,
    N_DECL, N_ASSIGN, N_PRINT, N_READ, N_IF, N_WHILE
};

enum {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD,
    OP_EQ, OP_NE, OP_LT, OP_GT, OP_LE, OP_GE,
    OP_AND, OP_OR, OP_NOT, OP_NEG, OP_POS
};

struct tok {
    int kind;
    int line;
    int32_t num;
    unsigned char ident[MAX_IDENT];
    int ident_len;
};

struct node {
    int kind, op, line, sym;
    int32_t num;
    struct node *a, *b, *c, *next;
};

struct kw {
    const unsigned char *b;
    int len;
    int tk;
};

static unsigned char k_shuru[]  = { KW_SHURU };
static unsigned char k_khatam[] = { KW_KHATAM };
static unsigned char k_adad[]   = { KW_ADAD };
static unsigned char k_harf[]   = { KW_HARF };
static unsigned char k_agar[]   = { KW_AGAR };
static unsigned char k_warna[]  = { KW_WARNA };
static unsigned char k_jabtak[] = { KW_JABTAK };
static unsigned char k_likho[]  = { KW_LIKHO };
static unsigned char k_parho[]  = { KW_PARHO };
static unsigned char k_aur[]    = { KW_AUR };
static unsigned char k_ya[]     = { KW_YA };
static unsigned char k_nahin[]  = { KW_NAHIN };
static unsigned char k_sach[]   = { KW_SACH };
static unsigned char k_jhoot[]  = { KW_JHOOT };

static const struct kw kws[] = {
    { k_shuru,  KW_LEN_SHURU,  TK_SHURU },
    { k_khatam, KW_LEN_KHATAM, TK_KHATAM },
    { k_adad,   KW_LEN_ADAD,   TK_ADAD },
    { k_harf,   KW_LEN_HARF,   TK_HARF },
    { k_agar,   KW_LEN_AGAR,   TK_AGAR },
    { k_warna,  KW_LEN_WARNA,  TK_WARNA },
    { k_jabtak, KW_LEN_JABTAK, TK_JABTAK },
    { k_likho,  KW_LEN_LIKHO,  TK_LIKHO },
    { k_parho,  KW_LEN_PARHO,  TK_PARHO },
    { k_aur,    KW_LEN_AUR,    TK_AUR },
    { k_ya,     KW_LEN_YA,     TK_YA },
    { k_nahin,  KW_LEN_NAHIN,  TK_NAHIN },
    { k_sach,   KW_LEN_SACH,   TK_SACH },
    { k_jhoot,  KW_LEN_JHOOT,  TK_JHOOT },
    { NULL, 0, 0 }
};

static const char *g_path;
static unsigned char *g_src;
static size_t g_len, g_pos;
static int g_line = 1;
static struct tok g_la;
static int g_have_la;
static FILE *g_out;
static int g_lab;

static struct {
    unsigned char name[MAX_IDENT];
    int len;
    unsigned addr;
} g_sym[MAX_SYMS];
static int g_nsym;
static unsigned g_ram;

static int eq_ci(char a, char b)
{
    if (a >= 'A' && a <= 'Z') a = (char)(a - 'A' + 'a');
    if (b >= 'A' && b <= 'Z') b = (char)(b - 'A' + 'a');
    return a == b;
}

static int has_ext(const char *path, const char *ext)
{
    size_t n, i, e;
    if (path == NULL || ext == NULL)
        return 0;
    n = strlen(path);
    e = strlen(ext);
    if (n < e)
        return 0;
    for (i = 0; i < e; i++)
        if (!eq_ci(path[n - e + i], ext[i]))
            return 0;
    return 1;
}

static void die(int line, const char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "alifc:%s:%d: ", g_path ? g_path : "?", line);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fputc('\n', stderr);
    exit(1);
}

static int peekb(void)
{
    if (g_pos >= g_len)
        return -1;
    return g_src[g_pos];
}

static int getb(void)
{
    int c;
    if (g_pos >= g_len)
        return -1;
    c = g_src[g_pos++];
    if (c == '\n')
        g_line++;
    return c;
}

static int ident_eq(const unsigned char *a, int na, const unsigned char *b, int nb)
{
    return na == nb && memcmp(a, b, (size_t)na) == 0;
}

static int kw_of(const unsigned char *s, int n)
{
    const struct kw *k;
    for (k = kws; k->b; k++) {
        if (ident_eq(s, n, k->b, k->len))
            return k->tk;
    }
    return TK_IDENT;
}

static struct tok lex(void)
{
    struct tok t;
    int c;

    memset(&t, 0, sizeof t);
    for (;;) {
        c = peekb();
        if (c < 0) {
            t.kind = TK_EOF;
            t.line = g_line;
            return t;
        }
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            getb();
            continue;
        }
        if (c == '/' && g_pos + 1 < g_len && g_src[g_pos + 1] == '/') {
            getb();
            getb();
            while ((c = peekb()) >= 0 && c != '\n')
                getb();
            continue;
        }
        if (AF8_IS_HARAKAT((unsigned char)c))
            die(g_line, "harakat not allowed in v1");
        if (c == AF8_INVALID)
            die(g_line, "illegal byte 0xFF");
        break;
    }

    t.line = g_line;
    c = getb();

    if (c == AF8_SEMICOLON || c == ';') {
        t.kind = TK_SEMI;
        return t;
    }
    if (c == '+') { t.kind = TK_PLUS; return t; }
    if (c == '-') { t.kind = TK_MINUS; return t; }
    if (c == '*') { t.kind = TK_STAR; return t; }
    if (c == '%') { t.kind = TK_PERCENT; return t; }
    if (c == '(') { t.kind = TK_LPAREN; return t; }
    if (c == ')') { t.kind = TK_RPAREN; return t; }
    if (c == '{') { t.kind = TK_LBRACE; return t; }
    if (c == '}') { t.kind = TK_RBRACE; return t; }
    if (c == '/') { t.kind = TK_SLASH; return t; }
    if (c == '=') {
        if (peekb() == '=') { getb(); t.kind = TK_EQEQ; }
        else t.kind = TK_EQ;
        return t;
    }
    if (c == '!') {
        if (peekb() == '=') { getb(); t.kind = TK_NE; return t; }
        die(t.line, "unexpected '!'");
    }
    if (c == '<') {
        if (peekb() == '=') { getb(); t.kind = TK_LE; }
        else t.kind = TK_LT;
        return t;
    }
    if (c == '>') {
        if (peekb() == '=') { getb(); t.kind = TK_GE; }
        else t.kind = TK_GT;
        return t;
    }

    if (AF8_IS_LETTER((unsigned char)c)) {
        t.ident[t.ident_len++] = (unsigned char)c;
        for (;;) {
            int p = peekb();
            if (p < 0)
                break;
            if (AF8_IS_LETTER((unsigned char)p) || AF8_IS_URDU_DIGIT((unsigned char)p)
                || (p >= '0' && p <= '9')) {
                if (t.ident_len + 1 >= MAX_IDENT)
                    die(t.line, "name too long");
                t.ident[t.ident_len++] = (unsigned char)getb();
            } else
                break;
        }
        t.kind = kw_of(t.ident, t.ident_len);
        return t;
    }

    if (c >= '0' && c <= '9') {
        uint32_t v = (uint32_t)(c - '0');
        while (peekb() >= '0' && peekb() <= '9') {
            int d = getb() - '0';
            if (v > 429496729u)
                die(t.line, "number too large");
            v = v * 10u + (uint32_t)d;
        }
        if (AF8_IS_URDU_DIGIT((unsigned char)peekb()))
            die(t.line, "do not mix ASCII and Urdu digits");
        if (v > 65535u)
            die(t.line, "literal must fit MOVI (0..65535) in v1");
        t.kind = TK_NUM;
        t.num = (int32_t)v;
        return t;
    }

    if (AF8_IS_URDU_DIGIT((unsigned char)c)) {
        uint32_t v = (uint32_t)((unsigned char)c - AF8_DIGIT_0);
        while (AF8_IS_URDU_DIGIT((unsigned char)peekb())) {
            int d = getb();
            v = v * 10u + (uint32_t)((unsigned char)d - AF8_DIGIT_0);
            if (v > 65535u)
                die(t.line, "literal must fit MOVI (0..65535) in v1");
        }
        if (peekb() >= '0' && peekb() <= '9')
            die(t.line, "do not mix ASCII and Urdu digits");
        t.kind = TK_NUM;
        t.num = (int32_t)v;
        return t;
    }

    die(t.line, "unexpected byte 0x%02X", c & 0xFF);
    return t;
}

static struct tok peek(void)
{
    if (!g_have_la) {
        g_la = lex();
        g_have_la = 1;
    }
    return g_la;
}

static struct tok take(void)
{
    struct tok t = peek();
    g_have_la = 0;
    return t;
}

static void expect(int kind, const char *what)
{
    struct tok t = take();
    if (t.kind != kind)
        die(t.line, "expected %s", what);
}

static struct node *nd(int kind, int line)
{
    struct node *n = (struct node *)calloc(1, sizeof *n);
    if (n == NULL)
        die(line, "out of memory");
    n->kind = kind;
    n->line = line;
    return n;
}

static int find_sym(const unsigned char *name, int len)
{
    int i;
    for (i = 0; i < g_nsym; i++)
        if (ident_eq(g_sym[i].name, g_sym[i].len, name, len))
            return i;
    return -1;
}

static int add_sym(const unsigned char *name, int len, int line)
{
    if (find_sym(name, len) >= 0)
        die(line, "duplicate name");
    if (g_nsym >= MAX_SYMS)
        die(line, "too many variables");
    if (g_ram + 4 > VAR_RAM_LIMIT)
        die(line, "too much variable RAM");
    memcpy(g_sym[g_nsym].name, name, (size_t)len);
    g_sym[g_nsym].len = len;
    g_sym[g_nsym].addr = g_ram;
    g_ram += 4;
    return g_nsym++;
}

static struct node *parse_expr(void);

static struct node *parse_primary(void)
{
    struct tok t = take();
    struct node *n;
    if (t.kind == TK_NUM) {
        n = nd(N_NUM, t.line);
        n->num = t.num;
        return n;
    }
    if (t.kind == TK_SACH || t.kind == TK_JHOOT) {
        n = nd(N_BOOL, t.line);
        n->num = (t.kind == TK_SACH) ? 1 : 0;
        return n;
    }
    if (t.kind == TK_IDENT) {
        int s = find_sym(t.ident, t.ident_len);
        if (s < 0)
            die(t.line, "unknown name");
        n = nd(N_ID, t.line);
        n->sym = s;
        return n;
    }
    if (t.kind == TK_LPAREN) {
        n = parse_expr();
        expect(TK_RPAREN, "')'");
        return n;
    }
    die(t.line, "expected expression");
    return NULL;
}

static struct node *parse_unary(void)
{
    struct tok t = peek();
    struct node *n;
    if (t.kind == TK_NAHIN || t.kind == TK_MINUS || t.kind == TK_PLUS) {
        take();
        n = nd(N_UN, t.line);
        n->op = (t.kind == TK_NAHIN) ? OP_NOT : (t.kind == TK_MINUS) ? OP_NEG : OP_POS;
        n->a = parse_unary();
        return n;
    }
    return parse_primary();
}

static struct node *parse_bin(struct node *(*down)(void), int a, int b, int opa, int opb)
{
    struct node *n = down();
    for (;;) {
        struct tok t = peek();
        struct node *p;
        int op;
        if (t.kind == a) op = opa;
        else if (b >= 0 && t.kind == b) op = opb;
        else break;
        take();
        p = nd(N_BIN, t.line);
        p->op = op;
        p->a = n;
        p->b = down();
        n = p;
    }
    return n;
}

static struct node *parse_mul(void)
{
    struct node *n = parse_unary();
    for (;;) {
        struct tok t = peek();
        struct node *p;
        int op;
        if (t.kind == TK_STAR) op = OP_MUL;
        else if (t.kind == TK_SLASH) op = OP_DIV;
        else if (t.kind == TK_PERCENT) op = OP_MOD;
        else break;
        take();
        p = nd(N_BIN, t.line);
        p->op = op;
        p->a = n;
        p->b = parse_unary();
        n = p;
    }
    return n;
}

static struct node *parse_add(void)
{
    return parse_bin(parse_mul, TK_PLUS, TK_MINUS, OP_ADD, OP_SUB);
}

static struct node *parse_cmp(void)
{
    struct node *n = parse_add();
    for (;;) {
        struct tok t = peek();
        struct node *p;
        int op = -1;
        if (t.kind == TK_EQEQ) op = OP_EQ;
        else if (t.kind == TK_NE) op = OP_NE;
        else if (t.kind == TK_LT) op = OP_LT;
        else if (t.kind == TK_GT) op = OP_GT;
        else if (t.kind == TK_LE) op = OP_LE;
        else if (t.kind == TK_GE) op = OP_GE;
        else break;
        take();
        p = nd(N_BIN, t.line);
        p->op = op;
        p->a = n;
        p->b = parse_add();
        n = p;
    }
    return n;
}

static struct node *parse_and(void)
{
    return parse_bin(parse_cmp, TK_AUR, -1, OP_AND, 0);
}

static struct node *parse_expr(void)
{
    return parse_bin(parse_and, TK_YA, -1, OP_OR, 0);
}

static struct node *parse_stmt(void);
static struct node *parse_block(void);

static struct node *parse_stmt_list_until(int end_tk)
{
    struct node *head = NULL, **tail = &head;
    while (peek().kind != end_tk && peek().kind != TK_EOF) {
        *tail = parse_stmt();
        tail = &(*tail)->next;
    }
    return head;
}

static struct node *parse_block(void)
{
    expect(TK_LBRACE, "'{'");
    {
        struct node *body = parse_stmt_list_until(TK_RBRACE);
        expect(TK_RBRACE, "'}'");
        return body;
    }
}

static struct node *parse_stmt(void)
{
    struct tok t = peek();
    struct node *n;
    if (t.kind == TK_ADAD) {
        take();
        t = take();
        if (t.kind != TK_IDENT)
            die(t.line, "expected name after 'adad'");
        n = nd(N_DECL, t.line);
        n->sym = add_sym(t.ident, t.ident_len, t.line);
        expect(TK_EQ, "'='");
        n->a = parse_expr();
        expect(TK_SEMI, "';'");
        return n;
    }
    // Harf ( Char ) check
    if (t.kind == TK_HARF) {
        take();
        t = take();
        if (t.kind != TK_IDENT)
            die(t.line, "expected name after 'harf'");
        n = nd(N_DECL, t.line);
        n->sym = add_sym(t.ident, t.ident_len, t.line);
        expect(TK_EQ, "'='");
        n->a = parse_expr();
        printf("Num type - %d\n", TK_HARF);
        expect(TK_SEMI, "';'");
        
        return n;
    }
    if (t.kind == TK_LIKHO) {
        take();
        n = nd(N_PRINT, t.line);
        n->a = parse_expr();
        expect(TK_SEMI, "';'");
        return n;
    }
    if (t.kind == TK_PARHO) {
        take();
        t = take();
        if (t.kind != TK_IDENT)
            die(t.line, "expected name after 'parho'");
        n = nd(N_READ, t.line);
        n->sym = find_sym(t.ident, t.ident_len);
        if (n->sym < 0)
            die(t.line, "unknown name");
        expect(TK_SEMI, "';'");
        return n;
    }
    if (t.kind == TK_AGAR) {
        take();
        n = nd(N_IF, t.line);
        n->a = parse_expr();
        n->b = parse_block();
        if (peek().kind == TK_WARNA) {
            take();
            n->c = parse_block();
        }
        return n;
    }
    if (t.kind == TK_JABTAK) {
        take();
        n = nd(N_WHILE, t.line);
        n->a = parse_expr();
        n->b = parse_block();
        return n;
    }
    if (t.kind == TK_IDENT) {
        t = take();
        n = nd(N_ASSIGN, t.line);
        n->sym = find_sym(t.ident, t.ident_len);
        if (n->sym < 0)
            die(t.line, "unknown name");
        expect(TK_EQ, "'='");
        n->a = parse_expr();
        expect(TK_SEMI, "';'");
        return n;
    }
    die(t.line, "expected statement");
    return NULL;
}

static int newlab(void)
{
    return g_lab++;
}

static void emit(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(g_out, fmt, ap);
    va_end(ap);
    fputc('\n', g_out);
}

static void gen_expr(struct node *n);

static void gen_load_addr(int sym)
{
    emit("        MOVI    R2, %u", g_sym[sym].addr);
}

static void gen_cmp(int jcc)
{
    int L = newlab();
    int D = newlab();
    emit("        CMP     R2, R1");
    emit("        MOVI    R1, 1");
    emit("        %s      L%d", jcc == OP_EQ ? "JE" : jcc == OP_NE ? "JNE"
        : jcc == OP_LT ? "JL" : jcc == OP_GT ? "JG"
        : jcc == OP_LE ? "JLE" : "JGE", L);
    emit("        MOVI    R1, 0");
    emit("        JMP     L%d", D);
    emit("L%d:", L);
    emit("L%d:", D);
}

static void gen_expr(struct node *n)
{
    int L0, L1;
    const char *arith;

    if (n == NULL)
        return;
    switch (n->kind) {
    case N_NUM:
    case N_BOOL:
        emit("        MOVI    R1, %d", (int)n->num);
        return;
    case N_ID:
        gen_load_addr(n->sym);
        emit("        LOAD    R1, [R2]");
        return;
    case N_UN:
        gen_expr(n->a);
        if (n->op == OP_POS)
            return;
        if (n->op == OP_NEG) {
            emit("        NEG     R1, R1");
            return;
        }
        L0 = newlab();
        L1 = newlab();
        emit("        CMPI    R1, 0");
        emit("        JE      L%d", L0);
        emit("        MOVI    R1, 0");
        emit("        JMP     L%d", L1);
        emit("L%d:", L0);
        emit("        MOVI    R1, 1");
        emit("L%d:", L1);
        return;
    case N_BIN:
        if (n->op == OP_AND) {
            L0 = newlab();
            L1 = newlab();
            gen_expr(n->a);
            emit("        CMPI    R1, 0");
            emit("        JE      L%d", L0);
            gen_expr(n->b);
            emit("        CMPI    R1, 0");
            emit("        JE      L%d", L0);
            emit("        MOVI    R1, 1");
            emit("        JMP     L%d", L1);
            emit("L%d:", L0);
            emit("        MOVI    R1, 0");
            emit("L%d:", L1);
            return;
        }
        if (n->op == OP_OR) {
            L0 = newlab();
            L1 = newlab();
            gen_expr(n->a);
            emit("        CMPI    R1, 0");
            emit("        JNE     L%d", L0);
            gen_expr(n->b);
            emit("        CMPI    R1, 0");
            emit("        JNE     L%d", L0);
            emit("        MOVI    R1, 0");
            emit("        JMP     L%d", L1);
            emit("L%d:", L0);
            emit("        MOVI    R1, 1");
            emit("L%d:", L1);
            return;
        }
        gen_expr(n->a);
        emit("        PUSH    R1");
        gen_expr(n->b);
        emit("        POP     R2");
        switch (n->op) {
        case OP_ADD: arith = "ADD"; break;
        case OP_SUB: arith = "SUB"; break;
        case OP_MUL: arith = "MUL"; break;
        case OP_DIV: arith = "DIV"; break;
        case OP_MOD: arith = "MOD"; break;
        default:
            gen_cmp(n->op);
            return;
        }
        emit("        %s     R1, R2, R1", arith);
        return;
    default:
        die(n->line, "internal: not an expression");
    }
}

static void gen_stmt(struct node *n)
{
    int L0, L1;
    for (; n; n = n->next) {
        switch (n->kind) {
        case N_DECL:
        case N_ASSIGN:
            gen_expr(n->a);
            gen_load_addr(n->sym);
            emit("        STORE   R1, [R2]");
            break;
        case N_PRINT:
            gen_expr(n->a);
            emit("        OUT     1, R1");
            break;
        case N_READ:
            emit("        IN      R1, 0");
            gen_load_addr(n->sym);
            emit("        STORE   R1, [R2]");
            break;
        case N_IF:
            L0 = newlab();
            L1 = newlab();
            gen_expr(n->a);
            emit("        CMPI    R1, 0");
            emit("        JE      L%d", L0);
            gen_stmt(n->b);
            emit("        JMP     L%d", L1);
            emit("L%d:", L0);
            if (n->c)
                gen_stmt(n->c);
            emit("L%d:", L1);
            break;
        case N_WHILE:
            L0 = newlab();
            L1 = newlab();
            emit("L%d:", L0);
            gen_expr(n->a);
            emit("        CMPI    R1, 0");
            emit("        JE      L%d", L1);
            gen_stmt(n->b);
            emit("        JMP     L%d", L0);
            emit("L%d:", L1);
            break;
        default:
            die(n->line, "internal: bad statement");
        }
    }
}

static char *derive_out(const char *in)
{
    size_t n = strlen(in), e = strlen(ALIF_EXT), base;
    char *out;
    if (n >= e && has_ext(in, ALIF_EXT))
        base = n - e;
    else
        base = n;
    out = (char *)malloc(base + strlen(AFB_EXT) + 1);
    if (out == NULL)
        die(1, "out of memory");
    memcpy(out, in, base);
    memcpy(out + base, AFB_EXT, strlen(AFB_EXT) + 1);
    return out;
}

static void usage(void)
{
    fprintf(stderr, "usage: alifc <program.alif> [-o <program.afb>]\n");
    exit(1);
}

int main(int argc, char **argv)
{
    const char *in_path = NULL, *out_path = NULL;
    char *derived = NULL;
    FILE *fp;
    struct node *body;
    int i;
    long flen;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0) {
            if (i + 1 >= argc)
                usage();
            out_path = argv[++i];
        } else if (argv[i][0] == '-')
            usage();
        else if (in_path == NULL)
            in_path = argv[i];
        else
            usage();
    }
    if (in_path == NULL)
        usage();
    if (!has_ext(in_path, ALIF_EXT)) {
        fprintf(stderr, "alifc: source must be a .alif file\n");
        usage();
    }
    if (out_path == NULL) {
        derived = derive_out(in_path);
        out_path = derived;
    } else if (!has_ext(out_path, AFB_EXT)) {
        fprintf(stderr, "alifc: output must be a .afb file\n");
        usage();
    }

    g_path = in_path;
    fp = fopen(in_path, "rb");
    if (fp == NULL) {
        fprintf(stderr, "alifc: cannot open %s\n", in_path);
        return 1;
    }
    if (fseek(fp, 0, SEEK_END) != 0 || (flen = ftell(fp)) < 0) {
        fclose(fp);
        die(1, "cannot read file");
    }
    g_len = (size_t)flen;
    g_src = (unsigned char *)malloc(g_len + 1);
    if (g_src == NULL) {
        fclose(fp);
        die(1, "out of memory");
    }
    rewind(fp);
    if (fread(g_src, 1, g_len, fp) != g_len) {
        fclose(fp);
        die(1, "truncated file");
    }
    fclose(fp);
    g_src[g_len] = 0;

    if (af8_looks_like_utf8(g_src, g_len)) {
        unsigned char *af8 = NULL;
        size_t n = 0;
        char err[256];
        if (af8_utf8_to_af8(g_src, g_len, &af8, &n, err, sizeof err) != 0)
            die(1, "%s", err);
        free(g_src);
        g_src = af8;
        g_len = n;
    }

    expect(TK_SHURU, "shuru");
    body = parse_stmt_list_until(TK_KHATAM);
    expect(TK_KHATAM, "khatam");
    if (peek().kind != TK_EOF)
        die(peek().line, "junk after khatam");

    g_out = fopen(out_path, "w");
    if (g_out == NULL) {
        fprintf(stderr, "alifc: cannot write %s\n", out_path);
        return 1;
    }
    emit("; generated by alifc from %s", in_path);
    gen_stmt(body);
    emit("        HLT");
    fclose(g_out);
    free(derived);
    return 0;
}
