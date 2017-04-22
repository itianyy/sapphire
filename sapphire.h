#ifndef SAPPHIRE_H
#define SAPPHIRE_H


#define	EOF_CH		((char)0xFF)

#define NEXT_CHAR   do{curChar = NextChar();}while(0)
#define	NEXT_TOKEN	do{get_token();}while(0)

#define DO_ALLOC(p,n)    ((p) = (typeof(p))mem_alloc((n)*sizeof(*(p))))
#define CALLOC(p,n)   memset(DO_ALLOC(p,n), 0, (n)*sizeof (*(p)))

#define DO_REALLOC(p,n)    ((p) = (typeof(p))mem_realloc((p),(n)*sizeof(*(p))))

#define CREATE_AST_NODE(p, k) \
    CALLOC(p,1);                \
    p->kind = NK_##k;

int my_sprintf(char *out, char *format, ...);

typedef enum {
    TK_NA,      //Not Available
    TK_NUM,     //number
    TK_SYM,     //symbol
    TK_STR,     //string
    TK_LPAREN,	//"("
    TK_RPAREN,  //")"
    TK_LBRACE,  //"{"
    TK_RBRACE,  //"}"
    TK_EOF,     //"EOF"
}TokenKind;


typedef	union{
	int  numVal;
    char *str;
}Value;

typedef struct{
	TokenKind kind;
	Value value;
}Token;

typedef enum {
    NK_SAP,     //sapphire
    NK_SEXPR,   //sexpr
    NK_QEXPR,   //qexpr
    NK_NUM,     //number
    NK_SYM,     //symbol
    NK_STR,     //string
    NK_ERR,     //error
}NodeKind;

typedef struct astNode{
    NodeKind kind;
	Value value;
    struct astNode *child;
    struct astNode *brother;
}AstNode;


typedef struct{
    int line;
    int col;
}Coord;

typedef enum{ 
    LVAL_ERR = 0,
    LVAL_NUM,
    LVAL_SYM, 
    LVAL_STR,
    LVAL_FUN,
    LVAL_SEXPR,
    LVAL_QEXPR,
}lval_type_e;

struct _lval;
struct _lenv;
typedef struct _lval lval;
typedef struct _lenv lenv;
typedef lval*(*lbuiltin)(lenv*, lval*);


struct _lval{
    lval_type_e type;
    
    long num;
    char* err;
    char* sym;
    char* str;
    
    lbuiltin builtin;
    lenv* env;
    lval* formals;
    lval* body;
    
    int count;
    struct _lval** cell;
};

typedef struct _relationship{
    char* sym;
    lval* val;
    struct _relationship* next;
}relationship;

struct _lenv {
    struct _lenv* par;
    relationship* relationships;
};

void DeleteAst(AstNode *a);

#define Check(f,r) \
    if((r)&&((r)->kind == NK_ERR)){ \
        if(f){ \
            DeleteAst(f); \
        } \
        return r; \
    }

#define CREATE_ERR_TOKEN(fmt, ...) \
    curToken.kind = TK_NA; \
    DO_ALLOC(curToken.value.str,32); \
    my_sprintf(curToken.value.str,"(%d,%d):error:"fmt,coord.line,coord.col ,##__VA_ARGS__);

#define CREATE_ERR_AST(p,fmt, ...) \
    CREATE_AST_NODE(p,ERR) \
    DO_ALLOC(p->value.str,64); \
    my_sprintf(p->value.str,"(%d,%d):error:"fmt,coord.line,coord.col ,##__VA_ARGS__);

#define lval_err(fmt, ...) \
    ({lval* v; \
    CALLOC(v,1); \
    v->type = LVAL_ERR; \
    DO_ALLOC(v->err,128); \
    my_sprintf(v->err,fmt, ##__VA_ARGS__); \
    v; \
    })

#define LASSERT(args, cond, fmt, ...) \
  if (!(cond)) { lval* err = lval_err(fmt, ##__VA_ARGS__); lval_del(args); return err; }

#define LASSERT_TYPE(func, args, index, expect) \
  LASSERT(args, args->cell[index]->type == expect, \
    "Function '%s' passed incorrect type for argument %i. Got %s, Expected %s.", \
    func, index, ltype_name(args->cell[index]->type), ltype_name(expect))

#define LASSERT_NUM(func, args, num) \
  LASSERT(args, args->count == num, \
    "Function '%s' passed incorrect number of arguments. Got %i, Expected %i.", \
    func, args->count, num)

#define LASSERT_NOT_EMPTY(func, args, index) \
  LASSERT(args, args->cell[index]->count != 0, \
    "Function '%s' passed {} for argument %i.", func, index);


#endif

