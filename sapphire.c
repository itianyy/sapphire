#include "./sapphire.h"

//The grammar of sapphire

//number:
//  -?[0-9]+

//symbol:
//  [a-z A-Z 0-9 _+-*/\=<>!&]+

//string:
//  "(\.|[^"])*"

//comment:
//  ;[^\r\n]*

//<sexpr>:
//  ( <expr>* )

//<qexpr>:
//  { <expr>* }

//<expr>:
//  number
//  symbol
//  string
//  <sexpr>
//  <qexpr>

//<sapphire>:
//  <expr>*


//begin
typedef unsigned char uint8_t;
typedef unsigned int size_t;

#define NULL 0

#define heapMINIMUM_BLOCK_SIZE	( ( size_t ) ( xHeapStructSize << 1 ) )
#define heapBITS_PER_BYTE		( ( size_t ) 8 )
#define configTOTAL_HEAP_SIZE			( ( size_t ) ( 1024*1024 ) )
#define portBYTE_ALIGNMENT              4
#define portBYTE_ALIGNMENT_MASK	( 0x0003 )

typedef struct A_BLOCK_LINK
{
	struct A_BLOCK_LINK *pxNextFreeBlock;
	size_t xBlockSize;
} BlockLink_t;

static uint8_t ucHeap[ configTOTAL_HEAP_SIZE ];
static const size_t xHeapStructSize	= ( sizeof( BlockLink_t ) + ( ( size_t ) ( portBYTE_ALIGNMENT - 1 ) ) ) & ~( ( size_t ) portBYTE_ALIGNMENT_MASK );

static BlockLink_t xStart, *pxEnd = NULL;
static size_t xBlockAllocatedBit = 0;

static void prvHeapInit( void )
{
    BlockLink_t *pxFirstFreeBlock;
    uint8_t *pucAlignedHeap;
    size_t uxAddress;
    size_t xTotalHeapSize = configTOTAL_HEAP_SIZE;

	
	uxAddress = ( size_t ) ucHeap;

	if( ( uxAddress & portBYTE_ALIGNMENT_MASK ) != 0 )
	{
		uxAddress += ( portBYTE_ALIGNMENT - 1 );
		uxAddress &= ~( ( size_t ) portBYTE_ALIGNMENT_MASK );
		xTotalHeapSize -= uxAddress - ( size_t ) ucHeap;
	}

	pucAlignedHeap = ( uint8_t * ) uxAddress;

	xStart.pxNextFreeBlock =  (struct A_BLOCK_LINK *)pucAlignedHeap;
	xStart.xBlockSize = ( size_t ) 0;

	uxAddress = ( ( size_t ) pucAlignedHeap ) + xTotalHeapSize;
	uxAddress -= xHeapStructSize;
	uxAddress &= ~( ( size_t ) portBYTE_ALIGNMENT_MASK );
	pxEnd = ( BlockLink_t * ) uxAddress;
	pxEnd->xBlockSize = 0;
	pxEnd->pxNextFreeBlock = NULL;

	pxFirstFreeBlock = ( BlockLink_t * ) pucAlignedHeap;
	pxFirstFreeBlock->xBlockSize = uxAddress - ( size_t ) pxFirstFreeBlock;
	pxFirstFreeBlock->pxNextFreeBlock = pxEnd;

	xBlockAllocatedBit = ( ( size_t ) 1 ) << ( ( sizeof( size_t ) * heapBITS_PER_BYTE ) - 1 );
}

static void prvInsertBlockIntoFreeList( BlockLink_t *pxBlockToInsert )
{
    BlockLink_t *pxIterator;
    uint8_t *puc;

	for( pxIterator = &xStart; pxIterator->pxNextFreeBlock < pxBlockToInsert; pxIterator = pxIterator->pxNextFreeBlock )
	{
	    ;
	}

	puc = ( uint8_t * ) pxIterator;
	if( ( puc + pxIterator->xBlockSize ) == ( uint8_t * ) pxBlockToInsert )
	{
		pxIterator->xBlockSize += pxBlockToInsert->xBlockSize;
		pxBlockToInsert = pxIterator;
	}
	
	puc = ( uint8_t * ) pxBlockToInsert;
	if( ( puc + pxBlockToInsert->xBlockSize ) == ( uint8_t * ) pxIterator->pxNextFreeBlock )
	{
		if( pxIterator->pxNextFreeBlock != pxEnd )
		{
			pxBlockToInsert->xBlockSize += pxIterator->pxNextFreeBlock->xBlockSize;
			pxBlockToInsert->pxNextFreeBlock = pxIterator->pxNextFreeBlock->pxNextFreeBlock;
		}
		else
		{
			pxBlockToInsert->pxNextFreeBlock = pxEnd;
		}
	}
	else
	{
		pxBlockToInsert->pxNextFreeBlock = pxIterator->pxNextFreeBlock;
	}
    
	if( pxIterator != pxBlockToInsert )
	{
		pxIterator->pxNextFreeBlock = pxBlockToInsert;
	}
	
}

void *mem_alloc( size_t xWantedSize )
{
    BlockLink_t *pxBlock, *pxPreviousBlock, *pxNewBlockLink;
    void *pvReturn = NULL;

	if( pxEnd == NULL )
	{
		prvHeapInit();
	}

	if( ( xWantedSize & xBlockAllocatedBit ) == 0 )
	{
		if( xWantedSize > 0 )
		{
			xWantedSize += xHeapStructSize;
			if( ( xWantedSize & portBYTE_ALIGNMENT_MASK ) != 0x00 )
			{
				xWantedSize += ( portBYTE_ALIGNMENT - ( xWantedSize & portBYTE_ALIGNMENT_MASK ) );
			}
		}

		if( xWantedSize > 0 )
		{
			pxPreviousBlock = &xStart;
			pxBlock = xStart.pxNextFreeBlock;
			while( ( pxBlock->xBlockSize < xWantedSize ) && ( pxBlock->pxNextFreeBlock != NULL ) )
			{
				pxPreviousBlock = pxBlock;
				pxBlock = pxBlock->pxNextFreeBlock;
			}
			if( pxBlock != pxEnd )
			{
				pvReturn = ( void * ) ( ( ( uint8_t * ) pxPreviousBlock->pxNextFreeBlock ) + xHeapStructSize );
				pxPreviousBlock->pxNextFreeBlock = pxBlock->pxNextFreeBlock;
				if( ( pxBlock->xBlockSize - xWantedSize ) > heapMINIMUM_BLOCK_SIZE )
				{
					pxNewBlockLink = ( BlockLink_t * ) ( ( ( uint8_t * ) pxBlock ) + xWantedSize );
					pxNewBlockLink->xBlockSize = pxBlock->xBlockSize - xWantedSize;
					pxBlock->xBlockSize = xWantedSize;
					prvInsertBlockIntoFreeList( pxNewBlockLink );
				}
				pxBlock->xBlockSize |= xBlockAllocatedBit;
				pxBlock->pxNextFreeBlock = NULL;
			}
		}
	}

	return pvReturn;
}

void mem_free( void *pv )
{
    uint8_t *puc = ( uint8_t * ) pv;
    BlockLink_t *pxLink;

	if( pv != NULL )
	{
		puc -= xHeapStructSize;
		pxLink = ( BlockLink_t * ) puc;

		if( ( pxLink->xBlockSize & xBlockAllocatedBit ) != 0 )
		{
			if( pxLink->pxNextFreeBlock == NULL )
			{
				pxLink->xBlockSize &= ~xBlockAllocatedBit;
				prvInsertBlockIntoFreeList( ( ( BlockLink_t * ) pxLink ) );
			}
		}
	}
}

void * memset(void * dest, int val, unsigned int len) {
	if ( dest == NULL) {
		while(1);
	}
	register unsigned char *ptr = (unsigned char*) dest;
	while (len-- > 0)
		*ptr++ = (unsigned char)val;
	return dest;
}

void bcopy(register char * src, register char * dest, int len) {
	if (dest < src)
		while (len--)
			*dest++ = *src++;
	else {
		char *lasts = src + (len - 1);
		char *lastd = dest + (len - 1);
		while (len--)
			*(char *) lastd-- = *(char *) lasts--;
	}
}

void* memcpy(void * out, const void * in, unsigned int length) {
	if ( length == 0 ) {
		return NULL;
	}

	if ( out == NULL || in == NULL  ) {
		while(1);
	}
	bcopy((char *) in, (char *) out, (int) length);
	return out;
}

unsigned int strlen(const char *str) {

	unsigned int len = 0;

	if (str != NULL) {
		while (*str++) {

			len++;

		}
	}

	return len;
}

int strcmp(const char* firstString, const char* secondString) {
	while (*firstString == *secondString) {
		if (*firstString == '\0') {
			return 0;
		}
		++firstString;
		++secondString;
	}
	if (((unsigned char) *firstString - (unsigned char) *secondString) < 0) {
		return -1;
	}
	return 1;
}

void * memmove(void * dest, const void * src, unsigned int n) {
	char * d = (char *)dest;
	char * s = (char *)src;

	while (n--)
		*d++ = *s++;

	return dest;
}

char* strcpy(char * dst0, const char * src0) {
	char *s = dst0;
	while ((*dst0++ = *src0++))
		;
	return s;
}

typedef unsigned char u8 ;
typedef signed char s8;

typedef unsigned short u16;
typedef signed short s16;

typedef int s32;
typedef unsigned int u32;

typedef long long s64;
typedef unsigned long long u64;

#include <stdarg.h>
#include <stdio.h>

//end

//http://www.cplusplus.com/reference/cstdlib/realloc/
//In case that ptr is a null pointer, the function behaves like malloc, 
//assigning a new block of size bytes and returning a pointer to its beginning.
//Otherwise, if size is zero, the memory previously allocated at ptr is deallocated 
//as if a call to free was made, and a null pointer is returned.
//If the function fails to allocate the requested block of memory, a null pointer is 
//returned, and the memory block pointed to by argument ptr is not deallocated (it is still valid, and with its contents unchanged).
void* mem_realloc (void* ptr, size_t size)
{
    void *pvReturn;
    
    if(ptr == NULL){
        return mem_alloc(size);
    }

    if(size == 0){
        mem_free(ptr);
        return NULL;
    }

    pvReturn = mem_alloc(size);
    if(pvReturn == NULL){
        return NULL;
    }else{
        memcpy(pvReturn,ptr,size);
        mem_free(ptr);
        return pvReturn;
    }
}

Token curToken;
static char curChar;
char * srcCode;
Coord coord;
lenv* ge;


static int is_white_space(char ch){
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
}

static int is_digit (char ch){
	if ((ch >= '0') && (ch <= '9')){
		return 1;
	}else{
		return 0;
	}
}

static int isalnum (char ch){
	if (((ch >= '0') && (ch <= '9')) ||
		((ch >= 'A') && (ch <= 'Z')) ||
		((ch >= 'a') && (ch <= 'z'))){
		return 1;
	}else{
		return 0;
	}
}

static int is_symbol_char(char ch){
    if(isalnum(ch)){
        return 1;
    }else if((ch == '_')||(ch == '+')||(ch == '-')||(ch == '*')
            ||(ch == '/')||(ch == '\\')||(ch == '=')||(ch == '<')
            ||(ch == '>')||(ch == '!')||(ch == '&')){
        return 1;
    }else{
        return 0;
    }
}

static char NextChar(void){
	int ch = *srcCode;
	srcCode++;
	if(ch == 0){
		return (char)EOF_CH;
	}else{
		return (char)ch;
	}
}

static void SkipWhiteSpace(void){
	while (curChar == '\t' || curChar == ' ' || curChar == '\r' || curChar == '\n' || curChar == ';'){
		switch (curChar){
		    case '\n':
			    //process the line number
                NEXT_CHAR;
			    break;
		    case ';':// comments
				NEXT_CHAR;
				while (curChar != '\n' && curChar != EOF_CH){
					NEXT_CHAR;
				}
			    break;
		    default:
			    NEXT_CHAR;
			    break;
		}
	}
}

void AppendSTR(char **str, char *tmp, int len){
    int size;
	char *p;
    int old_size;

    old_size = strlen(*str);
	size = old_size + len + 1;
    DO_ALLOC(p,size);
    if(*str){
        memcpy(p,*str,old_size);
        mem_free(*str);
    }
	memcpy(p + old_size,tmp,len);
	*str = p;
	(*str)[size - 1] = 0;
}

void get_token(void){

    if(((curToken.kind == TK_NA)||(curToken.kind == TK_SYM)||(curToken.kind == TK_STR))
        &&(curToken.value.str != NULL)){
        mem_free(curToken.value.str);
    }
    memset(&curToken,0,sizeof(Token));
    // skip white space
    SkipWhiteSpace();

    if(curChar == EOF_CH){
        curToken.kind = TK_EOF;
    }else if(is_digit(curChar)){//number
        int numVal = 0;
		curToken.kind = TK_NUM;
		do{
			numVal = numVal*10 + (curChar-'0');
			NEXT_CHAR;
		}while(is_digit(curChar));
		curToken.value.numVal = numVal;
    }else if(curChar == '-'){
        NEXT_CHAR;
        if(is_digit(curChar)){//number
            int numVal = 0;
		    curToken.kind = TK_NUM;
		    do{
			    numVal = numVal*10 + (curChar-'0');
			    NEXT_CHAR;
		    }while(is_digit(curChar));
		    curToken.value.numVal = -numVal;
        }else if(is_symbol_char(curChar)){//symbol
            int len = 0;
            char tmp[8];

            curToken.kind = TK_SYM;
            tmp[len] = '-';
            len++;
            do{				
			    tmp[len] = curChar;
                len++;
                if (len >= 8){
			        AppendSTR(&(curToken.value.str), tmp, len);
			        len = 0;
		        }
			    NEXT_CHAR;
		    }while(is_symbol_char(curChar));
            AppendSTR(&(curToken.value.str), tmp, len);
        }else{
            curToken.kind = TK_SYM;
            char tmp[1];
            tmp[0] = '-';
            AppendSTR(&(curToken.value.str), tmp, 1);
        }
    }else if(curChar == '"'){//string
        int len = 0;
        char tmp[32];
	    char *cp = tmp;
        int escape_state = 0;

        cp[len] = curChar;
		len++;
        NEXT_CHAR;
        while ((curChar != '"')||(escape_state)){
            int clear_escape_state = 0;
            
		    if (curChar == EOF_CH){
			    break;
		    }

            if(escape_state){
                clear_escape_state = 1;
            }
            
		    if((curChar == '\\')&&(escape_state == 0)){
                escape_state = 1;
		    }
		
		    cp[len] = curChar;
		    len++;
            NEXT_CHAR;
		    if (len >= 32){
			    AppendSTR(&(curToken.value.str), tmp, len);
			    len = 0;
		    }

            if(clear_escape_state){
                escape_state = 0;
            }
	    }

        if(curChar != '"'){//error
            if(curToken.value.str){
                mem_free(curToken.value.str);
                curToken.value.str = NULL;
            }
            CREATE_ERR_TOKEN("unclosed string");
        }else{
            cp[len] = curChar;
		    len++;
            AppendSTR(&(curToken.value.str), tmp, len);
            curToken.kind = TK_STR;
            NEXT_CHAR;
        }
    }else if(curChar == '('){
        curToken.kind = TK_LPAREN;
        NEXT_CHAR;
    }else if(curChar == ')'){
        curToken.kind = TK_RPAREN;
        NEXT_CHAR;
    }else if(curChar == '{'){
        curToken.kind = TK_LBRACE;
        NEXT_CHAR;
    }else if(curChar == '}'){
        curToken.kind = TK_RBRACE;
        NEXT_CHAR;
    }else if(is_symbol_char(curChar)){//symbol
        int len = 0;
        char tmp[8];

        curToken.kind = TK_SYM;
        do{				
			tmp[len] = curChar;
            len++;
            if (len >= 8){
			    AppendSTR(&(curToken.value.str), tmp, len);
			    len = 0;
		    }
			NEXT_CHAR;
		}while(is_symbol_char(curChar));
        AppendSTR(&(curToken.value.str), tmp, len);
    }else{//error
        CREATE_ERR_TOKEN("unknown char");
    }
}

void init_lexer(char *src){
	if(src){
		srcCode = src;
	}
}

char *tokenNames[] = {"","number","symbol","string","(",")","{","}","EOF"};

char *GetTokenName(TokenKind tk){
	return tokenNames[tk];
}

void SetAstNodeValue(AstNode *node,Token *token);

AstNode *Expect(TokenKind tk){
	if(curToken.kind == tk){
		NEXT_TOKEN;
        return NULL;
	}else{
	    AstNode *p;
	    if(curToken.kind == TK_NA){
            CREATE_AST_NODE(p, ERR);
            SetAstNodeValue(p,&curToken);
	    }else{
            CREATE_ERR_AST(p,"%s expected",GetTokenName(tk));
	    }
        return p;
	}
}

AstNode *ParseExpr(void);

AstNode *ParseSExpr(void){
    AstNode *sexpr = NULL;
    AstNode **tail;
    AstNode *r;
    
    r = Expect(TK_LPAREN);
    Check(sexpr,r);
    CREATE_AST_NODE(sexpr, SEXPR);
	tail = &sexpr->child;

    while (curToken.kind != TK_RPAREN){
		r = ParseExpr();
        Check(sexpr,r);
        *tail = r;
		tail = &(*tail)->brother;
	}
    r = Expect(TK_RPAREN);
    Check(sexpr,r);
    return sexpr;
}

AstNode *ParseQExpr(void){
    AstNode *qexpr = NULL;
    AstNode **tail;
    AstNode *r;
    
    r = Expect(TK_LBRACE);
    Check(qexpr,r);
    CREATE_AST_NODE(qexpr, QEXPR);
	tail = &qexpr->child;

    while (curToken.kind != TK_RBRACE){
		r = ParseExpr();
        Check(qexpr,r);
        *tail = r;
		tail = &(*tail)->brother;
	}
    r = Expect(TK_RBRACE);
    Check(qexpr,r);
    return qexpr;
}

void SetAstNodeValue(AstNode *node,Token *token){
    switch(token->kind){
        case TK_NA:      //Not Available
        case TK_SYM:     //symbol
        case TK_STR:     //string
        {
            DO_ALLOC(node->value.str,strlen(token->value.str) + 1);
            strcpy(node->value.str,token->value.str);
            break;
        }
        case TK_NUM:     //number
        {
            node->value.numVal = token->value.numVal;
            break;
        }
        default:
        {
            while(1){;}
        }
    }
}

AstNode *ParseExpr(void){
    AstNode *expr = NULL;
    
    switch(curToken.kind){
        case TK_NUM:
        {
            CREATE_AST_NODE(expr, NUM);
            SetAstNodeValue(expr,&curToken);
            NEXT_TOKEN;
            break;
        }
        case TK_SYM:
        {
            CREATE_AST_NODE(expr, SYM);
            SetAstNodeValue(expr,&curToken);
            NEXT_TOKEN;
            break;
        }
        case TK_STR:
        {
            CREATE_AST_NODE(expr, STR);
            SetAstNodeValue(expr,&curToken);
            NEXT_TOKEN;
            break;
        }
        case TK_LPAREN:
        {
            expr = ParseSExpr();
            break;
        }
        case TK_LBRACE:
        {
            expr = ParseQExpr();
            break;
        }
        case TK_NA:
        {
            CREATE_AST_NODE(expr, ERR);
            SetAstNodeValue(expr,&curToken);
            break;
        }
        default://error
        {
            CREATE_ERR_AST(expr,"number,symbol,string,( or { expected");
            break;
        }
    }

    return expr;
}

AstNode *ParseSapphire(void){
    AstNode *sapphire;
	AstNode **tail;
    AstNode *r;

    CREATE_AST_NODE(sapphire, SAP);
	tail = &sapphire->child;

    while (curToken.kind != TK_EOF){
		r = ParseExpr();
        Check(sapphire,r);
        *tail = r;
		tail = &(*tail)->brother;
	}
    r = Expect(TK_EOF);
    Check(sapphire,r);
    return sapphire;
}

void PrintAstDepth(AstNode *a, int d) {
    int i;
    AstNode *tail;
  
    for (i = 0; i < d; i++) { 
        printf("  "); 
    }

    switch(a->kind){
        case NK_SAP:     //sapphire
        {
            printf("sapphire:\n");
            break;
        }
        case NK_SEXPR:   //sexpr
        {
            printf("sexpr:\n");
            break;
        }
        case NK_QEXPR:   //qexpr
        {
            printf("qexpr:\n");
            break;
        }
        case NK_NUM:     //number
        {
            printf("number: '%d'\n", a->value.numVal);
            break;
        }
        case NK_SYM:     //symbol
        {
            printf("symbol: '%s'\n", a->value.str);
            break;
        }
        case NK_STR:     //string
        {
            printf("string: '%s'\n", a->value.str);
            break;
        }
        case NK_ERR:    //error
        {
            printf("error: '%s'\n", a->value.str);
            break;
        }
        default:
        {
            while(1){;}
        }
    }
  
    tail = a->child;
    while(tail){
        PrintAstDepth(tail, d+1);
        tail = tail->brother;
    }
}

void PrintAst(AstNode *ast) {
    PrintAstDepth(ast, 0);
}

void DeleteAst(AstNode *a) {
    AstNode *tail;
    AstNode *tmp;
  
    if (a == NULL) { 
        return; 
    }

    tail = a->child;
    while(tail){
        tmp = tail->brother;
        DeleteAst(tail);
        tail = tmp;
    }

    if(((a->kind == NK_STR)||(a->kind == NK_ERR)||(a->kind == NK_SYM))&&(a->value.str)){
        mem_free(a->value.str);
    }
    mem_free(a);
}

char* ltype_name(int t) {
  switch(t) {
    case LVAL_FUN: return "Function";
    case LVAL_NUM: return "Number";
    case LVAL_ERR: return "Error";
    case LVAL_SYM: return "Symbol";
    case LVAL_STR: return "String";
    case LVAL_SEXPR: return "S-Expression";
    case LVAL_QEXPR: return "Q-Expression";
    default: return "Unknown";
  }
}

lenv* lenv_new(void) {
    lenv* e;
    DO_ALLOC(e,1);
    e->par = NULL;
    e->relationships = NULL;
    return e;
}

void lval_del(lval* v);

void lenv_del(lenv* e){
    relationship *p,*q;

    p = e->relationships;
    while(p){
        q = p->next;
        mem_free(p->sym);
        lval_del(p->val);
        mem_free(p);
        p = q;
    }
    
    mem_free(e);
}

lval* lval_copy(lval* v);

lval* lenv_get(lenv* e, lval* k) {
    relationship* p;

    p = e->relationships;
    while(p){
        if(strcmp(p->sym, k->sym) == 0){
            return lval_copy(p->val);
        }else{
            p = p->next;
        }
    }
    
    if (e->par) {
        return lenv_get(e->par, k);
    } else {
        return lval_err("Unbound Symbol '%s'", k->sym);
    }
}

void lenv_put(lenv* e, lval* k, lval* v) {
    relationship* p = e->relationships;

    while(p){
        if(strcmp(p->sym, k->sym) == 0){
            lval_del(p->val);
            p->val = lval_copy(v);
            return;
        }else{
            p = p->next;
        }
    }
    DO_ALLOC(p,1);
    DO_ALLOC(p->sym,strlen(k->sym)+1);
    strcpy(p->sym,k->sym);
    p->val = lval_copy(v);
    p->next = e->relationships;
    e->relationships = p;
}

void lenv_def(lenv* e, lval* k, lval* v) {
    while (e->par) { 
        e = e->par; 
    }
    
    lenv_put(e, k, v);
}

lval* lval_num(long x) {
    lval* v;
    DO_ALLOC(v,1);
    v->type = LVAL_NUM;
    v->num = x;
    return v;
}

lval* lval_sym(char* s) {
    lval* v;
    DO_ALLOC(v,1);
    v->type = LVAL_SYM;
    DO_ALLOC(v->sym,strlen(s) + 1);
    strcpy(v->sym,s);
    return v;
}

lval* lval_str(char* s) {
    lval* v;
    DO_ALLOC(v,1);
    v->type = LVAL_STR;
    DO_ALLOC(v->str,strlen(s) + 1);
    strcpy(v->str,s);
    return v;
}

lval* lval_fun(lbuiltin builtin) {
    lval* v;
    DO_ALLOC(v,1);
    v->type = LVAL_FUN;
    v->builtin = builtin;
    return v;
}

lval* lval_sexpr(void) {
    lval* v;
    DO_ALLOC(v,1);
    v->type = LVAL_SEXPR;
    v->count = 0;
    v->cell = NULL;
    return v;
}

lval* lval_qexpr(void) {
    lval* v;
    DO_ALLOC(v,1);
    v->type = LVAL_QEXPR;
    v->count = 0;
    v->cell = NULL;
    return v;
}

lenv* lenv_copy(lenv* e) {
    relationship *p;
    
    lenv* n;
    DO_ALLOC(n,1);
    n->par = e->par;
    n->relationships = NULL;
    p = e->relationships;

    while(p){
        relationship *q;
        DO_ALLOC(q,1);
        DO_ALLOC(q->sym,strlen(p->sym) + 1);
        strcpy(q->sym,p->sym);
        q->val = lval_copy(p->val);
        q->next = n->relationships;
        n->relationships = q;
        p = p->next;
    }
    
    return n;
}

lval* lval_copy(lval* v) {
    lval* x;
    DO_ALLOC(x,1);
    x->type = v->type;
    switch (v->type) {
        case LVAL_FUN: 
        {
            if (v->builtin) {
                x->builtin = v->builtin;
            } else {
                x->builtin = NULL;
                x->env = lenv_copy(v->env);
                x->formals = lval_copy(v->formals);
                x->body = lval_copy(v->body);
            }
            break;
        }
        case LVAL_NUM: 
        {
            x->num = v->num; 
            break;
        }
        case LVAL_ERR:
        {
            DO_ALLOC(x->err,strlen(v->err) + 1);
            strcpy(x->err,v->err);
            break;
        }
        case LVAL_SYM:
        {
            DO_ALLOC(x->sym,strlen(v->sym) + 1);
            strcpy(x->sym,v->sym);
            break;
        }
        case LVAL_STR:
        {
            DO_ALLOC(x->str,strlen(v->str) + 1);
            strcpy(x->str,v->str);
            break;
        }
        case LVAL_SEXPR:
        case LVAL_QEXPR:
        {
            int i;
            x->count = v->count;
            DO_ALLOC(x->cell,x->count);
            for (i = 0; i < x->count; i++) {
                x->cell[i] = lval_copy(v->cell[i]);
            }
            break;
        }
    }
    return x;
}

void lval_del(lval* v) {
    switch (v->type) {
        case LVAL_NUM: 
        {
            break;
        }
        case LVAL_ERR: 
        {
            mem_free(v->err); 
            break;
        }
        case LVAL_SYM: 
        {
            mem_free(v->sym); 
            break;
        }
        case LVAL_STR: 
        {
            mem_free(v->str); 
            break;
        }
        case LVAL_FUN:
        {
            if (!v->builtin) {
                lenv_del(v->env);
                lval_del(v->formals);
                lval_del(v->body);
            }
            break;
        }
        case LVAL_SEXPR:
        case LVAL_QEXPR:
        {
            int i;
            for (i = 0; i < v->count; i++) {
                lval_del(v->cell[i]);
            }
            mem_free(v->cell);
            break;
        }
    }
    mem_free(v);
}


lval* lval_add(lval* v, lval* x) {
    v->count++;
    DO_REALLOC(v->cell,v->count);
    v->cell[v->count-1] = x;
    return v;
}

static char escape_input_c[]  = {
    '\a', '\b', '\f', '\n', '\r',
    '\t', '\v', '\\', '\'', '\"', '\0'};
    
static char *escape_output_c[] = {
    "\\a", "\\b", "\\f", "\\n", "\\r", "\\t", 
    "\\v", "\\\\", "\\'", "\\\"", "\\0", NULL};


char *unescape(char *x) {
    int i;
    int found = 0;
    char *s = x;
    char *y = x;

    while (*s) {
    
        i = 0;
        found = 0;
    
        while (escape_output_c[i]) {
            if ((*(s+0)) == escape_output_c[i][0] &&
                (*(s+1)) == escape_output_c[i][1]) {
                *y++ = escape_input_c[i]; 
                found = 1;
                s++;
                break;
            }
            i++;
        }
      
        if (!found) {
            *y++ = *s;
        }
    
        if (*s == '\0') { 
            break; 
        }else { 
            s++; 
        }
    }
    *y = '\0';
    return x;
}

char *escape(char *x) {
    int i;
    int found = 0;
    char *s = x;
    char *y;
    char *d;

    DO_ALLOC(y,2*strlen(x)+1);
    d = y;
    while (*s) {
        i = 0;
        found = 0;
        while (escape_output_c[i]) {
            if (*s == escape_input_c[i]) {
                *d++ = escape_output_c[i][0];
                *d++ = escape_output_c[i][1];
                found = 1;
                break;
            }
            i++;
        }
    
        if (!found) {
            *d++ = *s;
        }
    
        s++;
    }
    *d = '\0';
    mem_free(x);
    DO_ALLOC(x,strlen(y)+1);
    strcpy(x, y);
    mem_free(y);
  
    return x;
}

lval* lval_read_str(AstNode* t) {
    char *str;

    DO_ALLOC(str,strlen(t->value.str) - 1);
    memcpy(str,t->value.str + 1,strlen(t->value.str) - 2);
    str[strlen(t->value.str) - 2] = 0;
    str = unescape(str);
    lval* val = lval_str(str);
    return val;
}

lval* lval_read(AstNode* t) {
    lval* x = NULL;
    AstNode* p;

    switch(t->kind){
        case NK_NUM:
        {
            return lval_num(t->value.numVal);
        }
        case NK_SYM:
        {
            return lval_sym(t->value.str);
        }
        case NK_STR:
        {
            return lval_read_str(t); 
        }
        case NK_ERR:
        {
            return lval_err("%s",t->value.str);
        }
        case NK_SAP:
        case NK_SEXPR:
        {
            x = lval_sexpr();
            break;
        }
        case NK_QEXPR:
        {
            x = lval_qexpr();
            break;
        }
        default:
        {
            while(1){;}
        }
    }
    
    p = t->child;
    while(p){
        x = lval_add(x, lval_read(p));
        p = p->brother;
    }
 
    return x;
}


//http://www.cplusplus.com/reference/cstdio/sprintf/
//A terminating null character is automatically appended after the content.
int my_sprintf(char *out, char *format, ...) {
	va_list args;
    va_start(args,format);
    int escape_state = 0;
	char *cp = out;
    
    
    while(*format){
        if(escape_state){
            switch(*format){
                case '%':
                {
                    *cp++ = *format;
                    break;
                }
                case 's':
                {
                    char *s = (char *)va_arg(args,int);
                    if(s == NULL){
                        s = "(null)";
                    }
				  while(*s){
                        *cp++ = *s;
                        s++;
				  }
                    break;
                }
                case 'd':
                case 'x':
                {
                    int i = va_arg(args,int);
                    u32 u = i;
                    if(i == 0){
                        *cp++ = '0';
                    }else{
                        int neg = 0;
                        char buf[12];
                        char *p;
                        int tmp;
                        int base;
                        if(((*format) == 'd')&&(i < 0)){
                            neg = 1;
		                   u = -i;
	                    }
                        p = buf + 11;
	                    *p = '\0';
                        if((*format) == 'd'){
                            base = 10;
                        }else{
                            base = 16;
                        }
                        while(u){
                            tmp = u % base;
		                   if(tmp >= 10){
			                   tmp += 'a' - '0' - 10;
		                   }
		                   *--p = tmp + '0';
		                   u /= base;
	                    }
                        if(neg){
                            *--p = '-';
	                    }
                        while(*p){
                            *cp++ = *p;
                            p++;
                        }
                    }
                    break;
                }
                default:
                    while(1){;}
            }
            escape_state = 0;
        }else if((*format) == '%'){
            escape_state = 1;
        }else{
            *cp++ = *format;
        }
        format++;
    }

    if(escape_state){
        while(1){;}
    }
    
    va_end(args);
    *cp = 0;
    return (cp - out);
}

int lval_print(char *out,lval* v);

int lval_expr_print(char *out,lval* v, char open, char close) {
    int i;
    char *cp = out;

    *cp++ = open;
    for (i = 0; i < v->count; i++) {
        cp += lval_print(cp,v->cell[i]);
        if (i != (v->count-1)) {
            *cp++ = ' ';
        }
    } 
    *cp++ = close;
    *cp = 0;
    return (cp - out);
}

int lval_print_str(char *out,lval* v) {
    int len;
    char* str;
    DO_ALLOC(str,strlen(v->str)+1);
    strcpy(str, v->str);
    str = escape(str);
    len = my_sprintf(out,"\"%s\"",str);
    mem_free(str);
    return len;
}

int lval_print(char *out,lval* v) {
    switch (v->type) {
        case LVAL_NUM: 
        {
            return my_sprintf(out,"%d", v->num); 
        }
        case LVAL_ERR:
        {
            return my_sprintf(out,"Error: %s", v->err);
        }
        case LVAL_SYM: 
        {
            return my_sprintf(out,"%s", v->sym); 
        }
        case LVAL_STR: 
        {
            return lval_print_str(out,v); 
        }
        case LVAL_FUN: 
        {
            if (v->builtin) {
                return my_sprintf(out,"<builtin>");
            } else {
                char *cp = out;
                cp += my_sprintf(cp,"(lambda "); 
                cp += lval_print(cp,v->formals);
                *cp++ = ' ';
                cp += lval_print(cp,v->body); 
                *cp++ = ')';
                *cp = 0;
                return (cp - out);
            }
        }
        case LVAL_SEXPR: 
        {
            return lval_expr_print(out,v, '(', ')'); 
        }
        case LVAL_QEXPR: 
        {
            return lval_expr_print(out,v, '{', '}'); 
        }
        default:
        {
            while(1){;}
        }
    }
}

int lval_println(char *out,lval* v) 
{ 
    char *cp = out;
    cp += lval_print(cp,v); 
    *cp++ = '\n';
    *cp = 0;
    return (cp - out);
}

lval* lval_eval(lenv* e, lval* v);

lval* lval_pop(lval* v, int i) {
    lval* x = v->cell[i];
    memmove(&v->cell[i], &v->cell[i+1],sizeof(lval*) * (v->count-i-1));
    v->count--;
    DO_REALLOC(v->cell,v->count);
    return x;
}

lval* lval_take(lval* v, int i) {
    lval* x = lval_pop(v, i);
    lval_del(v);
    return x;
}

lval* builtin_op(lenv* e, lval* a, char* op) {
  int i;
  for (i = 0; i < a->count; i++) {
    LASSERT_TYPE(op, a, i, LVAL_NUM);
  }
  
  lval* x = lval_pop(a, 0);
  
  if ((strcmp(op, "-") == 0) && a->count == 0) {
    x->num = -x->num;
  }
  
  while (a->count > 0) {  
    lval* y = lval_pop(a, 0);
    
    if (strcmp(op, "+") == 0) { x->num += y->num; }
    if (strcmp(op, "-") == 0) { x->num -= y->num; }
    if (strcmp(op, "*") == 0) { x->num *= y->num; }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_del(x); 
        lval_del(y);
        x = lval_err("Division By Zero.");
        break;
      }
      x->num /= y->num;
    }
    
    lval_del(y);
  }
  
  lval_del(a);
  return x;
}

lval* lval_call(lenv* e, lval* f, lval* a);

lval* lval_eval_sexpr(lenv* e, lval* v) {
    int i;

    if (v->count == 0) { 
        return v; 
    }//()
    
    for (i = 0; i < v->count; i++) {
        v->cell[i] = lval_eval(e, v->cell[i]);
    }
    for (i = 0; i < v->count; i++) {
        if (v->cell[i]->type == LVAL_ERR) { 
            return lval_take(v, i); 
        }
    }
    
    if (v->count == 1) { 
        return lval_take(v, 0); 
    }
    
    lval* f = lval_pop(v, 0);
    
    if (f->type != LVAL_FUN) {
        lval* err = lval_err("S-Expression starts with incorrect type.Got %s, Expected %s.",ltype_name(f->type), ltype_name(LVAL_FUN));
        lval_del(f); 
        lval_del(v);
        return err;
    }else{
        lval* result = lval_call(e, f, v);
        lval_del(f);
        return result;
    }
}

lval* builtin_add(lenv* e, lval* a) {
    return builtin_op(e, a, "+");
}
lval* builtin_sub(lenv* e, lval* a) {
    return builtin_op(e, a, "-");
}
lval* builtin_mul(lenv* e, lval* a) {
    return builtin_op(e, a, "*");
}
lval* builtin_div(lenv* e, lval* a) {
    return builtin_op(e, a, "/");
}

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
    lval* k = lval_sym(name);
    lval* v = lval_fun(func);
    lenv_put(e, k, v);
    lval_del(k); 
    lval_del(v);
}

lval* lval_eval(lenv* e, lval* v){
    switch(v->type){
        case LVAL_SYM:
        {
            lval* x = lenv_get(e, v);
            lval_del(v);
            return x;
        }
        case LVAL_SEXPR:
        {
            return lval_eval_sexpr(e, v);
        }
        default:
        {
            return v;
        }
    }
}

lval* builtin_head(lenv* e, lval* a) {
    LASSERT_NUM("head", a, 1);
    LASSERT_TYPE("head", a, 0, LVAL_QEXPR);
    LASSERT_NOT_EMPTY("head", a, 0);
  
    lval* v = lval_take(a, 0);  
    while (v->count > 1) { 
        lval_del(lval_pop(v, 1)); 
    }
    return v;
}

lval* builtin_tail(lenv* e, lval* a) {
  LASSERT_NUM("tail", a, 1);
  LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);
  LASSERT_NOT_EMPTY("tail", a, 0);

  lval* v = lval_take(a, 0);  
  lval_del(lval_pop(v, 0));
  return v;
}

lval* builtin_list(lenv* e, lval* a) {
  a->type = LVAL_QEXPR;
  return a;
}

lval* builtin_eval(lenv* e, lval* a) {
  LASSERT_NUM("eval", a, 1);
  LASSERT_TYPE("eval", a, 0, LVAL_QEXPR);
  
  lval* x = lval_take(a, 0);
  x->type = LVAL_SEXPR;
  return lval_eval(e, x);
}

lval* lval_join(lval* x, lval* y) {
    /* For each cell in 'y' add it to 'x' */
    while (y->count) {
        x = lval_add(x, lval_pop(y, 0));
    }
    /* Delete the empty 'y' and return 'x' */
    lval_del(y);
    return x;
}

lval* builtin_join(lenv* e, lval* a) {
  int i;
  for (i = 0; i < a->count; i++) {
    LASSERT_TYPE("join", a, i, LVAL_QEXPR);
  }
  
  lval* x = lval_pop(a, 0);
  
  while (a->count) {
    lval* y = lval_pop(a, 0);
    x = lval_join(x, y);
  }
  lval_del(a);
  return x;
}

lval* builtin_var(lenv* e, lval* a, char* func) {
    int i;
    LASSERT_TYPE(func, a, 0, LVAL_QEXPR);
    lval* syms = a->cell[0];
    for (i = 0; i < syms->count; i++) {
        LASSERT(a, (syms->cell[i]->type == LVAL_SYM),"Function '%s' cannot define non-symbol. Got %s, Expected %s.", func,ltype_name(syms->cell[i]->type),ltype_name(LVAL_SYM));
    }
    LASSERT(a, (syms->count == a->count-1),"Function '%s' passed too many arguments for symbols. Got %i, Expected %i.", func, syms->count, a->count-1);
    for (i = 0; i < syms->count; i++) {
        /* If 'def' define in globally. If 'put' define in locally */
        if (strcmp(func, "define") == 0) {
            lenv_def(e, syms->cell[i], a->cell[i+1]);
        }
        if (strcmp(func, "=") == 0) {
            lenv_put(e, syms->cell[i], a->cell[i+1]);
        }
    }
    lval_del(a);
    return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* a) {
    return builtin_var(e, a, "define");
}
lval* builtin_put(lenv* e, lval* a) {
    return builtin_var(e, a, "=");
}

lval* lval_lambda(lval* formals, lval* body) {
    lval* v;
    DO_ALLOC(v,1);
    v->type = LVAL_FUN;
    /* Set Builtin to Null */
    v->builtin = NULL;
    /* Build new environment */
    v->env = lenv_new();
    /* Set Formals and Body */
    v->formals = formals;
    v->body = body;
    return v;
}

lval* builtin_lambda(lenv* e, lval* a) {
    int i;
    /* Check Two arguments, each of which are Q-Expressions */
    LASSERT_NUM("lambda", a, 2);
    LASSERT_TYPE("lambda", a, 0, LVAL_QEXPR);
    LASSERT_TYPE("lambda", a, 1, LVAL_QEXPR);
    /* Check first Q-Expression contains only Symbols */
    for (i = 0; i < a->cell[0]->count; i++) {
        LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
        "Cannot define non-symbol. Got %s, Expected %s.",ltype_name(a->cell[0]->cell[i]->type),ltype_name(LVAL_SYM));
    }
    /* Pop first two arguments and pass them to lval_lambda */
    lval* formals = lval_pop(a, 0);
    lval* body = lval_pop(a, 0);
    lval_del(a);
    return lval_lambda(formals, body);
}


lval* builtin_print(lenv* e, lval* a) {
    int i;
    /* Print each argument followed by a space */
    for (i = 0; i < a->count; i++) {
        #ifdef WIN32
        char tmp[64];
        lval_print(tmp,a->cell[i]); 
        printf("%s",tmp);
        putchar(' ');
        #else
        #endif
    }
    /* Print a newline and delete arguments */
    #ifdef WIN32
    putchar('\n');
    #else
    #endif
    lval_del(a);
    return lval_sexpr();
}

lval* builtin_error(lenv* e, lval* a) {
    LASSERT_NUM("error", a, 1);
    LASSERT_TYPE("error", a, 0, LVAL_STR);
    /* Construct Error from first argument */
    lval* err = lval_err(a->cell[0]->str);
    /* Delete arguments and return */
    lval_del(a);
    return err;
}

lval* builtin_ord(lenv* e, lval* a, char* op) {
    LASSERT_NUM(op, a, 2);
    LASSERT_TYPE(op, a, 0, LVAL_NUM);
    LASSERT_TYPE(op, a, 1, LVAL_NUM);
    int r;
    if (strcmp(op, ">") == 0) {
        r = (a->cell[0]->num > a->cell[1]->num);
    }
    if (strcmp(op, "<") == 0) {
        r = (a->cell[0]->num < a->cell[1]->num);
    }
    if (strcmp(op, ">=") == 0) {
        r = (a->cell[0]->num >= a->cell[1]->num);
    }
    if (strcmp(op, "<=") == 0) {
        r = (a->cell[0]->num <= a->cell[1]->num);
    }
    lval_del(a);
    return lval_num(r);
}

lval* builtin_gt(lenv* e, lval* a) {
    return builtin_ord(e, a, ">");
}
lval* builtin_lt(lenv* e, lval* a) {
    return builtin_ord(e, a, "<");
}
lval* builtin_ge(lenv* e, lval* a) {
    return builtin_ord(e, a, ">=");
}
lval* builtin_le(lenv* e, lval* a) {
    return builtin_ord(e, a, "<=");
}

int lval_eq(lval* x, lval* y) {
    int i;
    /* Different Types are always unequal */
    if (x->type != y->type) { return 0; }
    /* Compare Based upon type */
    switch (x->type) {
        /* Compare Number Value */
        case LVAL_NUM: return (x->num == y->num);
        /* Compare String Values */
        case LVAL_ERR: return (strcmp(x->err, y->err) == 0);
        case LVAL_SYM: return (strcmp(x->sym, y->sym) == 0);
        case LVAL_STR: return (strcmp(x->str, y->str) == 0);
        /* If builtin compare, otherwise compare formals and body */
        case LVAL_FUN:
            if (x->builtin || y->builtin) {
                return x->builtin == y->builtin;
            } else {
                return lval_eq(x->formals, y->formals)&& lval_eq(x->body, y->body);
            }
        /* If list compare every individual element */
        case LVAL_QEXPR:
        case LVAL_SEXPR:
            if (x->count != y->count) { return 0; }
            for (i = 0; i < x->count; i++) {
                /* If any element not equal then whole list not equal */
                if (!lval_eq(x->cell[i], y->cell[i])) { return 0; }
            }
            /* Otherwise lists must be equal */
            return 1;
        break;
    }
    return 0;
}

lval* builtin_cmp(lenv* e, lval* a, char* op) {
    LASSERT_NUM(op, a, 2);
    int r;
    if (strcmp(op, "==") == 0) {
        r = lval_eq(a->cell[0], a->cell[1]);
    }
    if (strcmp(op, "!=") == 0) {
        r = !lval_eq(a->cell[0], a->cell[1]);
    }
    lval_del(a);
    return lval_num(r);
}
lval* builtin_eq(lenv* e, lval* a) {
    return builtin_cmp(e, a, "==");
}
lval* builtin_ne(lenv* e, lval* a) {
    return builtin_cmp(e, a, "!=");
}

lval* builtin_if(lenv* e, lval* a) {
    LASSERT_NUM("if", a, 3);
    LASSERT_TYPE("if", a, 0, LVAL_NUM);
    LASSERT_TYPE("if", a, 1, LVAL_QEXPR);
    LASSERT_TYPE("if", a, 2, LVAL_QEXPR);
    /* Mark Both Expressions as evaluable */
    lval* x;
    a->cell[1]->type = LVAL_SEXPR;
    a->cell[2]->type = LVAL_SEXPR;
    if (a->cell[0]->num) {
        /* If condition is true evaluate first expression */
        x = lval_eval(e, lval_pop(a, 1));
    } else {
        /* Otherwise evaluate second expression */
        x = lval_eval(e, lval_pop(a, 2));
    }
    /* Delete argument list and return */
    lval_del(a);
    return x;
}

void lenv_add_builtins(lenv* e) {
    /* List Functions */
    lenv_add_builtin(e, "list", builtin_list);
    lenv_add_builtin(e, "head", builtin_head);
    lenv_add_builtin(e, "tail", builtin_tail);
    lenv_add_builtin(e, "eval", builtin_eval);
    lenv_add_builtin(e, "join", builtin_join);
    /* Mathematical Functions */
    lenv_add_builtin(e, "+", builtin_add);
    lenv_add_builtin(e, "-", builtin_sub);
    lenv_add_builtin(e, "*", builtin_mul);
    lenv_add_builtin(e, "/", builtin_div);
    /* Variable Functions */
    lenv_add_builtin(e, "define", builtin_def);
    lenv_add_builtin(e, "=", builtin_put);
    lenv_add_builtin(e, "lambda",  builtin_lambda);
    /* Comparison Functions */
    lenv_add_builtin(e, "if", builtin_if);
    lenv_add_builtin(e, "==", builtin_eq);
    lenv_add_builtin(e, "!=", builtin_ne);
    lenv_add_builtin(e, ">", builtin_gt);
    lenv_add_builtin(e, "<", builtin_lt);
    lenv_add_builtin(e, ">=", builtin_ge);
    lenv_add_builtin(e, "<=", builtin_le);
    /* String Functions */
    lenv_add_builtin(e, "error", builtin_error);
    lenv_add_builtin(e, "print", builtin_print);
}


lval* lval_call(lenv* e, lval* f, lval* a) {
    if (f->builtin) { 
        return f->builtin(e, a); 
    }
    /* Record Argument Counts */
    int given = a->count;
    int total = f->formals->count;
    /* While arguments still remain to be processed */
    while (a->count) {
        /* If we've ran out of formal arguments to bind */
        if (f->formals->count == 0) {
            lval_del(a); 
            return lval_err("Function passed too many arguments. Got %i, Expected %i.", given, total);
        }
        /* Pop the first symbol from the formals */
        lval* sym = lval_pop(f->formals, 0);
        /* Special Case to deal with '&' */
        if (strcmp(sym->sym, "&") == 0) {
            /* Ensure '&' is followed by another symbol */
            if (f->formals->count != 1) {
                lval_del(a);
                return lval_err("Function format invalid.Symbol '&' not followed by single symbol.");
            }
      
            /* Next formal should be bound to remaining arguments */
            lval* nsym = lval_pop(f->formals, 0);
            lenv_put(f->env, nsym, builtin_list(e, a));
            lval_del(sym); 
            lval_del(nsym);
            break;
        }
        /* Pop the next argument from the list */
        lval* val = lval_pop(a, 0);
        /* Bind a copy into the function's environment */
        lenv_put(f->env, sym, val);
        /* Delete symbol and value */
        lval_del(sym); 
        lval_del(val);
    }
    /* Argument list is now bound so can be cleaned up */
    lval_del(a);
    /* If '&' remains in formal list bind to empty list */
    if (f->formals->count > 0 &&strcmp(f->formals->cell[0]->sym, "&") == 0) {
        /* Check to ensure that & is not passed invalidly. */
        if (f->formals->count != 2) {
            return lval_err("Function format invalid.Symbol '&' not followed by single symbol.");
        }
    
        /* Pop and delete '&' symbol */
        lval_del(lval_pop(f->formals, 0));
    
        /* Pop next symbol and create empty list */
        lval* sym = lval_pop(f->formals, 0);
        lval* val = lval_qexpr();
    
        /* Bind to environment and delete */
        lenv_put(f->env, sym, val);
        lval_del(sym); 
        lval_del(val);
    }
    /* If all formals have been bound evaluate */
    if (f->formals->count == 0) {
        /* Set environment parent to evaluation environment */
        f->env->par = e;
        /* Evaluate and return */
        return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
    } else {
        /* Otherwise return partially evaluated function */
        return lval_copy(f);
    }
}

void sapphire_load_stdlib(void);

void InitSapphire(void){
    ge = lenv_new();
    lenv_add_builtins(ge);
    sapphire_load_stdlib();
}

lval* InterpretSapphire(char *src){
    AstNode *sapphire = NULL;
	init_lexer(src);
    NEXT_CHAR;
	NEXT_TOKEN;
	sapphire = ParseSapphire();
    PrintAst(sapphire);
    lval *x = lval_read(sapphire);
    DeleteAst(sapphire);
    return lval_eval(ge,x);
}

void sapphire_load_stdlib(void){
    lval_del(InterpretSapphire("(define {true} 1)"));
    lval_del(InterpretSapphire("(define {false} 0)"));
    /*Function Definitions*/
    lval_del(InterpretSapphire("(   define \
                                    {fun} \
                                    (lambda {f b} \
                                        { \
                                            define \
                                            (head f) \
                                            (lambda (tail f) b) \
                                        } \
                                    ) \
                                )"));
    /*Open new scope*/
    lval_del(InterpretSapphire("(   fun {let b} \
                                    { \
                                        ((lambda {_} b) ()) \
                                    } \
                                )"));
    /*Unpack List to Function*/
    lval_del(InterpretSapphire("(   fun {unpack f l} \
                                    { \
                                        eval (join (list f) l) \
                                    } \
                                )"));
    /*Unapply List to Function*/
    lval_del(InterpretSapphire("(   fun {pack f & xs} \
                                    { \
                                        f xs \
                                    } \
                                )"));
    /*First, Second, or Third Item in List*/
    lval_del(InterpretSapphire("(   fun {first l} \
                                    { \
                                        eval (head l) \
                                    } \
                                )"));
    lval_del(InterpretSapphire("(   fun {second l} \
                                    { \
                                        eval (head (tail l)) \
                                    } \
                                )"));
    lval_del(InterpretSapphire("(   fun {third l} \
                                    { \
                                        eval (head (tail (tail l))) \
                                    } \
                                )"));
    /*Nth item in List*/
    lval_del(InterpretSapphire("(   fun {nth n l} \
                                    { \
                                        if (== n 0) \
                                        { \
                                            first l \
                                        } \
                                        { \
                                            nth (- n 1) (tail l) \
                                        } \
                                    } \
                                )"));
    /*List Length*/
    lval_del(InterpretSapphire("(   fun {len l} \
                                    { \
                                        if (== l {}) \
                                        { \
                                            0 \
                                        } \
                                        { \
                                            + 1 (len (tail l)) \
                                        } \
                                    } \
                                )"));
    /*Last item in List*/
    lval_del(InterpretSapphire("(   fun {last l} \
                                    { \
                                        nth (- (len l) 1) l \
                                    } \
                                )"));
    /*Conditional Functions*/
    lval_del(InterpretSapphire("(   fun {cond & cs} \
                                    { \
                                        if  (== cs {}) \
                                            { \
                                                error \"No Selection Found\" \
                                            } \
                                            { \
                                                if  (first (first cs)) \
                                                    { \
                                                        second (first cs) \
                                                    } \
                                                    { \
                                                        unpack cond (tail cs) \
                                                    } \
                                            } \
                                    } \
                                )"));
    lval_del(InterpretSapphire("(define {else} true)"));
    /*Apply Function to List*/
    lval_del(InterpretSapphire("(   fun {map f l} \
                                    { \
                                        if (== l {}) \
                                        { \
                                            {} \
                                        } \
                                        { \
                                            join (list (f (first l))) (map f (tail l)) \
                                        } \
                                    } \
                                )"));
    /*Apply Filter to List*/
    lval_del(InterpretSapphire("(   fun {filter f l} \
                                    { \
                                        if (== l {}) \
                                        { \
                                            {} \
                                        } \
                                        { \
                                            join    (   if (f (first l)) \
                                                        { \
                                                            head l \
                                                        } \
                                                        { \
                                                            {} \
                                                        } \
                                                    ) \
                                                    (filter f (tail l)) \
                                        } \
                                    } \
                                )"));
}

char input[2048];

int main(int argc, char** argv) {
    InitSapphire();
    printf("Sapphire Version 0.1\n");
    printf("Press Ctrl+c to Exit\n");
    while (1) {
        printf("sapphire>");
        fgets(input, 2048, stdin);
        lval* x = InterpretSapphire(input);
        #ifdef WIN32
        char tmp[256];
        lval_println(tmp,x);
        printf("%s",tmp);
        #else
        #endif
        lval_del(x);
    }
    lenv_del(ge);
    return 0;
}

