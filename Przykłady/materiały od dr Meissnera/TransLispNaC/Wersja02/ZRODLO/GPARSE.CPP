/* c:\lex\lex&yacc\bin\yacc -LC -o gparse.cpp -D gparse.h gparse.y */
#ifdef YYTRACE
#define YYDEBUG 1
#else
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#endif
#line 1 "gparse.y"

#include <stdio.h>
#include "glex.h"
#include "gparse.h"

char *stralloc(int size);
char *idalloc(char *id);

extern YYSTYPE yylval;
#if YYDEBUG
enum YY_Types { YY_t_NoneDefined, YY_t_str
};
#endif
#if YYDEBUG
yyTypedRules yyRules[] = {
	{ "&00: %11 &00",  0},
	{ "%11: %11 %01",  0},
	{ "%11: %01",  0},
	{ "%01: &11 &04 &02 &11 &05 &11 %02 &12 &11 %03 &12 &12 &12",  1},
	{ "%02: &02 %02",  1},
	{ "%02:",  1},
	{ "%03: &03 %06",  1},
	{ "%03: &07 %10",  1},
	{ "%03: &06 %06 %06",  1},
	{ "%03: &08 %05",  1},
	{ "%03: &09 %06",  1},
	{ "%03: &10 %06 %06",  1},
	{ "%03: &02 %04",  1},
	{ "%04: %06 %04",  1},
	{ "%04:",  1},
	{ "%05: &11 %06 %06 &12 %05",  1},
	{ "%05:",  1},
	{ "%06: &11 %03 &12",  1},
	{ "%06: &02",  1},
	{ "%10: %07",  1},
	{ "%10: &02",  1},
	{ "%07: &11 &12",  1},
	{ "%07: &11 %08 &12",  1},
	{ "%08: %08 %09",  1},
	{ "%08: %09",  1},
	{ "%09: &02",  1},
	{ "%09: %07",  1},
{ "$accept",  0},{ "error",  0}
};
yyNamedType yyTokenTypes[] = {
	{ "$end",  0,  0},
	{ "error",  256,  0},
	{ "_id",  257,  1},
	{ "_car",  258,  1},
	{ "_label",  259,  0},
	{ "_lambda",  260,  0},
	{ "_cons",  261,  0},
	{ "_quote",  262,  0},
	{ "_cond",  263,  0},
	{ "_atom",  264,  0},
	{ "_eq",  265,  0},
	{ "'('",  40,  0},
	{ "')'",  41,  0}

};
#endif
static short yydef[] = {

	  -1,   -5,   -9,  -13,  -17,  -21,  -25
};
static short yyex[] = {

	   0,    0,   -1,    1,   41,    3,   -1,    1,   41,    3, 
	  -1,    1,   41,    4,   -1,    1,   41,    5,   -1,    1, 
	  41,    4,   -1,    1,   41,    5,   -1,    1
};
static short yyact[] = {

	 -32,   40,  -31,  259,  -30,  257,  -29,   40,  -28,  260, 
	  -2,   40,   -3,  257,  -26,   41,  -25,   40,   -4,  -20, 
	 -22,  -21,   -5,  -23,  -24,  265,  264,  263,  262,  261, 
	 258,  257,  -18,  -39,  257,   40,  -16,   40,  -14,  -38, 
	 257,   40,  -13,   41,  -14,  -37,  -34,  257,   41,   40, 
	  -9,   41,  -40,   41,  -14,  -36,  -34,  257,   41,   40, 
	 -51,   41,   -7,   41,   -1
};
static short yypact[] = {

	   1,   13,   13,   34,   37,   34,   37,   63,   61,   57, 
	  34,   53,   51,   47,   34,   34,   34,   25,   43,   34, 
	  40,   34,   34,   34,   25,   17,   15,   11,    9,    7, 
	   5,    3,    1
};
static short yygo[] = {

	 -53,  -52,    0,  -50,  -27,    2,  -12,  -19,   17,  -42, 
	 -43,    5,  -41,  -46,    6,   -8,  -47,  -11,  -44,  -49, 
	 -15,  -45,  -17,   -6,   23,   22,   21,   19,   16,   15, 
	  14,   10,  -54,  -56,   20,  -10,  -35,  -55,    9,  -48, 
	  -1,   -1
};
static short yypgo[] = {

	   0,    0,    0,    4,   10,   13,   37,   35,   33,   33, 
	  39,   23,   23,   13,   10,    7,    7,    7,    7,    7, 
	   7,    7,    4,    1,   40,   40,   39,   35,   37,    0
};
static short yyrlen[] = {

	   0,    0,    0,    0,    0,    0,    1,    2,    3,    2, 
	   1,    1,    3,    5,    2,    2,    3,    2,    2,    3, 
	   2,    2,    2,   13,    1,    2,    1,    1,    1,    2
};
#define YYS0	32
#define YYDELTA	27
#define YYNPACT	33
#define YYNDEF	7

#define YYr27	0
#define YYr28	1
#define YYr29	2
#define YYr5	3
#define YYr14	4
#define YYr16	5
#define YYr25	6
#define YYr23	7
#define YYr22	8
#define YYr21	9
#define YYr20	10
#define YYr18	11
#define YYr17	12
#define YYr15	13
#define YYr13	14
#define YYr12	15
#define YYr11	16
#define YYr10	17
#define YYr9	18
#define YYr8	19
#define YYr7	20
#define YYr6	21
#define YYr4	22
#define YYr3	23
#define YYr2	24
#define YYr1	25
#define YYrACCEPT	YYr27
#define YYrERROR	YYr28
#define YYrLR2	YYr29
#if YYDEBUG
char * yysvar[] = {
	"$accept",
	"F",
	"PARAM",
	"CIALO",
	"CPARAM",
	"CONDPARAM",
	"PAR1",
	"LISTA",
	"ILISTA",
	"ELLISTY",
	"QPARAM",
	"R",
	0
};
short yyrmap[] = {

	  27,   28,   29,    5,   14,   16,   25,   23,   22,   21, 
	  20,   18,   17,   15,   13,   12,   11,   10,    9,    8, 
	   7,    6,    4,    3,    2,    1,   19,   24,   26,    0
};
short yysmap[] = {

	   3,    9,   10,   15,   18,   25,   54,   50,   48,   46, 
	  41,   38,   37,   32,   31,   29,   27,   24,   22,   21, 
	  20,   19,   17,   16,   14,   13,   11,    8,    7,    6, 
	   4,    1,    0,   44,   51,   52,   47,   33,   23,   49, 
	  55,   39,   26,   40,   28,   30,   42,   35,   36,   12, 
	  53,    2,    5,   34,   45,   43
};
int yy_parse::yyntoken = 13;
int yy_parse::yynvar = 12;
int yy_parse::yynstate = 56;
int yy_parse::yynrule = 30;
#endif



#line 2 "c:\lex\lex&yacc/etc/yyparse.cpp"
// C++ YACC parser code
// Copyright 1991 by Mortice Kern Systems Inc.  All rights reserved.
//
// If YYDEBUG is defined as 1 and yy_parse::yydebug is set to 1,
// yyparse() will print a travelogue of its actions as it reads
// and parses input.
//
// YYSYNC can be defined to cause yyparse() to attempt to always
// hold a lookahead token

const YY_MIN_STATE_NUM = 20;	// not useful to be too small!

#if YYDEBUG
#ifdef YYTRACE
long	* yy_parse::States	= yyStates;
#endif
yyTypedRules * yy_parse::Rules	= yyRules;
yyNamedType * yy_parse::TokenTypes = yyTokenTypes;

#define YY_TRACE(fn) { done = 0; fn(); if (done) YYRETURN(-1); }
#endif

// Constructor for yy_parse: user-provided tables
yy_parse::yy_parse(int sz, short * states, YYSTYPE * stack)
{
	mustfree = 0;
	if ((size = sz) < YY_MIN_STATE_NUM
	 || (stateStack = states) == (short *) 0
	 || (valueStack = stack) == (YYSTYPE *) 0) {
		fprintf(stderr,"Bad state/stack given");
		exit(1);
	}
	reset = 1;		// force reset
#if YYDEBUG
	yydebug = 0;
	typeStack = new short[size+1];
	if (typeStack == (short *) 0) {
		fprintf(stderr,"Cannot allocate typeStack");
		exit(1);
	}
#endif
}
// Constructor for yy_parse: allocate tables with new
yy_parse::yy_parse(int sz)
{
	size = sz;
	reset = 1;		// force reset
	mustfree = 1;		// delete space in deconstructor
#if YYDEBUG
	yydebug = 0;
	typeStack = new short[size+1];
#endif
	stateStack = new short[size+1];
	valueStack = new YYSTYPE[size+1];

	if (stateStack == (short *) 0 || valueStack == (YYSTYPE *) 0
#if YYDEBUG
		|| typeStack == (short *) 0
#endif
	    ) {
		fprintf(stderr,"Not enough space for parser stacks");
		exit(1);
	}
}
// Destructor for class yy_parse
//	Free up space
yy_parse::~yy_parse()
{
	if (mustfree) {
		delete stateStack;
		delete valueStack;
	}
	stateStack = (short *) 0;
#if YYDEBUG
	delete typeStack;
#endif
}

#ifdef YACC_WINDOWS

// The initial portion of the yacc parser.
// In an windows environment, it will load the desired
// resources, obtain pointers to them, and then call
// the protected member win_yyparse() to acutally begin the
// parsing. When complete, win_yyparse() will return a
// value back to our new yyparse() function, which will 
// record that value temporarily, release the resources
// from global memory, and finally return the value
// back to the caller of yyparse().

int
yy_parse::yyparse(yy_scan* ps)
{
	int wReturnValue;
	HANDLE hRes_table;
	short *old_yydef;		// the following are used for saving
	short *old_yyex;		// the current pointers
	short *old_yyact;
	short *old_yypact;
	short *old_yygo;
	short *old_yypgo;
	short *old_yyrlen;

	// the following code will load the required
	// resources for a Windows based parser.

	hRes_table = LoadResource (hInst,
		FindResource (hInst, "UD_RES_yyYACC", "yyYACCTBL"));
	
	// return an error code if any
	// of the resources did not load

	if (hRes_table == (HANDLE)NULL)
		return (1);
	
	// the following code will lock the resources
	// into fixed memory locations for the parser
	// (also, save away the old pointer values)

	old_yydef = yydef;
	old_yyex = yyex;
	old_yyact = yyact;
	old_yypact = yypact;
	old_yygo = yygo;
	old_yypgo = yypgo;
	old_yyrlen = yyrlen;

	yydef = (short *)LockResource (hRes_table);
	yyex = (short *)(yydef + Sizeof_yydef);
	yyact = (short *)(yyex + Sizeof_yyex);
	yypact = (short *)(yyact + Sizeof_yyact);
	yygo = (short *)(yypact + Sizeof_yypact);
	yypgo = (short *)(yygo + Sizeof_yygo);
	yyrlen = (short *)(yypgo + Sizeof_yypgo);

	// call the official yyparse() function

	wReturnValue = win_yyparse (ps);

	// unlock the resources

	UnlockResource (hRes_table);

	// and now free the resource

	FreeResource (hRes_table);

	//
	// restore previous pointer values
	//

	yydef = old_yydef;
	yyex = old_yyex;
	yyact = old_yyact;
	yypact = old_yypact;
	yygo = old_yygo;
	yypgo = old_yypgo;
	yyrlen = old_yyrlen;

	return (wReturnValue);
}	// end yyparse()


// The parser proper.
//	Note that this code is reentrant; you can return a value
//	and then resume parsing by recalling yyparse().
//	Call yyreset() before yyparse() if you want a fresh start

int
yy_parse::win_yyparse(yy_scan* ps)

#else /* YACC_WINDOWS */

// The parser proper.
//	Note that this code is reentrant; you can return a value
//	and then resume parsing by recalling yyparse().
//	Call yyreset() before yyparse() if you want a fresh start
int
yy_parse::yyparse(yy_scan* ps)

#endif /* YACC_WINDOWS */

{
	short	* yyp, * yyq;		// table lookup
	int	yyj;
#if YYDEBUG
	int	yyruletype = 0;
#endif

	if ((scan = ps) == (yy_scan *) 0) {	// scanner
		fprintf(stderr,"No scanner");
		exit(1);
	}

	if (reset) {			// start new parse
		yynerrs = 0;
		yyerrflag = 0;
		yyps = stateStack;
		yypv = valueStack;
#if YYDEBUG
		yytp = typeStack;
#endif
		yystate = YYS0;
		yyclearin();
		reset = 0;
	} else			// continue saved parse
		goto yyNext;			// after action

yyStack:
	if (++yyps > &stateStack[size]) {
		scan->yyerror("Parser stack overflow");
		YYABORT;
	}
	*yyps = yystate;	/* stack current state */
	*++yypv = yyval;	/* ... and value */
#if YYDEBUG
	if (yydebug) {
		*++yytp = yyruletype;	/* ... and type */
		YY_TRACE(yyShowState)
	}
#endif

	/*
	 * Look up next action in action table.
	 */
yyEncore:
#ifdef YYSYNC
	if (yychar < 0) {
		if ((yychar = scan->yylex()) < 0) {
			if (yychar == -2) YYABORT;
			yychar = 0;
		}	/* endif */
		yylval = ::yylval;
#if YYDEBUG
		if (yydebug)
			yyShowRead();	// show new input token
#endif
	}
#endif
#ifdef YACC_WINDOWS
	if (yystate >= Sizeof_yypact) 	/* simple state */
#else /* YACC_WINDOWS */
	if (yystate >= sizeof yypact/sizeof yypact[0]) 	/* simple state */
#endif /* YACC_WINDOWS */
		yyi = yystate - YYDELTA;	/* reduce in any case */
	else {
		if(*(yyp = &yyact[yypact[yystate]]) >= 0) {
			/* Look for a shift on yychar */
#ifndef YYSYNC
			if (yychar < 0) {
				if ((yychar = scan->yylex()) < 0) {
					if (yychar == -2) YYABORT;
					yychar = 0;
				}	/* endif */
				yylval = ::yylval;
#if YYDEBUG
				if (yydebug)
					yyShowRead();	// show new input token
#endif
			}
#endif
			yyq = yyp;
			yyi = yychar;
			while (yyi < *yyp++)
				;
			if (yyi == yyp[-1]) {
				yystate = ~yyq[yyq-yyp];
#if YYDEBUG
				if (yydebug) {
					yyruletype = yyGetType(yychar);
					YY_TRACE(yyShowShift)
				}
#endif
				yyval = yylval;		/* stack value */
				yyclearin();		/* clear token */
				if (yyerrflag)
					yyerrflag--;	/* successful shift */
				goto yyStack;
			}
		}

		/*
	 	 *	Fell through - take default action
	 	 */

#ifdef YACC_WINDOWS
		if (yystate >= Sizeof_yydef) 	/* simple state */
#else /* YACC_WINDOWS */
		if (yystate >= sizeof yydef /sizeof yydef[0])
#endif /* YACC_WINDOWS */
			goto yyError;
		if ((yyi = yydef[yystate]) < 0)	 { /* default == reduce? */

			/* Search exception table */
			yyp = &yyex[~yyi];
#ifndef YYSYNC
			if (yychar < 0) {
				if ((yychar = scan->yylex()) < 0) {
					if (yychar == -2) YYABORT;
					yychar = 0;
				}	/* endif */
				yylval = ::yylval;
#if YYDEBUG
				if (yydebug)
					yyShowRead();	// show new input token
#endif
			}
#endif
			while((yyi = *yyp) >= 0 && yyi != yychar)
				yyp += 2;
			yyi = yyp[1];
		}
	}

	yyj = yyrlen[yyi];

#if YYDEBUG
	if (yydebug) {
		npop = yyj; rule = yyi;
		YY_TRACE(yyShowReduce)
		yytp -= yyj;
	}
#endif
	yyps -= yyj;		/* pop stacks */
	yypvt = yypv;		/* save top */
	yypv -= yyj;
	yyval = yypv[1];	/* default action $ = $1 */
#if YYDEBUG
	if (yydebug)
		yyruletype = yyRules[yyrmap[yyi]].type;
#endif
	switch (yyi) {		/* perform semantic action */
		
case YYr1: {	/* R :  R F */
#line 28 "gparse.y"
 printf(yypvt[0].str); free(yypvt[0].str); 
} break;

case YYr2: {	/* R :  F */
#line 29 "gparse.y"
 printf("#include \"lisp_std.h\"\n\n");
			  printf(yypvt[0].str); free(yypvt[0].str); 
} break;

case YYr3: {	/* F :  '(' _label _id '(' _lambda '(' PARAM ')' '(' CIALO ')' ')' ')' */
#line 33 "gparse.y"
 yyval.str = stralloc(32+strlen(yypvt[-10].str)+strlen(yypvt[-6].str)+strlen(yypvt[-3].str));
          sprintf(yyval.str,"Element *%s(%s) {\n\treturn %s;\n}\n\n",yypvt[-10].str,yypvt[-6].str,yypvt[-3].str); 
} break;

case YYr4: {	/* PARAM :  _id PARAM */
#line 37 "gparse.y"
 if (strlen(yypvt[0].str) != 0) {
                	yyval.str = stralloc(12+strlen(yypvt[-1].str)+strlen(yypvt[0].str));
                        sprintf(yyval.str,"Element *%s, %s",yypvt[-1].str,yypvt[0].str);
                        free(yypvt[-1].str); free(yypvt[0].str);
                  }
                  else {
                  	yyval.str = stralloc(10+strlen(yypvt[-1].str));
                        sprintf(yyval.str,"Element *%s",yypvt[-1].str);
                        free(yypvt[-1].str);
		  }
                
} break;

case YYr5: {	/* PARAM :  */
#line 48 "gparse.y"
 yyval.str = stralloc(1); strcpy(yyval.str,""); 
} break;

case YYr6: {	/* CIALO :  _car PAR1 */
#line 50 "gparse.y"
 yyval.str = stralloc(1+strlen(yypvt[0].str)+strlen(yypvt[-1].str)*5);
			  for (int i = 0; i < strlen(yypvt[-1].str); i++)
	                    yypvt[-1].str[i] == 'a' ? strcat(yyval.str,"car(") : strcat(yyval.str,"cdr(");
                          strcat(yyval.str,yypvt[0].str);
			  for (i = 0; i < strlen(yypvt[-1].str); i++) strcat(yyval.str,")");
                          free(yypvt[-1].str); free(yypvt[0].str); 
} break;

case YYr7: {	/* CIALO :  _quote QPARAM */
#line 56 "gparse.y"
 yyval.str = yypvt[0].str; 
} break;

case YYr8: {	/* CIALO :  _cons PAR1 PAR1 */
#line 57 "gparse.y"
 yyval.str = stralloc(9+strlen(yypvt[-1].str)+strlen(yypvt[0].str));
 			  sprintf(yyval.str,"cons(%s, %s)",yypvt[-1].str,yypvt[0].str);
                          free(yypvt[-1].str); free(yypvt[0].str); 
} break;

case YYr9: {	/* CIALO :  _cond CONDPARAM */
#line 60 "gparse.y"
 yyval.str = yypvt[0].str; 
} break;

case YYr10: {	/* CIALO :  _atom PAR1 */
#line 61 "gparse.y"
 yyval.str = stralloc(7+strlen(yypvt[0].str));
 			  sprintf(yyval.str,"atom(%s)",yypvt[0].str);
                          free(yypvt[0].str); 
} break;

case YYr11: {	/* CIALO :  _eq PAR1 PAR1 */
#line 64 "gparse.y"
 yyval.str = stralloc(7+strlen(yypvt[-1].str)+strlen(yypvt[0].str));
 			  sprintf(yyval.str,"eq(%s, %s)",yypvt[-1].str,yypvt[0].str);
                          free(yypvt[-1].str); free(yypvt[0].str); 
} break;

case YYr12: {	/* CIALO :  _id CPARAM */
#line 67 "gparse.y"
 yyval.str = stralloc(strlen(yypvt[-1].str)+3+strlen(yypvt[0].str));
 			  sprintf(yyval.str,"%s(%s)",yypvt[-1].str,yypvt[0].str);
                          free(yypvt[-1].str); free(yypvt[0].str); 
} break;

case YYr13: {	/* CPARAM :  PAR1 CPARAM */
#line 72 "gparse.y"
 if (strlen(yypvt[0].str) != 0) {
                	yyval.str = stralloc(3+strlen(yypvt[-1].str)+strlen(yypvt[0].str));
                        sprintf(yyval.str,"%s, %s",yypvt[-1].str,yypvt[0].str);
                        free(yypvt[-1].str); free(yypvt[0].str);
                  }
                  else yyval.str = yypvt[-1].str; 
} break;

case YYr14: {	/* CPARAM :  */
#line 78 "gparse.y"
 yyval.str = stralloc(1); strcpy(yyval.str,""); 
} break;

case YYr15: {	/* CONDPARAM :  '(' PAR1 PAR1 ')' CONDPARAM */
#line 81 "gparse.y"
 yyval.str = stralloc(18+strlen(yypvt[-3].str)+strlen(yypvt[-2].str)+strlen(yypvt[0].str));
                  sprintf(yyval.str,"(true(%s) ? %s \n\t: %s)",yypvt[-3].str,yypvt[-2].str,yypvt[0].str);
                  free(yypvt[-3].str); free(yypvt[-2].str); free(yypvt[0].str); 
} break;

case YYr16: {	/* CONDPARAM :  */
#line 84 "gparse.y"
 yyval.str = stralloc(13);
 		  strcpy(yyval.str,"newatom(NIL)"); 
} break;

case YYr17: {	/* PAR1 :  '(' CIALO ')' */
#line 87 "gparse.y"
 yyval.str = yypvt[-1].str; 
} break;

case YYr18: {	/* PAR1 :  _id */
#line 88 "gparse.y"
  
} break;

case YYr20: {	/* QPARAM :  _id */
#line 91 "gparse.y"
 yyval.str = idalloc(yypvt[0].str); 
} break;

case YYr21: {	/* LISTA :  '(' ')' */
#line 93 "gparse.y"
 yyval.str = stralloc(13);
			  strcpy(yyval.str,"newatom(NIL)"); 
} break;

case YYr22: {	/* LISTA :  '(' ILISTA ')' */
#line 95 "gparse.y"
 yyval.str = yypvt[-1].str; 
} break;

case YYr23: {	/* ILISTA :  ILISTA ELLISTY */
#line 97 "gparse.y"
 yyval.str = stralloc(9+strlen(yypvt[-1].str)+strlen(yypvt[0].str));
			  sprintf(yyval.str,"cons(%s, %s)",yypvt[-1].str,yypvt[0].str);
                          free(yypvt[-1].str); free(yypvt[0].str); 
} break;

case YYr25: {	/* ELLISTY :  _id */
#line 102 "gparse.y"
 yyval.str = idalloc(yypvt[0].str); 
} break;
#line 343 "c:\lex\lex&yacc/etc/yyparse.cpp"
	case YYrACCEPT:
		YYACCEPT;
	case YYrERROR:
		goto yyError;
	}
yyNext:
	/*
	 *	Look up next state in goto table.
	 */

	yyp = &yygo[yypgo[yyi]];
	yyq = yyp++;
	yyi = *yyps;
	while (yyi < *yyp++)		/* busy little loop */
		;
	yystate = ~(yyi == *--yyp? yyq[yyq-yyp]: *yyq);
#if YYDEBUG
	if (yydebug)
		YY_TRACE(yyShowGoto)
#endif
	goto yyStack;

yyerrlabel:	;		/* come here from YYERROR	*/
	yyerrflag = 1;
	if (yyi == YYrERROR) {
		yyps--, yypv--;
#if YYDEBUG
		if (yydebug) yytp--;
#endif
	}
	
yyError:
	switch (yyerrflag) {

	case 0:		/* new error */
		yynerrs++;
		yyi = yychar;
		scan->yyerror("Syntax error");
		if (yyi != yychar) {
			/* user has changed the current token */
			/* try again */
			yyerrflag++;	/* avoid loops */
			goto yyEncore;
		}

	case 1:		/* partially recovered */
	case 2:
		yyerrflag = 3;	/* need 3 valid shifts to recover */
			
		/*
		 *	Pop states, looking for a
		 *	shift on `error'.
		 */

		for ( ; yyps > stateStack; yyps--, yypv--
#if YYDEBUG
					, yytp--
#endif
		) {
#ifdef YACC_WINDOWS
			if (*yyps >= Sizeof_yypact) 	/* simple state */
#else /* YACC_WINDOWS */
			if (*yyps >= sizeof yypact/sizeof yypact[0])
#endif /* YACC_WINDOWS */
				continue;
			yyp = &yyact[yypact[*yyps]];
			yyq = yyp;
			do
				;
			while (YYERRCODE < *yyp++);
			if (YYERRCODE == yyp[-1]) {
				yystate = ~yyq[yyq-yyp];
				goto yyStack;
			}
				
			/* no shift in this state */
#if YYDEBUG
			if (yydebug && yyps > stateStack+1)
				YY_TRACE(yyShowErrRecovery)
#endif
			/* pop stacks; try again */
		}
		/* no shift on error - abort */
		break;

	case 3:
		/*
		 *	Erroneous token after
		 *	an error - discard it.
		 */

		if (yychar == 0)  /* but not EOF */
			break;
#if YYDEBUG
		if (yydebug)
			YY_TRACE(yyShowErrDiscard)
#endif
		yyclearin();
		goto yyEncore;	/* try again in same state */
	}
	YYABORT;

}
#if YYDEBUG
/*
 * Return type of token
 */
int
yy_parse::yyGetType(int tok)
{
	yyNamedType * tp;
	for (tp = &yyTokenTypes[yyntoken-1]; tp > yyTokenTypes; tp--)
		if (tp->token == tok)
			return tp->type;
	return 0;
}

	
// Print a token legibly.
char *
yy_parse::yyptok(int tok)
{
	yyNamedType * tp;
	for (tp = &yyTokenTypes[yyntoken-1]; tp > yyTokenTypes; tp--)
		if (tp->token == tok)
			return tp->name;
	return "";
}
/*
 * Read state 'num' from YYStatesFile
 */
#ifdef YYTRACE

char *
yy_parse::yygetState(int num)
{
	int	size;
	char	*cp;
	static FILE *yyStatesFile = (FILE *) 0;
	static char yyReadBuf[YYMAX_READ+1];

	if (yyStatesFile == (FILE *) 0
	 && (yyStatesFile = fopen(YYStatesFile, "r")) == (FILE *) 0)
		return "yyExpandName: cannot open states file";

	if (num < yynstate - 1)
		size = (int)(States[num+1] - States[num]);
	else {
		/* length of last item is length of file - ptr(last-1) */
		if (fseek(yyStatesFile, 0L, 2) < 0)
			goto cannot_seek;
		size = (int) (ftell(yyStatesFile) - States[num]);
	}
	if (size < 0 || size > YYMAX_READ)
		return "yyExpandName: bad read size";
	if (fseek(yyStatesFile, States[num], 0) < 0) {
	cannot_seek:
		return "yyExpandName: cannot seek in states file";
	}

	(void) fread(yyReadBuf, 1, size, yyStatesFile);
	yyReadBuf[size] = '\0';
	return yyReadBuf;
}
#endif /* YYTRACE */
/*
 * Expand encoded string into printable representation
 * Used to decode yyStates and yyRules strings.
 * If the expansion of 's' fits in 'buf', return 1; otherwise, 0.
 */
int
yy_parse::yyExpandName(int num, int isrule, char * buf, int len)
{
	int	i, n, cnt, type;
	char	* endp, * cp, * s;

	if (isrule)
		s = yyRules[num].name;
	else
#ifdef YYTRACE
		s = yygetState(num);
#else
		s = "*no states*";
#endif

	for (endp = buf + len - 8; *s; s++) {
		if (buf >= endp) {		/* too large: return 0 */
		full:	(void) strcpy(buf, " ...\n");
			return 0;
		} else if (*s == '%') {		/* nonterminal */
			type = 0;
			cnt = yynvar;
			goto getN;
		} else if (*s == '&') {		/* terminal */
			type = 1;
			cnt = yyntoken;
		getN:
			if (cnt < 100)
				i = 2;
			else if (cnt < 1000)
				i = 3;
			else
				i = 4;
			for (n = 0; i-- > 0; )
				n = (n * 10) + *++s - '0';
			if (type == 0) {
				if (n >= yynvar)
					goto too_big;
				cp = yysvar[n];
			} else if (n >= yyntoken) {
			    too_big:
				cp = "<range err>";
			} else
				cp = yyTokenTypes[n].name;

			if ((i = strlen(cp)) + buf > endp)
				goto full;
			(void) strcpy(buf, cp);
			buf += i;
		} else
			*buf++ = *s;
	}
	*buf = '\0';
	return 1;
}
#ifndef YYTRACE
/*
 * Show current state of yyparse
 */
void
yy_parse::yyShowState()
{
	(void) printf("state %d (%d), char %s (%d)\n%d stateStack entries\n",
		yysmap[yystate],yystate,yyptok(yychar),yychar,
		yypv - valueStack);
}
// show results of reduction: yyi is rule number
void
yy_parse::yyShowReduce()
{
	(void) printf("Reduce by rule %d (pop#=%d)\n", yyrmap[rule], npop);
}
// show read token
void
yy_parse::yyShowRead()
{
	(void) printf("read %s (%d)\n", yyptok(yychar), yychar);
}
// show Goto
void
yy_parse::yyShowGoto()
{
	(void) printf("goto %d (%d)\n", yysmap[yystate], yystate);
}
// show Shift
void
yy_parse::yyShowShift()
{
	(void) printf("shift %d (%d)\n", yysmap[yystate], yystate);
}
// show error recovery
void
yy_parse::yyShowErrRecovery()
{
	(void) printf("Error recovery pops state %d (%d), uncovers %d (%d)\n",
		yysmap[*(yyps-1)], *(yyps-1), yysmap[yystate], yystate);
}
// show token discards in error processing
void
yy_parse::yyShowErrDiscard()
{
	(void) printf("Error recovery discards %s (%d), ",
		yyptok(yychar), yychar);
}
#endif	/* ! YYTRACE */
#endif	/* YYDEBUG */
#line 107 "gparse.y"
char *stralloc(int size) {
	char *a = (char *)calloc(size,sizeof(char));
        if (a == 0) {
        	fprintf(stderr,"Out of memory\n");
                exit(-1);
        }
	return a;
}

char *idalloc(char *id) {
	char *nid;
	nid = stralloc(12+strlen(id));
	strcpy(nid,"newatom(\"");
        strcat(strcat(nid,id),"\")");
	free(id);

        return nid;
}

void main(int argc, char *argv[]) {
        int q;
	yy_scan scan;
	yy_parse parse;
	if (parse.yyparse(&scan) == 0);
		// perform();
        else exit(1);
}

