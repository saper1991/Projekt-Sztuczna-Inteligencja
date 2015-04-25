%{
#include <stdio.h>
#include "glex.h"
#include "gparse.h"

char *stralloc(int size);
char *idalloc(char *id);

%}
%union
{
	char *str;
}
%type <str>F
%type <str>PARAM
%type <str>CIALO
%type <str>CPARAM
%type <str>CONDPARAM
%type <str>PAR1
%type <str>LISTA
%type <str>ILISTA
%type <str>ELLISTY
%type <str>QPARAM
%token <str>_id
%token <str>_car
%token _label _lambda _cons _quote _cond _atom _eq
%%
R:	R F		{ printf($2); free($2); }
 |	F  		{ printf("#include \"lisp_std.h\"\n\n");
			  printf($1); free($1); }
 ;
F:	'(' _label _id '(' _lambda '(' PARAM ')' '(' CIALO ')' ')' ')'
	{ $$ = stralloc(32+strlen($3)+strlen($7)+strlen($10));
          sprintf($$,"Element *%s(%s) {\n\treturn %s;\n}\n\n",$3,$7,$10); }
 ;
PARAM:	_id PARAM
		{ if (strlen($2) != 0) {
                	$$ = stralloc(12+strlen($1)+strlen($2));
                        sprintf($$,"Element *%s, %s",$1,$2);
                        free($1); free($2);
                  }
                  else {
                  	$$ = stralloc(10+strlen($1));
                        sprintf($$,"Element *%s",$1);
                        free($1);
		  }
                }
 |              { $$ = stralloc(1); strcpy($$,""); }
 ;
CIALO:	_car PAR1	{ $$ = stralloc(1+strlen($2)+strlen($1)*5);
			  for (int i = 0; i < strlen($1); i++)
	                    $1[i] == 'a' ? strcat($$,"car(") : strcat($$,"cdr(");
                          strcat($$,$2);
			  for (i = 0; i < strlen($1); i++) strcat($$,")");
                          free($1); free($2); }
 |	_quote QPARAM   { $$ = $2; }
 |	_cons PAR1 PAR1	{ $$ = stralloc(9+strlen($2)+strlen($3));
 			  sprintf($$,"cons(%s, %s)",$2,$3);
                          free($2); free($3); }
 |	_cond CONDPARAM { $$ = $2; }
 |	_atom PAR1      { $$ = stralloc(7+strlen($2));
 			  sprintf($$,"atom(%s)",$2);
                          free($2); }
 |	_eq PAR1 PAR1   { $$ = stralloc(7+strlen($2)+strlen($3));
 			  sprintf($$,"eq(%s, %s)",$2,$3);
                          free($2); free($3); }
 |	_id CPARAM	{ $$ = stralloc(strlen($1)+3+strlen($2));
 			  sprintf($$,"%s(%s)",$1,$2);
                          free($1); free($2); }
 ;
CPARAM: PAR1 CPARAM
		{ if (strlen($2) != 0) {
                	$$ = stralloc(3+strlen($1)+strlen($2));
                        sprintf($$,"%s, %s",$1,$2);
                        free($1); free($2);
                  }
                  else $$ = $1; }
 |              { $$ = stralloc(1); strcpy($$,""); }
 ;
CONDPARAM:	'(' PAR1 PAR1 ')' CONDPARAM
		{ $$ = stralloc(18+strlen($2)+strlen($3)+strlen($5));
                  sprintf($$,"(true(%s) ? %s \n\t: %s)",$2,$3,$5);
                  free($2); free($3); free($5); }
 |              { $$ = stralloc(13);
 		  strcpy($$,"newatom(NIL)"); }
 ;
PAR1:	'(' CIALO ')'   { $$ = $2; }
 |	_id        
 ;
QPARAM:	LISTA
 |	_id  
 ;
LISTA:	'(' ')'		{ $$ = stralloc(13);
			  strcpy($$,"newatom(NIL)"); }
 |	'(' ILISTA ')'  { $$ = $2; }
 ;
ILISTA:	ILISTA ELLISTY  { $$ = stralloc(9+strlen($1)+strlen($2));
			  sprintf($$,"cons(%s, %s)",$1,$2);
                          free($1); free($2); }
 |	ELLISTY
 ;
ELLISTY:	_id
 |		LISTA
 ;
%%

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

void main(void) {
	yy_scan scan;
	yy_parse parse;
	if (parse.yyparse(&scan) == 0);
        else exit(1);
}
