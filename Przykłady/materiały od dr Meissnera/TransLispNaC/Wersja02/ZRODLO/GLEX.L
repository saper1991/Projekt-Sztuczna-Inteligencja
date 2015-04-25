%{
#include <string.h>
#include "glex.h"
#include "gparse.h"
int LineNr = 1;
YYSTYPE yylval;
%}
%%
label		return _label;
lambda		return _lambda;
c[ad]+r		{ yylval.str = (char *)calloc((strlen(yytext)-1),sizeof(char));
		  strncpy(yylval.str,yytext+1,strlen(yytext)-2);
		  return _car; }
quote		return _quote;
cons		return _cons;
cond		return _cond;
atom		return _atom;
eq		return _eq;
[a-zA-Z][a-zA-Z0-9]* { yylval.str = (char *)malloc((strlen(yytext)+1)*sizeof(char));
		       strcpy(yylval.str,yytext);
		       return _id; }
"("		return '(';
")"		return ')';
" "|\t		;
\n		LineNr++;
.		{ printf("%d: Lexical error\n",LineNr); exit(1); }
%%
