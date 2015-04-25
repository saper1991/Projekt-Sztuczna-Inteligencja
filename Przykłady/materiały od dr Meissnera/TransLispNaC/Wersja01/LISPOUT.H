#ifndef __LISPOUT.H
#define __LISPOUT.H

char* lewy(char*);
char* car(char*);
char* cdr(char*);
char* cons(char*,char*);
char* eq(char*,char*);
char* null(char*);
char* atom(char*);

int evalint(char*);

#endif