#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include "lispout.h"
#include "stdlisp.c"

/************************************************************/
/*                                                          */
/*           LISP 2 C - copyright i inne takie by           */
/*          A.D.Danilecki "szopen" i K.Jurkiewicz           */
/*                                                          */
/************************************************************/

char* USRDEFN0(char* x)
{
 return car(x);
}

char* lewy(char* a0)
{
 return USRDEFN0(a0);
}


void main(void) 
{
 char *s;
 int i,z;
 char *wyn;
 int koniec=0;
 char c;
 char *arg[10];

 for (z=0;z<10;z++) arg[z]=(char*) malloc(256);
 while(!koniec)
	{

	printf("| 0 - lewy |\n");
	printf("| 1 - car |\n");
	printf("| 2 - cdr |\n");
	printf("| 3 - cons |\n");
	printf("| 4 - eq |\n");
	printf("| 5 - null |\n");
	printf("| 6 - atom |\n");
	printf("Wpisz numer funkcji\n");
	scanf("%d",&i);
	switch (i) 
		{

		case(0) :
		for (z=0;z<1;z++)
			{

			printf("Podaj parametr nr %d funkcji lewy: ",z+1);
			fflush(stdin);
			gets(arg[z]);
			}
		wyn=lewy(arg[0]);
		break;

		case(1) :
		for (z=0;z<1;z++)
			{

			printf("Podaj parametr nr %d funkcji car: ",z+1);
			fflush(stdin);
			gets(arg[z]);
			}
		wyn=car(arg[0]);
		break;

		case(2) :
		for (z=0;z<1;z++)
			{

			printf("Podaj parametr nr %d funkcji cdr: ",z+1);
			fflush(stdin);
			gets(arg[z]);
			}
		wyn=cdr(arg[0]);
		break;

		case(3) :
		for (z=0;z<2;z++)
			{

			printf("Podaj parametr nr %d funkcji cons: ",z+1);
			fflush(stdin);
			gets(arg[z]);
			}
		wyn=cons(arg[0],arg[1]);
		break;

		case(4) :
		for (z=0;z<2;z++)
			{

			printf("Podaj parametr nr %d funkcji eq: ",z+1);
			fflush(stdin);
			gets(arg[z]);
			}
		wyn=eq(arg[0],arg[1]);
		break;

		case(5) :
		for (z=0;z<1;z++)
			{

			printf("Podaj parametr nr %d funkcji null: ",z+1);
			fflush(stdin);
			gets(arg[z]);
			}
		wyn=null(arg[0]);
		break;

		case(6) :
		for (z=0;z<1;z++)
			{

			printf("Podaj parametr nr %d funkcji atom: ",z+1);
			fflush(stdin);
			gets(arg[z]);
			}
		wyn=atom(arg[0]);
		break;

		}
	printf("Wynik funkcji jest nastepujacy: %s\n",wyn);
	printf("Czy kontynuowac?jesli chcesz przerwac nacisnij male q\n");
	c=getch();
	if (c=='q') 
		koniec=1;
	while(kbhit())
		getch();
	}
for (z=0;z<10;z++) 
	{
	free(arg[z]);
	}

}
