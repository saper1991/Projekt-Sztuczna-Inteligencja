
#include <stdio.h>
#include "lisp_std.h"
#include "extract.c" 

extern int elnum; // zmienna pozwala nam okreœliæ, ile jest zmiennych na "stosie wewnêtrznym lispu"

void main() {
        Element *x, *y, *a;
        char xs[80],ys[80],as[80];
/* przygotowanie danych */
        x = cons(newatom("a"),newatom(NIL));
        y = cons(newatom("c"),cons(newatom("d"),newatom(NIL)));

/* wypisanie liczby zmiennych na "stosie lispu" */
        printf("Liczba zmiennych lisp przed extract: %d\n",elnum);
        a = extract(cons(cons(y,x),newatom(NIL)));
        printf("Liczba zmiennych lisp po extract: %d\n",elnum);
        printf("x: %s\ny: %s\nxr: %s\n",listtoa(x,xs),listtoa(y,ys),
                listtoa(a,as));
/* zwolnienie pamiêci zajmowanej przez zmienne lispu */
        lispFlush();
        printf("Liczba zmiennych lisp po lispFlush: %d\n",elnum);
}
