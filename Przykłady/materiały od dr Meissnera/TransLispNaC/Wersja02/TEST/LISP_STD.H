#ifndef __LISP_STD_H__
#define __LISP_STD_H__

#define NIL "()"
#define FALSE "()"
#define TRUE "#T"

enum typ { ATOM, PARA };

typedef struct dane
{
  enum typ rodzaj;
  union {
    char *atom;
    struct {
      struct dane *lewy, *prawy;
    } para;
  } d;
} Element;

Element *car(const Element *lst);
Element *cdr(const Element *lst);
Element *cons(Element *l1, Element *l2);
Element *atom(const Element *lst);
Element *eq(const Element *l1, const Element *l2);

int      true(const Element *lst);
int      false(const Element *lst);
void 	 lispFlush(void);

Element *newatom(const char *atom);
char    *listtoa(const Element *lst, char *buf);

#endif
