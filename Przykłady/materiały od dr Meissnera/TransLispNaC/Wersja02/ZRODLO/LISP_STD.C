#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lisp_std.h"

#define MAXELEM 5000

Element *ElementList[MAXELEM];
int elnum = 0;

void *checkptr(void *ptr)
{
  if (!ptr)
  {
    fprintf(stderr, "Out of memory!\n");
    exit(-1);
  }
  return ptr;
}

char *nowystr(const char *str)
{
  char *nowy = (char *)checkptr(calloc(sizeof(char), strlen(str)+1));
  strcpy(nowy, str);
  return nowy;
}

Element *nowyel(void)
{
  return (Element *)checkptr(calloc(sizeof(Element), 1));
}

Element *car(const Element *lst)
{
  return lst->d.para.lewy;
}

Element *cdr(const Element *lst)
{
  return lst->d.para.prawy;
}

Element *cons(Element *l1, Element *l2)
{
  int i = 0;
  for (i = 0; i < elnum; i++)
	if (ElementList[i]->rodzaj == PARA && 
	    ElementList[i]->d.para.lewy == l1 &&
	    ElementList[i]->d.para.prawy == l2) return ElementList[i];
  if (i == MAXELEM) {
	printf("Out of memory: internal structure full\n");
	exit(-1);
  }
  ElementList[elnum] = nowyel();
  ElementList[elnum]->rodzaj = PARA;
  ElementList[elnum]->d.para.lewy = l1;
  ElementList[elnum]->d.para.prawy = l2;
  return ElementList[elnum++];
}

Element *atom(const Element *lst)
{
  return ((lst->rodzaj == ATOM) ? newatom(TRUE) : newatom(FALSE));
}

Element *eq(const Element *l1, const Element *l2)
{
  return ((strcmp(l1->d.atom,l2->d.atom) ? newatom(FALSE) : newatom(TRUE)));
}

int true(const Element *lst)
{
  return (lst->rodzaj == PARA || strcmp(lst->d.atom, FALSE));
}

int false(const Element *lst)
{
  return !true(lst);
}

int nil(const Element *lst)
{
  return !true(lst);
}

Element *newatom(const char *atom)
{
  int i = 0;
  for (i = 0; i < elnum; i++)
	if (ElementList[i]->rodzaj == ATOM && 
	    strcmp(ElementList[i]->d.atom,atom)==0) return ElementList[i];
  if (i == MAXELEM) {
	printf("Out of memory: internal structure full\n");
	exit(-1);
  }
  ElementList[elnum] = nowyel();
  ElementList[elnum]->rodzaj = ATOM;
  ElementList[elnum]->d.atom = nowystr(atom);
  return ElementList[elnum++];
}

char *lstoa(const Element *lst, char *buf, int nawiasy)
{
  if (lst->rodzaj == PARA)
  {
    if (nawiasy)
      strcat(buf, "( ");

    lstoa(lst->d.para.lewy, buf, 1);

    if (!nil(lst->d.para.prawy))
      lstoa(lst->d.para.prawy, buf, 0);
    else
      strcat(buf, ") ");
  }
  else
  {
    strcat(buf, lst->d.atom);
    strcat(buf, " ");
  }

  return buf;
}

char *listtoa(const Element *lst, char *buf)
{
  *buf = 0;
  return lstoa(lst, buf, 1);
}

void lispFlush() {
	int i;
	for (i = 0; i < elnum; i++) {
		if (ElementList[i]->rodzaj == ATOM)
			free(ElementList[i]->d.atom);
		free(ElementList[i]);
	}
	elnum = 0;
}