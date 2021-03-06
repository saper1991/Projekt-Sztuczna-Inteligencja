﻿start :-
	%readFromFile("Module1.vb", R),
	readFromFile("test.txt", R),
	print('\nKonwerter z Visual Basic na Prolog\n\n'),
	%testd(Z, R, []),
	%test_const(Z, R, []),
	plik(Z, R, []),
	naglowki(N), concat_atom([N,Z],Gotowe),
	zapisDoPliku("c.txt", Gotowe),
	write(Gotowe), !.

readFromFile(File, Output) :-
	%open(File, read, Temp, [encoding(utf8)]),
	open(File, read, Temp),
	read_stream_to_codes(Temp, Output),
	close(Temp).

zapisDoPliku(Plik, Tekst) :-
	open(Plik, write, X),
	current_output(C0),
	set_output(X),
	write(Tekst),
	close(X),
	set_output(C0).

%testfloat
test(P) --> liczba(M), "\n", test(P1), {concat_atom([M,'\n'],X),info(X),concat_atom([M,P1],P)}.
test(P) --> liczba(M), {concat_atom([M,'\n'],X),info(X),concat_atom([M],P)}.
%testdef
testd(D) --> list_def(LD,0),{concat_atom([LD],D)}.
%test const
test_const(D) --> list_def_const(LD,0),{concat_atom([LD],D)}.
%testcout
testc(TC) --> cout(C,0), "\n", testc(C1), {concat_atom([C,'\n'],X),info(X),concat_atom([C,'\n',C1],TC)}.
testc(TC) --> cout(C,0), {concat_atom([C,'\n'],X),info(X),concat_atom([C],TC)}.
%test for
testf(TF) --> petla_for(F,0), testf(F1), {concat_atom([F,'\n'],X),info(X),concat_atom([F,'\n',F1],TF)}.
testf(TF) --> petla_for(F,0), {concat_atom([F,'\n'],X),info(X),concat_atom([F],TF)}.
%test linia
test_linia(TL) --> linie(TL,0).
	
%plik(P) --> tekst(P).
plik(P) --> modul(M,-1), nl_k, plik(P2), {info('moduly\n'), concat_atom([M,P2],P)}.
plik(P) --> modul(M,-1), nl_o, {info('modul\n'), concat_atom([M],P)}.

naglowki(N) :- concat_atom(['#include <stdio.h>\n#include <iostream>\nusing namespace std;\n\n'],N).

%modul(M) --> "Module", tekst(Cialo), "End Module", {concat_atom([Cialo],M)}.
%modul(M) --> "Module", odstep, wyraz(Nazwa), nowa_linia, tekst(MC), "End Module", !, {concat_atom([W,'modul:',Nazwa,'\n',MC],M)}.
modul(M,Wci) --> "Module", odstep_k, nazwa(_), nl_k, !, {Wci1 is Wci+1}, modul_cialo(MC,Dekl,Wci1), !, "End Module", {concat_atom([Dekl,'\n',MC],M)}.
modul_cialo(M,Dekl,Wci) --> funkcja(F,N,Wci),   modul_cialo(M2,Dekl2,Wci), {concat_atom([F,M2],M), concat_atom([N,Dekl2],Dekl), info(' FUN ')}.
modul_cialo(M,Dekl,Wci) --> procedura(P,N,Wci), modul_cialo(M2,Dekl2,Wci), {concat_atom([P,M2],M), concat_atom([N,Dekl2],Dekl), info(' PROC ')}.
modul_cialo(M,Dekl,Wci) --> struktura(S,N,Wci), modul_cialo(M2,Dekl2,Wci), {concat_atom([S,M2],M), concat_atom([N,Dekl2],Dekl), info(' STRUCT ')}.
modul_cialo('','',_) --> "".

funkcja(F,Dekl,Wci) --> funkcja_nagl(FN), {Wci1 is Wci+1}, funkcja_cialo(FC,Wci1),  {info('funkcja\n'), wciecie(W,Wci), concat_atom(['\n',W,FN,'\n',W,'{\n',FC,W,'}\n'],F), concat_atom([FN, ';\n'],Dekl)}.
funkcja_nagl(FN) --> "Function", odstep_k, nazwa(Nazwa), odstep, !, "(", parametry(Parametry), ")", !, odstep, "As", odstep_k, wyraz(Typ), nl_k, !, {concat_atom([Typ,' ',Nazwa, '(', Parametry, ')'],FN), info(FN)}.
funkcja_cialo('',_) --> funkcja_stopka.
funkcja_cialo(FC,Wci) --> funkcja_return(R,Wci), funkcja_cialo(FC1,Wci), {concat_atom([R,FC1],FC)}.
funkcja_cialo(FC,Wci) --> instrukcja(Cialo,Wci), funkcja_cialo(FC1,Wci), {concat_atom([Cialo,FC1],FC)}.
funkcja_stopka --> "End Function", nl_k.
funkcja_return(R,Wci) --> "Return", odstep_k, wyrazenie(Wyr), {wciecie(W,Wci), concat_atom([W, 'return ',Wyr,';'],R)}.

procedura(F,Dekl,Wci) --> procedura_nagl(PN), {Wci1 is Wci+1}, procedura_cialo(PC,Wci1), {info('procedura\n'), wciecie(W,Wci), concat_atom(['\n',W,PN,'\n',W,'{\n',PC,W,'}\n'],F), concat_atom([PN, ';\n'],Dekl)}.
procedura_nagl(PN) --> "Sub", odstep_k, wyraz(Nazwa), odstep, !, "(" , parametry(Parametry), ")", nl_k, {concat_atom(['void ',Nazwa, '(', Parametry, ')'],PN), info(PN)}.
procedura_cialo('',_) --> procedura_stopka.
procedura_cialo(PC,Wci) --> instrukcja(Cialo, Wci), procedura_cialo(PC1,Wci), {concat_atom([Cialo,PC1],PC)}.
procedura_stopka --> "End Sub", nl_k.

struktura(F,Dekl,Wci) --> struktura_nagl(SN), {Wci1 is Wci+1}, struktura_cialo(SC,Wci1), {info('struktura\n'), wciecie(W,Wci), concat_atom(['\n',W,SN,'\n',W,'{\n',SC,W,'}\n'],F), concat_atom([SN, ';\n'],Dekl)}.
struktura_nagl(SN) --> "Structure", odstep_k, wyraz(Nazwa), odstep, !, nl_k, {concat_atom(['struct ',Nazwa],SN), info(SN)}.
struktura_cialo('',_) --> struktura_stopka.
struktura_cialo(SC,Wci) --> instrukcja(Cialo, Wci), struktura_cialo(SC1,Wci), {concat_atom([Cialo,SC1],SC)}.
struktura_stopka --> "End Structure", nl_k.

%parametry(P) --> parametry_1(P), {concat_atom(['parametry: ',P,'\n'],X), info(X)}.
%parametry_1(P)  --> odstep,   wyraz(W), parametry_n(P2), {concat_atom([W,' ',P2],P)}.
%parametry_1('') --> odstep.
%parametry_n(P)  --> odstep_k, wyraz(W), parametry_n(P2), {concat_atom([W,' ',P2],P)}.
%parametry_n('') --> odstep.
%parametry_("") --> "".
parametry(P) --> parametr(P1), odstep, ",", odstep, parametry(P2), {concat_atom([P1,', ',P2],P)}.
parametry(P) --> parametr(P).
parametry('') --> "".
parametr(P) --> "ByVal", odstep_k, nazwa(N), odstep_k, "As", odstep_k, typ(T), {concat_atom([T,' ',N],P)}.
%parametry

instrukcje(I,Wci) --> instrukcja(I1,Wci), instrukcje(I2,Wci), {concat_atom([I1,I2],I)}.
instrukcje('',_) --> "".

%Typ
typ('int')--> "Integer".
typ('bool')--> "Boolean".
typ('double')--> "Double".
typ('char')--> "Char".


%Definicja zmiennych
list_def(LD,Wci) --> definition(D,Wci), nl_k, list_def(LD1,Wci), {concat_atom([D,'\n',LD1],LD)}.
list_def(LD,Wci) --> definition(D,Wci), {concat_atom([D, '\n'],LD)}.
definition(L,Wci) --> "Dim", odstep_k, lista_list_zmiennych(L,Wci).
lista_list_zmiennych(L,Wci) --> lista_zmiennych(LZ,Wci), odstep, ",", odstep, lista_list_zmiennych(LLZ,Wci), {concat_atom([LZ,'\n',LLZ],L),info(L)}.
lista_list_zmiennych(L,Wci) --> lista_zmiennych(L,Wci).
%lista_zmiennych(L,Wci) --> lista_nazw_zmiennnych(LNZ), odstep_k, "As", odstep_k, typ(T), ew_podstawienie(P), {wciecie(W,Wci), concat_atom([W, T,' ',LNZ,P,';'],L),info(L)}.
lista_zmiennych(L,Wci) --> zmienne_bez_wart(LNZ), odstep_k, "As", odstep_k, typ(T), {wciecie(W,Wci), concat_atom([W, T,' ',LNZ,';'],L),info(L)}.
lista_zmiennych(L,Wci) --> zmienne_z_wart(LNZ), nazwa(N), odstep_k, "As", odstep_k, typ(T), podstawienie(P), {wciecie(W,Wci), concat_atom([W,LNZ,T,' ',N, ' = ', P,';'],L),info(L)}.
lista_zmiennych(L,Wci) --> zmienne_z_wart(ZZW), zmienne_bez_wart(ZBW), odstep_k, "As", odstep_k, typ(T), {wciecie(W,Wci), concat_atom([W,ZZW,T,' ', ZBW,';'],L),info(L)}.
zmienne_z_wart(L) --> zmienne_z_wart_(L).
zmienne_z_wart('') --> "".
zmienne_z_wart_(L) --> nazwa(N), podstawienie(P,Typ), odstep, ",", odstep, zmienne_z_wart_(LZ), {concat_atom([Typ, ' ', N, ' = ', P,';\n',LZ],L)}.
zmienne_z_wart_('') --> "".
zmienne_bez_wart(L) --> nazwa(N), odstep, ",", odstep, zmienne_bez_wart(LZ), {concat_atom([N, ', ', LZ],L)}.
zmienne_bez_wart(L) --> nazwa(N),"(", calkowita(LC), ")", odstep, ",", odstep, zmienne_bez_wart(LZ), {concat_atom([N,'[', LC, '], ',LZ],L)}.
zmienne_bez_wart(N) --> nazwa(N).
zmienne_bez_wart(Z) --> nazwa(N),"(", calkowita(LC), ")", {concat_atom([N,'[', LC, ']'],Z)}.
podstawienie(P) --> odstep, "=", odstep, wartosc(P).
podstawienie(P,Typ) --> odstep, "=", odstep, wartosc(P,Typ).
%ew_podstawienie(P) --> podstawienie(Wart), {concat_atom([' = ',P],Wart)}.
%ew_podstawienie('') --> "".

%Definicja stałych
list_def_const(LD,Wci) --> {info('D1\n')}, definition_const(D,Wci), nl_k, list_def_const(LD1,Wci), {concat_atom([D,'\n',LD1],LD)}.
list_def_const(LD,Wci) --> {info('D2\n')}, definition_const(D,Wci), {concat_atom([D, '\n'],LD)}.
definition_const(L,Wci) --> {info('C\n')}, "Const", odstep_k, lista_stalych(L,Wci).
lista_stalych(L,Wci) --> {info('L1\n')}, stala(LZ,Wci), odstep, ",", odstep, lista_stalych(LLZ,Wci), {concat_atom([LZ,'\n',LLZ],L),info(L)}.
lista_stalych(L,Wci) --> {info('L2\n')}, stala(L,Wci).
stala(L,Wci) --> {info('S1\n')}, nazwa(N), odstep_k, "As", odstep_k, typ(T), odstep, "=", odstep, wartosc(Wart), {wciecie(W,Wci), concat_atom([W, 'const ', T, ' ', N, ' = ',Wart ,';'],L),info(L)}.
stala(L,Wci) --> {info('S2\n')}, nazwa(N), odstep, "=", odstep, wartosc_int(Wart), {wciecie(W,Wci), concat_atom([W, 'const ', 'int', ' ', N, ' = ', Wart, ';'],L),info(L)}.
stala(L,Wci) --> {info('S2\n')}, nazwa(N), odstep, "=", odstep, wartosc_double(Wart), {wciecie(W,Wci), concat_atom([W, 'const ', 'double', ' ', N, ' = ', Wart, ';'],L),info(L)}.
stala(L,Wci) --> {info('S2\n')}, nazwa(N), odstep, "=", odstep, wartosc_char(Wart), {wciecie(W,Wci), concat_atom([W, 'const ', 'char', ' ', N, ' = ', Wart, ';'],L),info(L)}.
stala(L,Wci) --> {info('S2\n')}, nazwa(N), odstep, "=", odstep, wartosc_bool(Wart), {wciecie(W,Wci), concat_atom([W, 'const ', 'bool', ' ', N, ' = ', Wart, ';'],L),info(L)}.

wartosc(W) --> wartosc_int(W).
wartosc(W) --> wartosc_double(W).
wartosc(W) --> wartosc_char(W).
wartosc(W) --> wartosc_bool(W).
wartosc(W, 'int') --> wartosc_int(W).
wartosc(W, 'double') --> wartosc_double(W).
wartosc(W, 'char') --> wartosc_char(W).
wartosc(W, 'bool') --> wartosc_bool(W).
wartosc_int(W) --> calkowita(W).
wartosc_double(W) --> zmiennap(W).
wartosc_char(W) --> "\"", [Znak], "\"", {atom_codes(Z, [Znak]), concat_atom(['\'', Z, '\''],W)}.
wartosc_bool('true') --> "True".
wartosc_bool('false') --> "False".

%petla FOR
petla_for(F,Wci) --> petla_for_naglowek(FN,Zmienna), {Wci1 is Wci+1}, petla_for_cialo(FC,Zmienna, Wci1), {wciecie(W,Wci), concat_atom([W, FN, '\n', W, '{', FC, W,'}\n'],F)}.
petla_for_naglowek(F,N) --> "For", odstep_k, nazwa(N), odstep, "=", odstep, wyrazenie(LP), odstep_k, "To", odstep_k, wyrazenie(LK), odstep_k, "Step", odstep_k, calkowita(S), {concat_atom(['for(',N,'=',LP,'; ',N,'<=',LK,'; ',N,'=',N,'+', S,')'],F)}.
petla_for_naglowek(F,N) --> "For", odstep_k, nazwa(N), odstep, "=", odstep, wyrazenie(LP), odstep_k, "To", odstep_k, wyrazenie(LK), {concat_atom(['for(',N,'=',LP,'; ',N,'<=',LK,'; ',N,'=',N,'+1)'],F)}.
petla_for_cialo('',N,_) --> petla_for_stopka(N).
petla_for_cialo(FC,N,Wci) --> petla_for_exit(E,0), petla_for_cialo(FC1,N,Wci), {wciecie(W,Wci), concat_atom([W,E,'\n',FC1],FC)}.
petla_for_cialo(FC,N,Wci) --> petla_for_continue(C,0), petla_for_cialo(FC1,N,Wci), {wciecie(W,Wci), concat_atom([W,C,'\n',FC1],FC)}.
petla_for_cialo(FC,N,Wci) --> instrukcja(I,Wci), petla_for_cialo(FC1,N,Wci), {concat_atom([I,FC1],FC)}.
petla_for_stopka(N) --> "Next", odstep_k, nazwa(N), nl_k.
petla_for_exit(E,Wci) --> "Exit", odstep_k, "For", nl_k, {wciecie(W,Wci), concat_atom([W, 'break;'],E)}.
petla_for_continue(C,Wci) --> "Continue", odstep_k, "For", nl_k, {wciecie(W,Wci), concat_atom([W, 'continue;'],C)}.

%instrukcja warunkowa IF
warunek_if(IF,Wci) --> if_jednoliniowy(IF,Wci).
warunek_if(IF,Wci) --> if_wieloliniowy(IF,Wci).
if_jednoliniowy(IF,Wci) --> "If", odstep_k, warunek(War), odstep_k, "Then", odstep_k, instrukcja(I,0), else_jednoliniowy(E), nl_k, {wciecie(W,Wci), concat_atom([W,'if ', War,' ',I,E,'\n'],IF)}.
else_jednoliniowy(E) --> odstep_k, "Else", odstep_k, instrukcja(I,0), {concat_atom([' else ',I],E)}.
else_jednoliniowy('') --> "".
if_wieloliniowy(IF,Wci) --> if_naglowek(IN), {Wci1 is Wci+1}, if_cialo(IC,Wci1), {wciecie(W,Wci), concat_atom([W, IN, '\n', W, '{\n', IC, W,'}\n'],IF)}.
if_naglowek(IN) --> "If", odstep_k, warunek(War), odstep_k, then, nl_k, {concat_atom(['if ', War],IN)}.
if_cialo('', _) --> if_stopka.
%if_cialo(IC, Wci) --> "Else", {Wci1 is Wci-1}, if_wieloliniowy(IC1,Wci1), {wciecie(W,Wci1), concat_atom([W,'}else\n',IC1],IC)}.
if_cialo(IC, Wci) --> "Else", if_naglowek(IN), if_cialo(IC1,Wci), {Wci1 is Wci-1, wciecie(W,Wci1), concat_atom([W,'}else ',IN,'\n', W ,'{\n',IC1],IC)}.
if_cialo(IC, Wci) --> "Else", nl_k, if_cialo(IC1,Wci), {Wci1 is Wci-1, wciecie(W,Wci1), concat_atom([W,'}else\n', W, '{\n',IC1],IC)}.
if_cialo(IC, Wci) --> instrukcja(I,Wci), if_cialo(IC1,Wci), {concat_atom([I,IC1],IC)}.
if_stopka --> "End If", nl_k.
then --> "Then".
then --> "".


warunek(War) --> wyrazenie(W1), odstep, relacja(R), odstep, wyrazenie(W2), {concat_atom(['(', W1, ' ', R, ' ', W2, ')'],War)}.
%warunek(War) --> liczba(W1), odstep, relacja(R), odstep, liczba(W2), {concat_atom(['(', W1, ' ', R, ' ', W2, ')'],War)}.
relacja('!=') --> "<>".
relacja('<=') --> "<=".
relacja('>=') --> ">=".
relacja('==') --> "=".
relacja('>') --> ">".
relacja('<') --> "<".

instrukcja(I,Wci) --> petla_for(I,Wci), {info(' FOR ')}.
instrukcja(I,Wci) --> cout(I,Wci), {info(' COUT ')}.
instrukcja(I,Wci) --> list_def(I,Wci), {info(' DEF ')}.
instrukcja(I,Wci) --> list_def_const(I,Wci), {info(' DEF ')}.
instrukcja(I,Wci) --> komentarz(I,Wci).
instrukcja(I,Wci) --> przypisanie(I,Wci).
instrukcja(I,Wci) --> warunek_if(I,Wci).
instrukcja(I,Wci) --> petla_for_exit(I,Wci).
instrukcja(I,Wci) --> petla_for_continue(I,Wci).
instrukcja(I,Wci) --> "Randomize()", nl_k, {wciecie(W,Wci), concat_atom([W,'srand(time(0));\n'],I)}.
instrukcja(I,Wci) --> wywolanie_funkcji(WF), nl_k, {wciecie(W,Wci), concat_atom([W,WF,';\n'],I)}.
instrukcja(I,Wci) --> linia(I,Wci).%, {info(' LINIA ')}.
%instrukcja(I,Wci) --> odstep,{concat_atom(['<lin>'],I)}.%, {concat_atom(['//',L],I), info(' LINIA ')}.
%instrukcja(I,Wci) --> linia(L,Wci),{concat_atom(['//',L],I)}.%, {concat_atom(['//',L],I), info(' LINIA ')}.


komentarz(I,Wci) --> "'", linia(L,0), {wciecie(W,Wci), concat_atom([W,'//',L],I)}.

przypisanie(P,Wci) --> nazwa(N1), odstep, "=", odstep, wyrazenie(Wyr), nl_k, {wciecie(W,Wci), concat_atom([W,N1,' = ', Wyr, ';\n'],P)}.
przypisanie(P,Wci) --> nazwa(N1),"(",wyrazenie(Indeks),")", odstep, "=", odstep, wyrazenie(Wyr), nl_k, {wciecie(W,Wci), concat_atom([W,N1,'[',Indeks,'] = ', Wyr, ';\n'],P)}.

wyrazenie(Wyr) --> elem_wyr(E1), odstep, operator(O), odstep, wyrazenie(E2), {concat_atom([E1,O,E2],Wyr)}.
wyrazenie(Wyr) --> elem_wyr(E1), odstep, operator(O), odstep, elem_wyr(E2), {concat_atom([E1,O,E2],Wyr)}.
wyrazenie(Wyr) --> elem_wyr(E), {concat_atom([E],Wyr)}.
lista_wyrazen(LW)  --> wyrazenie(Wyr), odstep, ",", odstep, lista_wyrazen(LW1), {concat_atom([Wyr,', ',LW1],LW)}.
lista_wyrazen(Wyr) --> wyrazenie(Wyr).
lista_wyrazen('')  --> "".
operator('+') --> "+".
operator('-') --> "-".
operator('*') --> "*".
operator('/') --> "/".
operator('%') --> "Mod".
elem_wyr(EW) --> "(", odstep, wyrazenie(Wyr), odstep, ")", {concat_atom(['(',Wyr,')'],EW)}.
elem_wyr(EW) --> nazwa(Nazwa),".Length", {concat_atom(['(sizeof ',Nazwa,' / sizeof *',Nazwa,')'],EW)}.
elem_wyr(EW) --> "Rnd()", {concat_atom(['(rand()/RAND_MAX)'],EW)}.
elem_wyr(EW) --> wywolanie_funkcji(EW).
elem_wyr(EW) --> nazwa(EW).
elem_wyr(EW) --> liczba(EW).
wywolanie_funkcji(WF) --> nazwa(Nazwa),"(",lista_wyrazen(LW),")", {concat_atom([Nazwa,'(',LW,')'],WF)}.

%Biale znaki.
ws --> " ", ws.
ws --> "\t", ws.
ws --> "\n", ws.
ws --> "".
odstep --> " ", odstep. %odstep opcjonalny
odstep --> "\t", odstep.
odstep --> "".
odstep_k --> " ", odstep_k.   %odstep konieczny
odstep_k --> "\t", odstep_k.
odstep_k --> " ".
odstep_k --> "\t".
nowa_linia --> "\n", nowa_linia.
nowa_linia --> "".
przynajmniej1nl --> ws, "\n", ws.
nl_z --> "\n\r".
nl_z --> "\r\n".
nl_z --> "\n".
nl_z --> "\r".
nl_k --> odstep, nl_z, odstep, nl_.
nl_  --> nl_z, odstep, nl_.
nl_  --> "".
nl_o --> odstep, nl_z, nl_o.
nl_o --> odstep.

%wciecia
wciecie('',0).
wciecie('\t',1).
wciecie(W,L) :- L>1, L1 is L-1, wciecie(W1,L1), concat_atom([W1,'\t'],W).

%liczby ca3kowite
liczba(I) --> liczba_i(I), {!}.
liczba_i(I) --> zmiennap(I).
liczba_i(I) --> calkowita(I).
calkowita(I) --> cyfra(I1), calkowita(Rest), {concat_atom([I1,Rest], I)}.
calkowita(I) --> cyfra(I).
cyfra(I) --> [I1], {code_type(I1, digit), atom_codes(I, [I1])}.
%liczby zmiennoprzecinkowe
zmiennap(I) --> calkowita(I1), ".", calkowita(Rest), {concat_atom([I1,'.',Rest], I)}.

%ci1gi znaków
%string(C) --> chars(C1), odstep, string(C2), {concat_atom([C1,C2],C)}.
string(C) --> chars(C).

chars(C) --> char(C1), chars(Rest), {concat_atom([C1, Rest], C)}.
chars(C) --> char(C).
char('\\"') --> "\\\"".
char('') --> "\"", {!,fail}.
char(C) --> [C1], {code_type(C1, alnum), atom_codes(C, [C1])}.
char(C) --> [C1], {code_type(C1, punct), atom_codes(C, [C1])}.
char(C) --> [C1], {code_type(C1, space), not(code_type(C1, newline)), atom_codes(C, [C1])}.
char('.') --> ".".

wyraz(W) --> wyraz_(W), {concat_atom(['wyraz: ',W,'\n'],X), info(X)}.
wyraz_(W) --> znak_alfanum(Z), wyraz_(W2), {concat_atom([Z,W2],W)}.
wyraz_(W) --> znak_alfanum(Z), {concat_atom([Z],W)}.
znak_alfanum(Z) --> [Znak], {code_type(Znak, alnum), atom_codes(Z, [Znak])}.
znak_bialy(Z) :- code_type(Znak, white), atom_codes(Z, [Znak]).

%nazwa zaczyna sie od litery
nazwa(N) --> nazwa_(N), {concat_atom(['nazwa: ',N,'\n'],X), info(X)}.
nazwa_(N) --> litera(L), wyraz_(W), !, {concat_atom([L,W],N)}.
nazwa_(L) --> litera(L).
litera(L) --> [Znak], {code_type(Znak, alpha), atom_codes(L, [Znak])}.

%LINIA - predykat pomocniczy zastujacy instrukcje (Joker)
linia('\n',_) --> nl_k.
linia(L,Wci) --> [Znak], linia(L1,0), {atom_codes(Z, [Znak]), wciecie(W,Wci), concat_atom([W, Z, L1], L)}.
linie(TL,Wci) --> linia(L,Wci), linie(TL1,Wci), {concat_atom([L, TL1],TL)}.
linie(L,Wci) --> linia(L,Wci).

%ciagi znakow z bia3ymi znakami
tekst(T) --> znak_niebialy(Z), tekst_(T2), {concat_atom([Z,T2],T), concat_atom(['tekst: {',T,'}\n'],X)}.%, info(X)}.
tekst_(T) --> znak(Z), tekst_(T2), {concat_atom([Z,T2],T)}.
tekst_(T) --> znak_niebialy(Z), {concat_atom([Z],T)}.
znak(Z) --> [Znak], {atom_codes(Z, [Znak])}.
znak_niebialy(Z) --> [Znak], {not(code_type(Znak, space)), atom_codes(Z, [Znak])}.

%Drukowanie
cout(C,Wci) --> "Console.Write(",cout_srodek(S),")", {wciecie(W,Wci), concat_atom([W,'cout<<',S,';'],C),info(S)}.
cout(C,Wci) --> "Console.WriteLine(",cout_srodek(S),")", {wciecie(W,Wci), info(S),concat_atom([W,'cout<<',S,'<<endl;'],C),info(S)}.
cout_srodek(S) --> cout_s(CS1), odstep, "&", odstep, cout_srodek(CS2), {concat_atom([CS1,'<<',CS2],S)}.
cout_srodek(S) --> cout_s(S).
cout_s(CS) --> "\"", string(S), "\"", {concat_atom(['\"',S,'\"'],CS)}.
cout_s(CS) --> "\"\"", {concat_atom(['\"\"'],CS)}.
cout_s(CS) --> wyrazenie(CS).


%tekst_do(T,Do) --> znak(Z), tekst_do(T2,Do), {concat_atom([Z,T2],T)}.

%print by móc wy1czyc drukowanie
info(K) :- print(K).