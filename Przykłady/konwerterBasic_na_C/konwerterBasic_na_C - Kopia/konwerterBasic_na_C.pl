start :-
	readFromFile("test for.txt", R),
	print('\nKonwerter z Visual Basic na Prolog\n\n'),
	testf(Z, R, []),
	%plik(Z, R, []),
	zapisDoPliku("c.txt", Z),
	write(Z), !.

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
test(P) --> liczba(M), "\n", test(P1), {concat_atom([M,'\n'],X),komentarz(X),concat_atom([M,P1],P)}.
test(P) --> liczba(M), {concat_atom([M,'\n'],X),komentarz(X),concat_atom([M],P)}.
%testdef
testd(D) --> list_def(LD),{concat_atom([LD],D)}.
%testcout
testc(TC) --> cout(C), "\n", testc(C1), {concat_atom([C,'\n'],X),komentarz(X),concat_atom([C,'\n',C1],TC)}.
testc(TC) --> cout(C), {concat_atom([C,'\n'],X),komentarz(X),concat_atom([C],TC)}.
%test for
testf(TF) --> petla_for(F), nl_k, testf(F1), {concat_atom([F,'\n'],X),komentarz(X),concat_atom([F,'\n',F1],TF)}.
testf(TF) --> petla_for(F), {concat_atom([F,'\n'],X),komentarz(X),concat_atom([F],TF)}.
%test linia
test_linia(TL) --> linie(TL).
	
%plik(P) --> tekst(P).
plik(P) --> modul(M), nowa_linia, plik(P2), nowa_linia, {komentarz('moduly\n'), concat_atom([M,P2],P)}.
plik(P) --> modul(M), nowa_linia, {komentarz('modul\n'), concat_atom([M],P)}.

%modul(M) --> "Module", tekst(Cialo), "End Module", {concat_atom([Cialo],M)}.
%modul(M) --> "Module", odstep, wyraz(Nazwa), nowa_linia, tekst(MC), "End Module", !, {concat_atom(['modul:',Nazwa,'\n',MC],M)}.
modul(M) --> "Module", odstep, wyraz(Nazwa), !, modul_cialo(MC), "End Module", {concat_atom(['modul:',Nazwa,'\n',MC],M)}.
modul_cialo(M) --> ws, funkcja(F),   ws, modul_cialo(M2), {concat_atom([F,M2],M)}.
modul_cialo(M) --> ws, procedura(P), ws, modul_cialo(M2), {concat_atom([P,M2],M)}.
modul_cialo(M) --> ws, funkcja(F),   ws, {concat_atom([F],M)}.
modul_cialo(M) --> ws, procedura(P), ws, {concat_atom([P],M)}.

funkcja(F) --> funkcja_nagl(FN), przynajmniej1nl, funkcja_cialo(FC),  {komentarz('funkcja\n'), concat_atom(['\n',FN,'\n{\n',FC,'\n}\n'],F)}.
funkcja_nagl(FN) --> "Function", odstep, wyraz(Nazwa), !, "(", parametry(Parametry), ")", !, odstep, "As", odstep, wyraz(Typ), !, {concat_atom([Typ,' ',Nazwa, '(', Parametry, ')'],FN), komentarz(FN)}.
funkcja_cialo(FC) --> tekst(Cialo), przynajmniej1nl, "End Function", !, {concat_atom([Cialo],FC)}.

procedura(F) --> procedura_nagl(PN), przynajmniej1nl, procedura_cialo(PC), {komentarz('procedura\n'), concat_atom(['\n',PN,'\n{\n',PC,'\n}\n'],F)}.
procedura_nagl(PN) --> "Sub", odstep, wyraz(Nazwa), !, "(" , parametry(Parametry), ")", {concat_atom(['void ',Nazwa, '(', Parametry, ')'],PN), komentarz(PN)}.
procedura_cialo(PC) --> tekst(Cialo), przynajmniej1nl, "End Sub", !, {concat_atom([Cialo],PC)}.

parametry(P) --> parametry_1(P), {concat_atom(['parametry: ',P,'\n'],X), komentarz(X)}.
parametry_1(P)  --> odstep,   wyraz(W), parametry_n(P2), {concat_atom([W,' ',P2],P)}.
parametry_1('') --> odstep.
parametry_n(P)  --> odstep_k, wyraz(W), parametry_n(P2), {concat_atom([W,' ',P2],P)}.
parametry_n('') --> odstep.
%parametry_("") --> "".

%Typ
typ('int')--> "Integer".
typ('bool')--> "Boolean".
typ('double')--> "Double".
typ('char')--> "Char".


%Definicja
list_def(LD) --> definition(D), nl_k, list_def(LD1), {concat_atom([D,'\n',LD1],LD)}.
list_def(LD) --> definition(D), {concat_atom([D],LD)}.
%definition(D) --> "Dim", odstep, wyraz(W), odstep, "As", odstep, typ(T), {concat_atom([T,' ',W,';'],D),komentarz(D)}.
%definition(D) --> "Dim", odstep_k, lista_nazw_zmiennnych(L), odstep_k, "As", odstep, typ(T), {concat_atom([T,' ',L,';'],D),komentarz(D)}.
definition(L) --> "Dim", odstep_k, lista_list_zmiennych(L).
lista_list_zmiennych(L) --> lista_zmiennych(LZ), odstep, ",", odstep, lista_list_zmiennych(LLZ), {concat_atom([LZ,'\n',LLZ],L),komentarz(L)}.
lista_list_zmiennych(L) --> lista_zmiennych(L).
lista_zmiennych(L) --> lista_nazw_zmiennnych(LNZ), odstep_k, "As", odstep_k, typ(T), {concat_atom([T,' ',LNZ,';'],L),komentarz(L)}.
lista_nazw_zmiennnych(L) --> nazwa(N), odstep, ",", odstep, lista_nazw_zmiennnych(LNZ), {concat_atom([N,', ',LNZ],L)}.
lista_nazw_zmiennnych(N) --> nazwa(N).

%petla for
petla_for(F) --> petla_for_naglowek(FN,Zmienna), petla_for_cialo(FC,Zmienna), {concat_atom([FN, '\n{', FC, '}'],F)}.
petla_for_naglowek(F,N) --> "For", odstep_k, nazwa(N), odstep, "=", odstep, calkowita(LP), odstep_k, "To", odstep_k, calkowita(LK), {concat_atom(['for(',N,'=',LP,'; ',N,'<=',LK,'; ',N,'=',N,'+1)'],F)}.
petla_for_cialo('',N) --> petla_for_stopka(N).
petla_for_cialo(FC,N) --> instrukcja(I), petla_for_cialo(FC1,N), {concat_atom([I,'\n',FC1],FC)}.
petla_for_stopka(N) --> "Next", odstep_k, nazwa(N).


instrukcja(I) --> petla_for(I).
instrukcja(I) --> cout(I).
instrukcja(I) --> list_def(I).
instrukcja(I) --> linia(I).


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
char(C) --> [C1], {code_type(C1, alnum), atom_codes(C, [C1])}.
char(C) --> [C1], {code_type(C1, punct), atom_codes(C, [C1])}.
char(C) --> [C1], {code_type(C1, space), not(code_type(C1, newline)), atom_codes(C, [C1])}.
char('.') --> ".".

wyraz(W) --> wyraz_(W), {concat_atom(['wyraz: ',W,'\n'],X), komentarz(X)}.
wyraz_(W) --> znak_alfanum(Z), wyraz_(W2), {concat_atom([Z,W2],W)}.
wyraz_(W) --> znak_alfanum(Z), {concat_atom([Z],W)}.
znak_alfanum(Z) --> [Znak], {code_type(Znak, alnum), atom_codes(Z, [Znak])}.
znak_bialy(Z) :- code_type(Znak, white), atom_codes(Z, [Znak]).

%nazwa zaczyna sie od litery
nazwa(N) --> nazwa_(N), {concat_atom(['nazwa: ',N,'\n'],X), komentarz(X)}.
nazwa_(N) --> litera(L), wyraz_(W), !, {concat_atom([L,W],N)}.
nazwa_(L) --> litera(L).
litera(L) --> [Znak], {code_type(Znak, alpha), atom_codes(L, [Znak])}.

%LINIA - predykat pomocniczy zastujacy instrukcje (Joker)
linia('') --> nl_k.
linia(L) --> [Znak], linia(L1), {atom_codes(Z, [Znak]), concat_atom([Z, L1], L)}.
linie(TL) --> linia(L), linie(TL1), {concat_atom([L, '\n', TL1],TL)}.
linie(L) --> linia(L).

%ciagi znakow z bia3ymi znakami
tekst(T) --> znak_niebialy(Z), tekst_(T2), {concat_atom([Z,T2],T), concat_atom(['tekst: {',T,'}\n'],X)}.%, komentarz(X)}.
tekst_(T) --> znak(Z), tekst_(T2), {concat_atom([Z,T2],T)}.
tekst_(T) --> znak_niebialy(Z), {concat_atom([Z],T)}.
znak(Z) --> [Znak], {atom_codes(Z, [Znak])}.
znak_niebialy(Z) --> [Znak], {not(code_type(Znak, space)), atom_codes(Z, [Znak])}.

%Drukowanie
cout(C) --> "Console.Write(\"",{komentarz('!')},string(S),"\")", {concat_atom(['cout<<\"',S,'\";'],C),komentarz(S)}.
cout(C) --> "Console.WriteLine(\"",{komentarz('!')},string(S),"\")", {komentarz(S),concat_atom(['cout<<\"',S,'\"<<endl;'],C),komentarz(S)}.


%tekst_do(T,Do) --> znak(Z), tekst_do(T2,Do), {concat_atom([Z,T2],T)}.

%print by móc wy1czyc drukowanie
komentarz(K) :- print(K).