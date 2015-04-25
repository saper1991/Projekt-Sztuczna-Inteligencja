#!/usr/bin/swipl -q -t basic2c -s

% Przetwornik języka Atari BASIC na język C
%
% Autorzy:
%   Mariusz Galler
%   Stanisław Karlik
%   Radosław Szalski
%

% --------------------------------------------------------------------------------
% DCG
% --------------------------------------------------------------------------------

% Wszystko otaczane jest int main() {} aby można było automatycznie kompilować.
% TODO dodawanie otoczki include/main powinno być opcjonalne ?
program(Z) --> code(Za), {concat_atom(['#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <math.h>\nint main() {\nsrand(time(NULL));\n', Za, '\n}'], Z)}.

% Kod - składa się z 1 lub więcej linii zakończonych znakami nowej linii.
% Przypadek, gdy program nie kończy się pustą linią
code(Z) --> line(Z1), whitespace, {concat_atom([Z1], Z)}.
code(Z) --> line(Z1), whitespace, newline, code(Z2), {concat_atom([Z1, '\n', Z2], Z)}.
code(Z) --> line(Z1), whitespace, newline, {concat_atom([Z1, '\n'], Z)}.

% Zakomentowane linie, jeżeli napotkamy '#', to nic innego nas nie interesuje -> ignorujemy do '\n'
line(Z) --> whitespace, comment_sign, anything(_), {concat_atom([''], Z)}.
line(Z) --> whitespace, lineno(_), whitespace, rem, whitespace, anything(A), {concat_atom(['// ', A], Z)}.
line(Z) --> whitespace, lineno(N), whitespace, statements(Z1), {concat_atom(['l', N, ':; ', Z1], Z)}.

% Komentarz wew. całkowite ignorowanie linii, różny od instrukcji REM.
comment_sign --> "#".
% Właściwy 'komentarz' w języku Atari BASIC, powoduje ignorowanie linii ale zostaje ona zawarta w kodzie wynikowym.
rem --> "rem".

statements(S) --> statement(S).
statements(S) --> statement(S1), whitespace, ":", whitespace, statements(S2), {concat_atom([S1, S2], S)}.

% Deklaruje domyślnie zmienne stringowe jako char*
statement(Z) --> "dim", whitespace, dimstmt(D), {concat_atom([D], Z)}.
statement(Z) --> "dim", whitespace, dimstmt(D1), whitespace, ",", whitespace, dimstmt(D2), {concat_atom([D1, '\n', D2], Z)}.
% Warunek IF
statement(Z) --> "if", whitespace, lexp(E), whitespace, "then", whitespace, statement(ZW), {concat_atom(['if( ', E, ' ){\n', ZW, '\n}'], Z)}.
statement(Z) --> "if", whitespace, lexp(E), whitespace, "then", whitespace, lineno(N), {concat_atom(['if( ', E, ' )\n', 'goto l', N, ';\n'], Z)}.

% INPUT
statement(Z) --> "input", whitespace, aexp(A), {concat_atom(['int ', A, ';\n', 'scanf("%d", &', A, ');'], Z)}.
statement(Z) --> "input", whitespace, sexp(A), {concat_atom(['scanf("%s", ', A, ');'], Z)}.

% GOTO
statement(Z) --> "goto", whitespace, lineno(N), {concat_atom(['goto l', N, ';'], Z)}.

% Pętla FOR
% C nie dopuszcza deklaracji w for(), dlatego zmienna sterująca jest zawsze wcześniej deklarowana. Jesteśmy narażeni na błąd redeklaracji zmiennej w przypadku ponownego użycia tej samej w forze..
statement(Z) --> "for", whitespace, avar(A), whitespace, "=", aexp(B), whitespace, "to", whitespace, avar(A), whitespace, aoper(O), whitespace, aexp(C), {concat_atom(['int ', A, ';\n', 'for(', A, ' = ', B, '; ', A, ' ', O, ' ', C, '; ', A, ' += 1', '){'], Z)}.
statement(Z) --> "for", whitespace, avar(A), whitespace, "=", aexp(B), whitespace, "to", whitespace, avar(A), whitespace, aoper(O), whitespace, aexp(C), whitespace, "step", whitespace, aexp(D), {concat_atom(['int ', A, ';\n', 'for(', A, ' = ', B, '; ', A, ' ', O, ' ', C, '; ', A, ' += ', D, '){'], Z)}.
statement(Z) --> "next", whitespace, avar(_), {concat_atom(['}'], Z)}.
% wersja tylko z liczbą w warunku (FOR I=0 TO 2 ...)
statement(Z) --> "for", whitespace, avar(A), whitespace, "=", aexp(B), whitespace, "to", whitespace, aexp(C), {concat_atom(['int ', A, ';\n', 'for(', A, ' = ', B, '; ', A, ' ', '<=', ' ', C, '; ', A, ' += 1', '){'], Z)}.
statement(Z) --> "for", whitespace, avar(A), whitespace, "=", aexp(B), whitespace, "to", whitespace, aexp(C), whitespace, "step", whitespace, aexp(D), {concat_atom(['int ', A, ';\n', 'for(', A, ' = ', B, '; ', A, ' ', '<=', ' ', C, '; ', A, ' += ', D, '){'], Z)}.
statement(Z) --> "next", whitespace, avar(_), {concat_atom(['}'], Z)}.

statement(Z) --> whitespace, "stop", anything(_), {concat_atom(['\nprintf("Stopped at line");'], Z)}.
% Sam print nie może narzucać cudzysłowów, zależą one od instrukcji wew.
statement(Z) --> "print", whitespace, print_exps(C), {concat_atom(['printf(', C, ');'], Z)}.
statement(Z) --> let_or_not, whitespace, avar(A), whitespace, "=", whitespace, aexp(B), {concat_atom(['float ', A, ' = ', B, ';'], Z)}.
% Funkcja INT - może wystąpić tylko w przypisaniu
statement(Z) --> let_or_not, whitespace, avar(A), whitespace, "=", whitespace, int_fnc(B), {concat_atom(['int ', A, ' = ', B, ';'], Z)}.
statement(Z) --> let_or_not, whitespace, svar(A), whitespace, "=", whitespace, sexp(B), {concat_atom([A, ' = ', B, ';'], Z)}.

statement(Z) --> strcat(A), {concat_atom([A, ';'], Z)}. 

dimstmt(Z) --> svar(A), "(", aexp(_), ")", {concat_atom(['char* ', A, ';'], Z)}.
dimstmt(Z) --> avar(A), "(", aexp(L), ")", {concat_atom(['int ', A, '[(int)', L, '];'], Z)}.
dimstmt(Z) --> avar(A), "(", aexp(L1), ",", aexp(L2), ")", {concat_atom(['int ', A, '[(int)', L1, '][(int)', L2, '];'], Z)}. 

print_exps(C) --> sexp(C1), {concat_atom(['"%s\\n", ', C1], C)}.
print_exps(C) --> aexp(C1), {concat_atom(['"%f\\n", ', '(float) ', C1], C)}.
print_exps(C) --> "(", whitespace, aexp(C1), whitespace, ")", {concat_atom(['"%f\\n", ', '(float) ', C1], C)}.
print_exps(C) --> exp(C1), separator(Sep), exps(C2), {concat_atom([C1, Sep, C2], C)}.
% ogólny przypadek exp na końcu!
print_exps(C) --> exp(C1), {concat_atom(['"', C1, '"'], C)}.

% Funkcja INT
int_fnc(B) --> "int", whitespace, "(", whitespace, aexp(B1), whitespace, ")", {concat_atom(['floor(', B1, ')'], B)}.

exp(C) --> aexp(C).
exp(C) --> sexp(C).

anyvar(C) --> avar(C).
anyvar(C) --> svar(C).

aexp(C) --> aconst(C).
aexp(C) --> funcs(C).
aexp(C) --> avar(C).
aexp(C) --> simple_aexp(C1), whitespace, aop(C2), whitespace, simple_aexp(C3), {concat_atom([C1, C2, C3], C)}.
aexp(C) --> simple_aexp(C1), whitespace, "^", whitespace, simple_aexp(C2), {concat_atom(['pow((double)',C1,', (double)', C2, ')'], C)}.

simple_aexp(C) --> aconst(C).
simple_aexp(C) --> funcs(C).
simple_aexp(C) --> avar(C).

aop(R) --> "+", {concat_atom(['+'], R)}.
aop(R) --> "-", {concat_atom(['-'], R)}.
aop(R) --> "*", {concat_atom(['*'], R)}.
aop(R) --> "/", {concat_atom(['/'], R)}.

sexp(C) --> sfunc(C).
sexp(C) --> svar(C).
sexp(C) --> sconst(C).
sexp(C) --> strcat(C).

lexp(R) --> aexp(E), {concat_atom(['fabs( ', E, ' )'], R)}.
lexp(R) --> aexp(E1), whitespace, lop(O), whitespace, aexp(E2), {concat_atom([E1, O, E2], R)}.
lexp(R) --> "(", whitespace, lexp(E), whitespace, ")", {concat_atom(['(', E, ')'], R)}.
lexp(R) --> "not(", whitespace, lexp(E), whitespace, ")", {concat_atom(['!(', E, ')'], R)}.
lexp(R) --> single_lexp(E1), whitespace, "and", whitespace, single_lexp(E2), {concat_atom([E1, ' && ', E2], R)}.
lexp(R) --> single_lexp(E1), whitespace, "or", whitespace, single_lexp(E2), {concat_atom([E1, ' || ', E2], R)}.

single_lexp(R) --> aexp(E), {concat_atom(['fabs( ', E, ' )'], R)}.
single_lexp(R) --> aexp(E1), whitespace, lop(O), whitespace, aexp(E2), {concat_atom([E1, O, E2], R)}.
single_lexp(R) --> "(", whitespace, lexp(E), whitespace, ")", {concat_atom(['(', E, ')'], R)}.
single_lexp(R) --> "not(", whitespace, lexp(E), whitespace, ")", {concat_atom(['!(', E, ')'], R)}.

lop(R) --> "=", {concat_atom(['=='], R)}.
lop(R) --> "<>", {concat_atom(['!='], R)}.
lop(R) --> ">", {concat_atom(['>'], R)}.
lop(R) --> "<", {concat_atom(['<'], R)}.
lop(R) --> "<=", {concat_atom(['<='], R)}.
lop(R) --> ">=", {concat_atom(['>='], R)}.

funcs(C) --> fun(C1), whitespace, "(", whitespace, aexp(C2), whitespace, ")", {concat_atom([C1, '(', C2, ')'], C)}.
funcs(C) --> "val", whitespace, "(", whitespace, sexp(C2), whitespace, ")", {concat_atom(['atoi(', C2, ')'], C)}.
% Funkcja SGN, traktowana osobno, ponieważ zastępujemy ją odpowiadającym wyrażeniem w C.
funcs(C) --> "sgn", whitespace, "(", whitespace, aexp(C2), whitespace, ")", {concat_atom(['((', C2, ' > 0) ? 1 : ((', C2, ' < 0) ? 0 : 0))'], C)}.
% RND Wymaga inicjalizacji generatora, liczba w nawiasach to 'dummy variable'.
funcs(C) --> "rnd", whitespace, "(", whitespace, aexp(_), whitespace, ")", {concat_atom(['((double) rand() / (RAND_MAX))'], C)}.
% Wartość adresu musi być rzutowana najpierw na int.
funcs(C) --> "adr", whitespace, "(", whitespace, anyvar(C2), whitespace, ")", {concat_atom(['(int) &', C2], C)}.
% Funkcje log10 i exp w C nie przyjmują parametrów typu int
funcs(C) --> "clog", whitespace, "(", whitespace, aexp(C2), whitespace, ")", {concat_atom(['log10((double) ', C2, ' )'], C)}.
funcs(C) --> "exp", whitespace, "(", whitespace, aexp(C2), whitespace, ")", {concat_atom(['exp((double) ', C2, ' )'], C)}.
funcs(C) --> "asc", whitespace, "(", whitespace, sexp(C2), whitespace, ")", {concat_atom(['(int)', C2, '[0]'], C)}.
funcs(C) --> "len", whitespace, "(", whitespace, sexp(C2), whitespace, ")", {concat_atom(['strlen( ', C2, ' )'], C)}.
fun(C) --> "log", {concat_atom(['log'], C)}.
fun(C) --> "sqr", {concat_atom(['sqrt'], C)}.
fun(C) --> "atn", {concat_atom(['atan'], C)}.
fun(C) --> "cos", {concat_atom(['cos'], C)}.
fun(C) --> "sin", {concat_atom(['sin'], C)}.
fun(C) --> "abs", {concat_atom(['fabs'], C)}.

sfunc(C) --> "chr$", whitespace, "(", aexp(E), whitespace, ")", {concat_atom(['(char)', E], C)}.

avar(C) --> alpha_char(Ca), string(C2), {atom_codes(C1, [Ca]), concat_atom([C1, C2], C)}.
avar(C) --> alpha_char(C1), {atom_codes(C, [C1])}.
avar(I) --> "-", avar(I1), {concat_atom(['-', I1], I)}.
avar(I) --> "+", avar(I1), {concat_atom([I1], I)}.

aconst(I) --> number(I).
aconst(I) --> "-", number(I1), {concat_atom(['-', I1], I)}.
aconst(I) --> "+", number(I1), {concat_atom([I1], I)}.

svar(C) --> avar(C1), "$", {concat_atom([C1, '$'], C)}.
sconst(C) --> "\"", string_or_empty(C1), "\"", {concat_atom(['"', C1 ,'"'], C)}.

strcat(C) --> svar(C1), "(", whitespace, "len", whitespace, "(", whitespace, svar(_), whitespace, "+", whitespace, "1", whitespace, ")", whitespace, ")", whitespace, "=", whitespace, svar(C3), {concat_atom(['strcat(', C1, ',', C3, ')'], C)}.
%strcat(C) --> svar(C1), "(", whitespace, "len(", whitespace, svar(_), whitespace, ")", whitespace, "+", whitespace, "1", whitespace, ")", whitespace, "=", whitespace, svar(C3), {concat_atom(['strcat(', C1, ',', C3, ')'], C)}.

% Operatory arytmetyczne
aoper(O) --> "<", {concat_atom(['<'], O)}.
aoper(O) --> ">", {concat_atom(['>'], O)}.
aoper(O) --> "<=", {concat_atom(['<='], O)}.
aoper(O) --> "<=", {concat_atom(['<='], O)}.

% Separator ";" nie generuje żadnego znaku.
separator(Sep) --> whitespace, ";", whitespace, {Sep=''}.
% Separator "," generuje znak tab.
separator(Sep) --> whitespace, ",", whitespace, {Sep='\t'}.

lineno(N) --> integer_number(N).

% Dowolne napisy złożone ze znaków alfanumerycznych.
string_or_empty(C) --> "", {concat_atom([], C)}.
string_or_empty(C) --> string(C).
string(C) --> chars(C).
chars(C) --> char(C1), chars(Rest), {concat_atom([C1, Rest], C)}.
chars(C) --> char(C).
char(C) --> [C1], {code_type(C1, alnum), atom_codes(C, [C1])}.
alpha_char(C) --> [C], {code_type(C, alpha)}.

% Liczby
number(N) --> integer_number(N).
number(N) --> float_number(N).

% Liczby całkowite, złożone z jednej lub więcej cyfr.
integer_number(I) --> digit(I1), integer_number(Rest), {concat_atom([I1, Rest], I)}.
integer_number(I) --> digit(I).
digit(I) --> [I1], {code_type(I1, digit), atom_codes(I, [I1])}.

% Liczby float
float_number(I) --> digit(C), ".", integer_number(R), {concat_atom([C, '.', R], I)}.
float_number(I) --> digit(Ca), integer_number(Cb), ".", integer_number(R), {concat_atom([Ca, Cb, '.', R], I)}.

% Białe znaki.
whitespace --> " ", whitespace.
whitespace --> "\t", whitespace.
whitespace --> "".

% Znaki nowej linii.
newline --> "\n".
newline --> "\r".
newline --> "\r\n".
newline --> "\n\r".

% Unifikuje się z dowolnym ciągiem, ale nie może pochłaniać znaków nowej linii.
anything(A) --> [A1], anything(A3), {atom_codes(A2, [A1]), concat_atom([A2, A3], A)}.
anything(A) --> [A1], {atom_codes(A2, [A1]), concat_atom([A2], A)}.

let_or_not --> "let".
let_or_not --> "".

% --------------------------------------------------------------------------------
% Utilities
% --------------------------------------------------------------------------------

basic2c :-
    unix(argv([_|B])),
    find_file_arg(B, Arg),
    Arg = [File_name|_],
    access_file(File_name, 'read'),
    !,
    write(user_error, 'Podano nazwę pliku do wczytania: '), write(user_error, File_name), nl,
    read_file_to_codes(File_name, C, []),
    upper2lower(C, D),
    program(Z, D, []),
    writeln(Z),
    write(user_error, 'Kod programu jest poprawny!'), nl.

% Druga klauzula wywoływana w momencie, gdy podano błędny plik. Powoduje wyświetlenie błędu bez zapętlenia.
basic2c :-
    write('error'),
    !,
    fail.

% Konwersja wielkich liter do małych
checkChar(X, Y) :-
    upper(X),
    Y is X + 32.
checkChar(X, Y) :-
    not(upper(X)),
    Y is X.

upper2lower([], []).
upper2lower([X|T1], [Y|T2]) :-
    checkChar(X,Y),
    upper2lower(T1,T2).

upper(X):-
    X > 64,
    X < 91.

% Znak '--' oddziela argumenty Prologa od argumentów użytkownika.
% Jeżeli głową jest '--' to ogonem jest lista argumentów.
find_file_arg([], _) :-
    writeln('Nie znaleziono argumentu.').
find_file_arg(['--'|File], File).
find_file_arg([_|T], File_arg) :-
    find_file_arg(T, File_arg).

% vim:filetype=prolog
