
 execute :-
        readFromFile("D:\\Projekty\\Przetwornik-DCG-C-do-asm-Filip\\Plik .pl\\input2.txt", R),
		print('org 100h'),
        program(Z, R, []),
	write(Z), !.

readFromFile(File, Output) :-
                open(File, read, Temp, [encoding(utf8)]),
                read_stream_to_codes(Temp, Output),
                close(Temp).

%program(Z) --> code(Za), {concat_atom([Za], Z)}.

%tymczasowe wywolanei dla test�w

%program(Z) --> add(Za), {concat_atom([Za], Z)}.

%program(Z) --> sub(Za), {concat_atom([Za], Z)}.

%program(Z) --> div(Za), {concat_atom([Za], Z)}.

%program(Z) --> mul(Za), {concat_atom([Za], Z)}.

%program(Z) --> inc(Za), {concat_atom([Za], Z)}.

%program(Z) --> dec(Za), {concat_atom([Za], Z)}.

%program(Z) --> if(Za), {concat_atom([Za], Z)}.

%program(Z) --> while(Za),  {concat_atom([Za], Z)}.

%program(Z) --> dowhile(Za), {concat_atom([Za], Z)}.

%program(Z) --> declaration(Za), {concat_atom([Za], Z)}.

program(Z) --> func(Z1),whitespace, func(Z2), {concat_atom([Z1,Z2],Z)}.

program(Z) --> func(Z1), {concat_atom([Z1],Z)}.


%operacje w funkcjach

func(Z) --> type_name,whitespace,chars(Z1),whitespace,"()",whitespace, "{", whitespace, func_exp_s(Z2),whitespace,"}", {concat_atom(['\n',Z1,':\n', Z2], Z)}.

% func_exp(Z) --> declaration(Z1),whitespace, if(Z2),
% {concat_atom([Z1,Z2],Z)}.

% func_exp(Z) --> func_exp(Z1), whitespace, func_exp(Z2),
% {concat_atom([Z1,Z2],Z)}.

func_exp_s(Z) --> func_exp(Za),whitespace, func_exp(Zb), {concat_atom([Za,Zb], Z)}.

func_exp_s(Z) --> func_exp(Za), {concat_atom([Za],Z)}.

func_exp(Z) --> declaration(Za), {concat_atom([Za], Z)}.
func_exp(Z) --> declaration(Za), whitespace, func_exp(Zb),  {concat_atom([Za,Zb], Z)}.

func_exp(Z) --> if(Za), {concat_atom([Za], Z)}.
func_exp(Z) --> if(Za),whitespace,func_exp(Zb), {concat_atom([Za,Zb], Z)}.

func_exp(Z) --> while(Za), {concat_atom([Za], Z)}.
func_exp(Z) --> while(Za), whitespace, func_exp(Z), {concat_atom([Za], Z)}.

func_exp(Z) --> for(Za), {concat_atom([Za], Z)}.
func_exp(Z) --> for(Za), whitespace, func_exp(Z), {concat_atom([Za], Z)}.

func_exp(Z) --> dowhile(Za), {concat_atom([Za], Z)}.
func_exp(Z) --> dowhile(Za), whitespace, func_exp(Z), {concat_atom([Za], Z)}.


func_exp(Z) --> exp(Za), {concat_atom([Za],Z)}.
func_exp(Z) --> exp(Za), func_exp(Zb), {concat_atom([Za,Zb],Z)}.
func_execute(Z) --> chars(Za),"();",{concat_atom(['\ncall ',Za,'\n'],Z)}.

%operacjearytmetyczne
%dodawanie

add(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,add_op,whitespace, chars(B),whitespace,";", {concat_atom(['\nmov eax, [',A,']\nadd eax, ',B,'\nmov [',C,'], eax\n'],Z)}.

add(Z) --> whitespace, chars(C), whitespace, "=",whitespace, integer_number(A),whitespace,add_op,whitespace, chars(B),whitespace,";", {concat_atom(['\nmov eax, [',A,']\nadd eax, ',B,'\nmov [',C,'], eax\n'],Z)}.

add(Z) --> whitespace, chars(C), whitespace, "=",whitespace, integer_number(A),whitespace,add_op,whitespace, integer_number(B),whitespace,";", {concat_atom(['\nmov eax, [',A,']\nadd eax, ',B,'\nmov [',C,'], eax\n'],Z)}.


sub(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,sub_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nsub eax, ',B,'\nmov [',C,'], eax\n'],Z)}.

div(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,div_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nmov edx,0\nidiv dword ',B,'\nmov [',C,'], eax\n'],Z)}.

mul(Z) --> whitespace, chars(C), whitespace, "=" , whitespace, chars(A),whitespace,mul_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nimul dword [',B,']\nmov [',C,'], eax\n'],Z)}.

inc(Z) --> whitespace, chars(A), whitespace, add_op, add_op, whitespace, {concat_atom(['mov eax, [',A,']\ninc eax\nmov [', A, '], eax'],Z)}.

dec(Z) --> whitespace, chars(A), whitespace, sub_op, sub_op, whitespace, {concat_atom(['mov eax, [',A,']\ndec eax'],Z)}.

equal(Z) --> whitespace, chars(A), whitespace, "=", whitespace, chars(B), ";", {concat_atom(['mov eax, [',A,']\nmov eax, [',B,']'],Z)}.

equal(Z) --> whitespace, chars(A), whitespace, "=", whitespace, integer_number(B), ";", {concat_atom(['mov eax, [',A,']\nmov eax, ',B],Z)}.

%instrukcje warunkowe
if(Z) --> "if",whitespace,"(",whitespace,if_cond(Za),whitespace,")",whitespace,"{",whitespace,exp_if(Zb),whitespace,"}", {concat_atom([Za,Zb],Z)}.
%zak�adam, �e mo�emy por�wnywa� tylko liczby w postaci if(x>5)

%if(x>5)
if_cond(Z) --> chars(A),whitespace,cond_op_greater,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njg klamra'],Z)}.

%if(x<5)
if_cond(Z) --> chars(A),whitespace,cond_op_less,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njl klamra'],Z)}.

%if(x==5)
if_cond(Z) --> chars(A),whitespace,cond_op_equal_to,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njz klamra\n'],Z)}.

while(Z) --> "while", whitespace, "(",whitespace, chars(A), whitespace, cond_op_equal_to,whitespace, integer_number(B), whitespace, ")", whitespace, "{", whitespace, loop_exp(Za), whitespace, "}", {concat_atom(['\nwhloop:\nmov eax, [', A, ']\ncmp eax, ', B, '\njne whendloop\n', Za, '\njmp whloop\nwh_endloop:'], Z)}.

while(Z) --> "while", whitespace, "(",whitespace, chars(A), whitespace, cond_op_not_equal_to,whitespace, integer_number(B), whitespace, ")", whitespace, "{", whitespace, loop_exp(Za), whitespace, "}", {concat_atom(['\nwhloop:\nmov eax, [', A, ']\ncmp eax, ', B, '\nje whendloop\n', Za, '\njmp whloop\nwh_endloop:'], Z)}.

while(Z) --> "while", whitespace, "(",whitespace, chars(A), whitespace, cond_op_less,whitespace, integer_number(B), whitespace, ")", whitespace, "{", whitespace, loop_exp(Za), whitespace, "}", {concat_atom(['\nwhloop:\nmov eax, [', A, ']\ncmp eax, ', B, '\njnl whendloop\n', Za, '\njmp whloop\nwhendloop:'], Z)}.

while(Z) --> "while", whitespace, "(",whitespace, chars(A), whitespace, cond_op_greater,whitespace, integer_number(B), whitespace, ")", whitespace, "{", whitespace, loop_exp(Za), whitespace, "}", {concat_atom(['\nwhloop:\nmov eax, [', A, ']\ncmp eax, ', B, '\njng whendloop\n', Za, '\njmp whloop\nwhendloop:'], Z)}.

while(Z) --> "while", whitespace, "(", whitespace, "true", whitespace, ")", whitespace, "{", whitespace, loop_exp(Za), whitespace, "}", {concat_atom(['\nwh:', Za, '\n jmp wh'], Z)}.



%p�tla do-while
dowhile(Z) --> "do", whitespace, "{", whitespace, loop_exp(Za), whitespace, "}", whitespace, "while", whitespace, "(", whitespace, dwcond(Zb), whitespace, ")", whitespace, ";", {concat_atom(['\ndowhloop:\n', Za, Zb], Z)}.

dwcond(Z) --> chars(A), whitespace, cond_op_equal_to, whitespace, integer_number(B), {concat_atom(['mov eax, [', A, ']\ncmp eax, ', B, '\n je dowhloop'], Z)}.

dwcond(Z) --> chars(A), whitespace, cond_op_not_equal_to, whitespace, integer_number(B), {concat_atom(['mov eax, [', A, ']\ncmp eax, ', B, '\n jne dowhloop'], Z)}.

dwcond(Z) --> chars(A), whitespace, cond_op_less, whitespace, integer_number(B), {concat_atom(['mov eax, [', A, ']\ncmp eax, ', B, '\n jl dowhloop'], Z)}.

dwcond(Z) --> chars(A), whitespace, cond_op_greater, whitespace, integer_number(B), {concat_atom(['mov eax, [', A, ']\ncmp eax, ', B, '\n jg dowhloop'], Z)}.

%petla for
for(Z) --> "for", whitespace, "(", whitespace, for1cond(Za), whitespace, ";", whitespace, for2cond(Zb), whitespace, ";", whitespace, for3cond(Zc), whitespace, ")", whitespace, "{", whitespace, loop_exp(Zd), whitespace, "}", {concat_atom([Za, '\nfor:\n', Zd, Zc, Zb], Z)}.

for1cond(Z) --> chars(A), whitespace, "=", whitespace, integer_number(B), {concat_atom(['\nmov eax, [', A, ']\nmov eax, ', B, '\nmov [i], eax'], Z)}.

for2cond(Z) --> chars(A), whitespace, cond_op_equal_to, whitespace, integer_number(Y), {concat_atom(['\nmov eax, [', A, ']\ncmp eax,', Y, '\nje for'], Z)}.
for2cond(Z) --> chars(A), whitespace, cond_op_less, whitespace, integer_number(Y), {concat_atom(['\nmov eax, [', A, ']\ncmp eax,', Y, '\njl for'], Z)}.
for2cond(Z) --> chars(A), whitespace, cond_op_greater, whitespace, integer_number(Y), {concat_atom(['\nmov eax, [', A, ']\ncmp eax,', Y, '\njg for'], Z)}.

for3cond(Z) --> chars(A), whitespace, "=", whitespace, chars(A), whitespace, "+", whitespace, integer_number(C), {concat_atom(['\nmov eax, [', A, ']\nadd eax, ', C, '\nmov [', A, '], eax'], Z)}.

loop_exp(Z) --> exp(Za), whitespace, exp(Zb), {concat_atom([Za, Zb], Z)}.
loop_exp(Z) --> exp(Za), {concat_atom([Za], Z)}.


%operatory arytmetyczne
add_op --> "+".
sub_op--> "-".
div_op --> "/".
mul_op --> "*".

%operatory warunkowe
cond_op_greater --> ">".
%cond_op_greater_or_equal --> ">=".
cond_op_less --> "<".
%cond_op_less_or_equal --> "<=".
cond_op_equal_to --> "==".

cond_op_not_equal_to --> "!=".

% exp to ka�de mo�liwe wyra�enie, kt�re si� moze pojawi�, nale�a�oby
% zdefiniowa� kilka(na�cie lub set) mo�liwo�ci

exp_if(Z) --> exp(Za),whitespace, exp(Zb), {concat_atom(['\n\nklamra:\n',Za,'\n',Zb],Z)}.

exp_if(Z) --> exp(Za), {concat_atom(['\n\nklamra:\n',Za],Z)}.

%exp(Z) --> exp(Zb), whitespace, {concat_atom([Zb],Z)}.
exp(Z) --> add(Za), {concat_atom([Za], Z)}.
exp(Z) --> add(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> sub(Za), {concat_atom([Za], Z)}.
exp(Z) --> sub(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> inc(Za), {concat_atom([Za], Z)}.
exp(Z) --> inc(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> dec(Za), {concat_atom([Za], Z)}.
exp(Z) --> dec(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> div(Za), {concat_atom([Za], Z)}.
exp(Z) --> div(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> mul(Za), {concat_atom([Za], Z)}.
exp(Z) --> mul(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> equal(Za), {concat_atom([Za], Z)}.
exp(Z) --> equal(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> declaration(Za), {concat_atom([Za], Z)}.
exp(Z) --> declaration(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

exp(Z) --> while(Za), exp(Zb), {concat_atom([Za, Zb], Z)}.
exp(Z) --> while(Za), {concat_atom([Za], Z)}.

exp(Z) --> dowhile(Za), exp(Zb), {concat_atom([Za, Zb], Z)}.
exp(Z) --> dowhile(Za), {concat_atom([Za], Z)}.

exp(Z) --> for(Za), exp(Zb), {concat_atom([Za, Zb], Z)}.
exp(Z) --> for(Za), {concat_atom([Za], Z)}.

exp(Z) --> if(Za), exp(Zb), {concat_atom([Za, Zb], Z)}.
exp(Z) --> if(Za), {concat_atom([Za], Z)}.

%wywo�ywanie funkcji - nie dzia�a
exp(Z) --> func_execute(Za),{concat_atom([Za],Z)}.
exp(Z) --> func_execute(Za), exp(Zb), {concat_atom([Za,Zb],Z)}.



type_name --> "int".
type_name --> "char".
type_name --> "void".

declaration(Z) --> "int", whitespace, chars(A), whitespace, "=", whitespace, integer_number(B), whitespace,";",       {concat_atom(['\n',A,': dd ',B],Z)}.

declaration(Z) --> "char", whitespace, chars(A), whitespace, "=", whitespace, "'", char(B), "'", whitespace,";",       {concat_atom(['\n',A,': db ','"',B,'"'],Z)}.

declaration(Z) --> "char", whitespace, chars(A),"[]", whitespace, "=", whitespace, "'", string(B), "'", whitespace,";",       {concat_atom(['\n',A,': db ','"',B,'",0'],Z)}.

% Bia�e znaki.
whitespace --> " ", whitespace.
whitespace --> "\t", whitespace.
whitespace --> "\n", whitespace.
whitespace --> "".

%liczby ca�kowite
integer_number(I) --> digit(I1), integer_number(Rest), {concat_atom([I1,Rest], I)}.
integer_number(I) --> digit(I).
digit(I) --> [I1], {code_type(I1, digit), atom_codes(I, [I1])}.

%ci�gi znak�w
string(C) --> chars(C).

chars(C) --> char(C1), chars(Rest), {concat_atom([C1, Rest], C)}.
chars(C) --> char(C).
char(C) --> [C1], {code_type(C1, alnum), atom_codes(C, [C1])}.







