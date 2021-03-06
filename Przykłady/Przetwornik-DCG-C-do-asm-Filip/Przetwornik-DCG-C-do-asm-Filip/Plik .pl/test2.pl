
 execute :-
        readFromFile("C:\\Users\\Filip\\Documents\\GitHub\\Przetwornik-DCG-C-do-asm\\Plik .pl\\input.txt", R),
        program(Z, R, []),
	write(Z), !.

readFromFile(File, Output) :-
                open(File, read, Temp, [encoding(utf8)]),
                read_stream_to_codes(Temp, Output),
                close(Temp).

%program(Z) --> code(Za), {concat_atom([Za], Z)}.

%tymczasowe wywolanei dla testów

%program(Z) --> add(Za), {concat_atom([Za], Z)}.

%program(Z) --> sub(Za), {concat_atom([Za], Z)}.

%program(Z) --> div(Za), {concat_atom([Za], Z)}.

%program(Z) --> mul(Za), {concat_atom([Za], Z)}.

%program(Z) --> inc(Za), {concat_atom([Za], Z)}.

%program(Z) --> dec(Za), {concat_atom([Za], Z)}.

%program(Z) --> if(Za), {concat_atom([Za], Z)}.

%program(Z) --> declaration(Za), {concat_atom([Za], Z)}.

%program(Z) --> printf(Z1,Z2), {concat_atom([Z1,Z2],Z)}.

%program(Z) --> main_printf(Za,Zb),{concat_atom([Za,Zb], Z)}.
%


printf(Za,Zb) --> "printf",whitespace,"(",whitespace,main_printf(C1,C2),whitespace,")",whitespace,";",{concat_atom([C1],Za),concat_atom([C2],Zb)}.


main_printf(Ca,Cb)-->"\"",whitespace,exp_to_printf(Z1),"\"",whitespace,",",whitespace,printf_char(Z2),whitespace,{concat_atom(['\nmsg: db ',' \"',Z1,'\",10,0'],Ca), concat_atom(['\nmov [',Z2,']',',','eax','\npush eax\npush dword msg\ncall _printf'],Cb)}.

main_printf(Ca,Cb)-->"\"",whitespace,chars(Z1),whitespace,"\"",whitespace,{concat_atom(['\nmsg: db ',' \"',Z1,'\",10,0'],Ca), concat_atom(['\npush dword msg\ncall _printf'],Cb)}.


printf_char(C) --> chars(C1),{concat_atom([C1],C)}.

exp_to_printf(C) --> chars(Z1),whitespace,"=",whitespace,"%d",whitespace,{concat_atom([Z1,' =',' %d'],C)}.




%Poniższe działa ok
program(Za) --> func(Z1,Z2,Z3),whitespace, func(Z4,Z5,Z6),whitespace, {concat_atom(['\nglobal _',Z1,'\nglobal _',Z4,'\nextern _printf','\nSECTION .data','\n',Z2,'\n',Z5,'\nSECTION .text\n','\n_',Z1,':','\npush ebp\nmov ebp,esp',Z3,'\nmov esp, ebp \npop ebp','\n_',Z4,':\npush ebp\nmov ebp,esp\n',Z6,'\nmov esp, ebp \npop ebp','\nint 0x80\nret'],Za)}
|func(Z1,Z2,Z3), whitespace,{concat_atom(['\nglobal _',Z1,'\nextern _printf','\nSECTION .data','\n',Z2,'\nSECTION .text\n_',Z1,':\n','\nmov esp, ebp \npop ebp',Z3,'\nmov esp, ebp \npop ebp','\nint 0x80\nret'],Za)}
|func(Z1,Z2,Z3),whitespace,{concat_atom([Z1,':\n',Z2,Z3],Za)}.


%operacje w funkcjach

func(Za,Zb,Zc) --> type_name,whitespace,chars(Z1),whitespace,"()",whitespace, "{", whitespace,declarations(Z2),whitespace,func_exp_s(Z3,Z4),whitespace,whitespace,"}", {concat_atom([Z1],Za),concat_atom([Z2],Zb),concat_atom([Z3,Z4],Zc)}
|type_name,whitespace,chars(Z1),whitespace,"()",whitespace, "{", whitespace,declarations(Z2),whitespace,func_exp_s(Z3,Z4),whitespace,whitespace,"}", {concat_atom([Z1],Za),concat_atom([Z2,Z3],Zb),concat_atom([Z4],Zc)}.

func(Za,Zb,Zc) --> type_name,whitespace,chars(Z1),whitespace,"()",whitespace, "{", whitespace,"}", {concat_atom([Z1], Za),concat_atom([''], Zb),concat_atom([''], Zc)}.

% func_exp(Z) --> declaration(Z1),whitespace, if(Z2),
% {concat_atom([Z1,Z2],Z)}.

% func_exp(Z) --> func_exp(Z1), whitespace, func_exp(Z2),
% {concat_atom([Z1,Z2],Z)}.

func_exp_s(Z1,Z2) --> func_exp(Za,Zb),whitespace, func_exp(Zc,Zd), {concat_atom([Za,Zc], Z1),concat_atom([Zb,Zd], Z2)}| func_exp(Za,Zb), {concat_atom([Zb],Z2),concat_atom([Za], Z1)}.

func_exp_s(Z1,Z2) --> exp(Za,Zb),whitespace, func_exp_s(Zc,Zd), {concat_atom([Za,Zc], Z1),concat_atom([Zb,Zd],Z2)}| func_exp(Za,Zb),
	{concat_atom([Zb],Z2),concat_atom([Za],Z1)}.



declarations(Z) --> declaration(Za), {concat_atom([Za], Z)}| declaration(Za), whitespace,declarations(Zb),whitespace,  {concat_atom([Za,Zb], Z)}.
% declarations(Z) --> declaration(Za),
% whitespace,declaration(Zb),whitespace, {concat_atom([Za,Zb], Z)}.

func_exp(Z1,Z2) --> if(Za), {concat_atom([Za], Z2), concat_atom([''],Z1)}.
func_exp(Z1,Z2) --> if(Za),whitespace,func_exp(Zb,Zc), {concat_atom([Za,Zc], Z2), concat_atom([Zb],Z1)}.

func_exp(Z1,Z2) --> exp(Za), {concat_atom([Za],Z2), concat_atom([''],Z1)}.
func_exp(Z1,Z2) --> exp(Za), func_exp(Zb,Zc), {concat_atom([Za,Zc],Z2), concat_atom([Zb],Z1)}.

func_exp(Z1,Z2) --> exp(Za), {concat_atom([Za],Z2), concat_atom([''],Z1)}.
func_exp(Z1,Z2) --> exp(Za), func_exp(Zb,Zc), {concat_atom([Za,Zc],Z2), concat_atom([Zb],Z1)}.

func_exp(Z1,Z2) --> exp(Za,Zb), {concat_atom([Za],Z1), concat_atom([Zb],Z2)}.
func_exp(Z1,Z2) --> exp(Za,Zb), func_exp(Zc,Zd), {concat_atom([Za,Zc],Z1), concat_atom([Zb,Zd],Z2)}.

exp(Za,Zb) --> printf(Z1,Z2), {concat_atom([Z1],Za), concat_atom([Z2],Zb)}.

func_execute(Z) --> chars(Za),"();",{concat_atom(['\ncall ',Za,'\n'],Z)}.

%printf

% printf_exp(Z1,Z2) --> cudzyslow,
% chars(Za),cudzyslow,",",chars(Zb),{concat_atom([Za],Z1),concat_atom([Zb],Z2)}.
%

%operacjearytmetyczne
%dodawanie

add(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,add_op,whitespace, chars(B),whitespace,";", {concat_atom(['\nmov eax, [',A,']\nadd eax, [',B,']\nmov [',C,'], eax\n'],Z)}.

add(Z) --> whitespace, chars(C), whitespace, "=",whitespace, integer_number(A),whitespace,add_op,whitespace, chars(B),whitespace,";", {concat_atom(['\nmov eax, [',A,']\nadd eax, [',B,']\nmov [',C,'], eax\n'],Z)}.

add(Z) --> whitespace, chars(C), whitespace, "=",whitespace, integer_number(A),whitespace,add_op,whitespace, integer_number(B),whitespace,";", {concat_atom(['\nmov eax, [',A,']\nadd eax, [',B,']\nmov [',C,'], eax\n'],Z)}.


sub(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,sub_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nsub eax, [',B,']\nmov [',C,'], eax\n'],Z)}.

div(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,div_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nmov edx,0\nidiv dword [',B,']\nmov [',C,'], eax\n'],Z)}.

mul(Z) --> whitespace, chars(C), whitespace, "=" , whitespace, chars(A),whitespace,mul_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nimul dword [',B,']\nmov [',C,'], eax\n'],Z)}.

inc(Z) --> whitespace, chars(A), whitespace, add_op, add_op, whitespace, {concat_atom(['mov eax, [',A,']\ninc eax'],Z)}.

dec(Z) --> whitespace, chars(A), whitespace, sub_op, sub_op, whitespace, {concat_atom(['mov eax, [',A,']\ndec eax'],Z)}.

equal(Z) --> whitespace, chars(A), whitespace, "=", whitespace, chars(B), ";", {concat_atom(['mov eax, [',A,']\nmov eax, [',B,']'],Z)}.

equal(Z) --> whitespace, chars(A), whitespace, "=", whitespace, integer_number(B), ";", {concat_atom(['mov eax, [',A,']\nmov eax, ',B],Z)}.

%instrukcje warunkowe
if(Z) --> "if",whitespace,"(",whitespace,if_cond(Za),whitespace,")",whitespace,"{",whitespace,exp_if(Zb),whitespace,"}", {concat_atom([Za,Zb],Z)}.
%zakładam, że możemy porównywać tylko liczby w postaci if(x>5)

%if(x>5)
if_cond(Z) --> chars(A),whitespace,cond_op_greater,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njg klamra'],Z)}.

%if(x<5)
if_cond(Z) --> chars(A),whitespace,cond_op_less,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njl klamra'],Z)}.

%if(x==5)
if_cond(Z) --> chars(A),whitespace,cond_op_equal_to,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njz klamra\n'],Z)}.


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


% exp to każde możliwe wyrażenie, które się moze pojawić, należałoby
% zdefiniować kilka(naście lub set) możliwości

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

%exp(Z) --> declarations(Za), {concat_atom([Za], Z)}.
%exp(Z) --> declaration(Za), exp(Zb), {concat_atom([Za,'\n',Zb], Z)}.

%wywoływanie funkcji - nie działa
exp(Z) --> func_execute(Za),{concat_atom([Za],Z)}.
exp(Z) --> func_execute(Za), exp(Zb), {concat_atom([Za,Zb],Z)}.



type_name --> "int".
type_name --> "char".
type_name --> "void".

declaration(Z) --> "int", whitespace, chars(A), whitespace, "=", whitespace, integer_number(B), whitespace,";",       {concat_atom(['\n',A,': dd ',B],Z)}.

declaration(Z) --> "char", whitespace, chars(A), whitespace, "=", whitespace, "'", char(B), "'", whitespace,";",       {concat_atom(['\n',A,': db ','"',B,'"'],Z)}.

declaration(Z) --> "char", whitespace, chars(A),"[]", whitespace, "=", whitespace, "'", string(B), "'", whitespace,";",       {concat_atom(['\n',A,': db ','"',B,'",0'],Z)}.
%declaration(Z)-->whitespace, {concat_atom([],Z)}.

% Białe znaki.
whitespace --> " ", whitespace.
whitespace --> "\t", whitespace.
whitespace --> "\n", whitespace.
whitespace --> "".

%liczby całkowite
integer_number(I) --> digit(I1), integer_number(Rest), {concat_atom([I1,Rest], I)}.
integer_number(I) --> digit(I).
digit(I) --> [I1], {code_type(I1, digit), atom_codes(I, [I1])}.

%ciągi znaków
string(C) --> chars(C).

chars(C) --> char(C1), chars(Rest), {concat_atom([C1, Rest], C)}.
chars(C) --> char(C).
char(C) --> [C1], {code_type(C1, alnum), atom_codes(C, [C1])}.
%alpha_char(C) --> [C], {code_type(C, alpha)}.

% c_code --> functions, [int, main, '(' ], args, [')'], ['{'], code,
% [return], [0,';'] ,['}'].
% c_code --> functions, [void, main, '(' ], args, [')'], ['{'], code
% ,['}'].

%args --> [].
%functions --> [].







