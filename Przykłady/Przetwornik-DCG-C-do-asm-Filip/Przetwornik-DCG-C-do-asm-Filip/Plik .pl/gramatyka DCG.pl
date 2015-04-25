c_code --> functions, [int, main, '(' ], args, [')'], ['{'], code, [return], [0,';'] ,['}'].
c_code --> functions, [void, main, '(' ], args, [')'], ['{'], code ,['}'].

args --> [].
functions --> [].

code --> [].
code --> declaration.
code --> arithmetic.
code --> print_.
code --> scan.
code --> if.
%code --> for.
%code --> while.
%code --> do_while.

code --> declaration, code.
code --> arithmetic, code.
code --> print_, code.
code --> scan, code.
code --> if, code.
%code --> for, code.
%code --> while, code.
%code --> do_while, code.

declaration --> [int], [X], [';'],      {atom(X), asserta(var(int, X, nil))}.
declaration --> [int], [X], ['='], [Y], [';'],       {atom(X), integer(Y), asserta(var(int , X, Y))}.
declaration --> [char], [X], [';'],        {atom(X), asserta(var(char, X, nil))}.
declaration --> [char], [X], ['='], ['"'], [Y], ['"'], [';'], {atom(X), atom(Y), asserta(var(char, X, Y))}.
declaration --> [string], [X], [';'], {atom(X), asserta(var(string, X, nil))}.
declaration --> [string], [X], ['='], ['"'], [Y], ['"'], [';'], {atom(X), atom(Y), assert(var(string, X, Y))}.

arithmetic -->  [X, '='], arithm_exp, [';'], {var(_, X, _)}.

arithm_exp --> [X], arithm_exp_operator, [Y],  {var(A, X, _), var(A, Y, _)}.

arithm_exp_operator --> ['+'].
arithm_exp_operator --> ['-'].
arithm_exp_operator --> ['*'].
arithm_exp_operator --> ['/'].

print_ -->  [printf, '('], print_args1, print_args2, [')', ';'].

print_args1 --> ['"', X, '"'], {atom(X)}.

print_args2 --> [].
print_args2 --> [X], print_args2, {var(_, X, _)}.

scan --> [scanf, '('], scan_args, [')', ';'].

scan_args --> ['&', X], {var(_, X, _), not(var(_, _, nil))}.

if --> [if, '('], if_exp, [')', '{'], code, ['}'].
if --> [if, '('], if_exp, [')', '{'], code, ['}', else, '{'], code, ['}'].

if_exp --> [X], if_operator, [Y],  {var(A, X, _), var(A, Y, _)}.

if_operator --> ['=='].
if_operator --> ['>'].
if_operator --> ['<'].
if_operator --> ['<='].
if_operator --> ['>='].
if_operator --> ['!='].





















