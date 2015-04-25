%
%   Ewaluator wyrazan arytmetycznych zawierajacych 
%   liczby jednocyfrowe. Przyk³ad z "SICStus Prolog
%   User's Manual, Release 3.8.4, May 2000, s. 104.
%
%   Uruchomienie - przyk³adowy cel: 
%   expr(Val, "-2+3*5+1", []).
%

expr(Z) --> term(X), "+", expr(Y), {Z is X + Y}.
expr(Z) --> term(X), "-", expr(Y), {Z is X - Y}.
expr(X) --> term(X).
term(Z) --> number(X), "*", term(Y), {Z is X * Y}.
term(Z) --> number(X), "/", term(Y), {Z is X / Y}.
term(Z) --> number(Z).
number(C) --> "+", number(C).
number(C) --> "-", number(X), {C is -X}.
number(X) --> [C], {"0"=<C, C=<"9", X is C - "0"}.
