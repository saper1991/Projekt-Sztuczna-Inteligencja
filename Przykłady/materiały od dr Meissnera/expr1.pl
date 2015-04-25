%
%   Ewaluator wyrazan arytmetycznych zawierajacych 
%   liczby wielocyfrowe. Zmodyfikowany przyk³ad z 
%   "SICStus Prolog User's Manual, Release 3.8.4, 
%   May 2000, s. 104.
%
%   Uruchomienie - przyk³adowy cel: 
%   expr(Val, "-2+30*5+1", []).
%

expr(Z) --> term(X), "+", expr(Y), {Z is X + Y}.
expr(Z) --> term(X), "-", expr(Y), {Z is X - Y}.
expr(X) --> term(X).
term(Z) --> number(X), "*", term(Y), {Z is X * Y}.
term(Z) --> number(X), "/", term(Y), {Z is X / Y}.
term(Z) --> number(Z).
number(C) --> "+", digits(C).
number(C) --> "-", digits(X), {C is -X}.
number(C) --> digits(C).
digits(D) --> digit(D);digit(A),digits(B), {number_codes(B,Cs),length(Cs,L), D is A*(10^L)+B}.
digit(D) --> [C], {"0"=<C, C=<"9", D is C - "0"}.




