conc(C) --> lcletters(C).
conc(C) --> "(", spaces, "AND", spaces, conc(C1), spaces, conc(C2), spaces, ")", {concat(C1,'&',C11),concat(C11,C2,C)}.
conc(C) --> "(", spaces, "NOT", spaces,conc(C1), ")", {concat('-',C1,C)}.
lcletters(C) --> lcletter(C);lcletter(A),lcletters(B),{concat(A,B,C)}.
lcletter(C) --> [L],{"a"=<L, L=<"z", char_code(C,L)}.
spaces --> "";" ",spaces.

:- conc(C, "( AND  (AND creature ( NOT animal)) wise)", []), write(C), nl.
