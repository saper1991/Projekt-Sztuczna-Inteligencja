conc(C) --> cunary(C).
conc(C) --> cunary(C1), "&", conc(C2), {atomic_list_concat(['and(',C1,' ',C2,')'],'',C)}.

cunary(C) --> "(",conc(C),")".
cunary(C) --> "-",conc(C1),{atomic_list_concat(['not(',C1,')'],'',C)}.
cunary(C) --> catomic(C).

catomic(C) --> lcletter(C);lcletter(A),catomic(B),{concat(A,B,C)}.

lcletter(C) --> [L],{"a"=<L, L=<"z", char_code(C,L)}.

p(X) :- conc(C, X, []), write(C), nl.
