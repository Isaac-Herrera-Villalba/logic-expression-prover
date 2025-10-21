/*
latex.pl
------------------------------------------------------------
Convert proof tree into LaTeX using bussproofs
*/

:- module(latex, [
    start_latex_report/1,
    add_formula/3,
    finish_latex_report/1
]).

symbol(and, '\\land').
symbol(or, '\\lor').
symbol(neg, '\\lnot').
symbol(implies, '\\rightarrow').
symbol(dimplies, '\\leftrightarrow').

start_latex_report(Path) :-
    open(Path, write, S),
    write(S, '\\documentclass[12pt]{article}\n'),
    write(S, '\\usepackage[utf8]{inputenc}\n'),
    write(S, '\\usepackage{amsmath,amssymb}\n'),
    write(S, '\\begin{document}\n'),
    write(S, '\\section*{Resultados de las fórmulas lógicas}\n'),
    close(S).

add_formula(Path, Formula, true) :-
    open(Path, append, S),
    write(S, '\\noindent\n'),
    write(S, '\\( '), write_formula(S, Formula), write(S, ' \\)'),
    write(S, ' \\;\\Rightarrow\\; \\textbf{Verdadero}\\\\[6pt]\n'),
    close(S).
add_formula(Path, Formula, false) :-
    open(Path, append, S),
    write(S, '\\noindent\n'),
    write(S, '\\( '), write_formula(S, Formula), write(S, ' \\)'),
    write(S, ' \\;\\Rightarrow\\; \\textbf{Falso}\\\\[6pt]\n'),
    close(S).

finish_latex_report(Path) :-
    open(Path, append, S),
    write(S, '\\end{document}\n'),
    close(S).

write_formula(S, atom(X)) :- write(S, X).
write_formula(S, neg(F)) :-
    symbol(neg, Sym),
    format(S, '~w ', [Sym]),
    write_formula(S, F).
write_formula(S, and(A,B)) :-
    write(S, '('),
    write_formula(S, A),
    symbol(and, Sym),
    format(S, ' ~w ', [Sym]),
    write_formula(S, B),
    write(S, ')').
write_formula(S, or(A,B)) :-
    write(S, '('),
    write_formula(S, A),
    symbol(or, Sym),
    format(S, ' ~w ', [Sym]),
    write_formula(S, B),
    write(S, ')').
write_formula(S, implies(A,B)) :-
    write(S, '('),
    write_formula(S, A),
    symbol(implies, Sym),
    format(S, ' ~w ', [Sym]),
    write_formula(S, B),
    write(S, ')').
write_formula(S, dimplies(A,B)) :-
    write(S, '('),
    write_formula(S, A),
    symbol(dimplies, Sym),
    format(S, ' ~w ', [Sym]),
    write_formula(S, B),
    write(S, ')').
% ------------------------------------------------------------

