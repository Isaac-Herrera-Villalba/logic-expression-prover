/*
 src/latex.pl
 ------------------------------------------------------------
 Módulo para generar reportes en LaTeX con fórmulas y derivaciones lógicas.
 Genera un documento con encabezado, resultados y árboles de derivación.
 ------------------------------------------------------------
*/

:- module(latex, [
    start_latex_report/1,
    add_formula/3,
    add_formula/4,
    finish_latex_report/1
]).

% ------------------------------------------------------------
% Inicia el archivo LaTeX con encabezado
% ------------------------------------------------------------
start_latex_report(File) :-
    open(File, write, Stream),
    format(Stream, '\\documentclass[12pt]{article}~n', []),
    format(Stream, '\\usepackage[utf8]{inputenc}~n', []),
    format(Stream, '\\usepackage{amsmath,amssymb,amsthm}~n', []),
    format(Stream, '\\usepackage{bussproofs}~n', []),
    format(Stream, '\\usepackage{geometry}~n', []),
    format(Stream, '\\geometry{margin=2.5cm}~n~n', []),
    format(Stream, '\\begin{document}~n', []),
    format(Stream, '\\section*{Resultados de las fórmulas lógicas}~n~n', []),
    close(Stream).

% ------------------------------------------------------------
% Agrega una fórmula al reporte (sin árbol)
% ------------------------------------------------------------
add_formula(File, Formula, true) :-
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Verdadero}~n~n', [Tex]),
    close(Stream).

add_formula(File, Formula, false) :-
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Falso}~n~n', [Tex]),
    close(Stream).

% ------------------------------------------------------------
% Agrega una fórmula al reporte con árbol de derivación
% ------------------------------------------------------------
add_formula(File, Formula, true, Tree) :-
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Verdadero}~n', [Tex]),
    print_tree(Stream, Tree),
    format(Stream, '\\vspace{1em}~n', []),
    close(Stream).

add_formula(File, Formula, false, Tree) :-
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Falso}~n', [Tex]),
    print_tree(Stream, Tree),
    format(Stream, '\\vspace{1em}~n', []),
    close(Stream).

% ------------------------------------------------------------
% Finaliza el documento LaTeX
% ------------------------------------------------------------
finish_latex_report(File) :-
    open(File, append, Stream),
    format(Stream, '\\end{document}~n', []),
    close(Stream).

% ------------------------------------------------------------
% Imprime árbol de derivación en formato escalonado con "⊢"
% ------------------------------------------------------------
print_tree(Stream, Tree) :-
    format(Stream, '\\begin{center}\\renewcommand{\\arraystretch}{1.1}~n', []),
    print_tree_level(Stream, Tree, 0),
    format(Stream, '\\end{center}~n', []).

print_tree_level(Stream, tree(F, []), _) :-
    formula_to_tex(F, TexF),
    format(Stream, '\\underline{$~w$}\\\\~n', [TexF]).
print_tree_level(Stream, tree(F, Subs), Depth) :-
    NewDepth is Depth + 1,
    forall(member(Sub, Subs),
        print_tree_level(Stream, Sub, NewDepth)
    ),
    indent(Depth, Indent),
    formula_to_tex(F, TexF),
    (Subs \= [] ->
        format(Stream, '{~w\\underline{$\\vdash~w$}}\\\\~n', [Indent, TexF])
    ;
        format(Stream, '{~w\\underline{$~w$}}\\\\~n', [Indent, TexF])
    ).

% ------------------------------------------------------------
% Añade espacios para centrar visualmente las fórmulas (pirámide)
% ------------------------------------------------------------
indent(Depth, Indent) :-
    Spaces is Depth * 6,   % ajusta este número si quieres más o menos desplazamiento
    length(L, Spaces),
    maplist(=(' '), L),
    atom_chars(Indent, L).

% ------------------------------------------------------------
% Convierte los predicados Prolog en notación matemática LaTeX
% ------------------------------------------------------------
formula_to_tex(atom(X), T) :- format(atom(T), '~w', [X]).
formula_to_tex(neg(X), T) :-
    formula_to_tex(X, TX),
    format(atom(T), '(\\neg ~w)', [TX]).

formula_to_tex(and(A,B), T) :-
    formula_to_tex(A, TA),
    formula_to_tex(B, TB),
    format(atom(T), '(~w \\wedge ~w)', [TA, TB]).
formula_to_tex(or(A,B), T) :-
    formula_to_tex(A, TA),
    formula_to_tex(B, TB),
    format(atom(T), '(~w \\vee ~w)', [TA, TB]).
formula_to_tex(implies(A,B), T) :-
    formula_to_tex(A, TA),
    formula_to_tex(B, TB),
    format(atom(T), '(~w \\rightarrow ~w)', [TA, TB]).
formula_to_tex(dimplies(A,B), T) :-
    formula_to_tex(A, TA),
    formula_to_tex(B, TB),
    format(atom(T), '(~w \\leftrightarrow ~w)', [TA, TB]).
formula_to_tex(X, T) :- format(atom(T), '~w', [X]).
