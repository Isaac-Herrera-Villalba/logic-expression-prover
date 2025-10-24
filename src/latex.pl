/*
 src/latex.pl
 ------------------------------------------------------------
 Descripción:

 Módulo encargado de generar reportes en formato LaTeX a partir
 de los resultados obtenidos en la evaluación de fórmulas y
 secuentes. Permite incluir tanto el estado lógico de cada
 expresión (válida, satisfacible, insatisfacible) como sus
 árboles de derivación o modelos semánticos.
 ------------------------------------------------------------
*/

:- module(latex, [
    start_latex_report/1,
    add_formula/3,
    add_formula/4,
    add_formula_status/4,
    add_sequent/3,
    add_sequent/4,
    finish_latex_report/1
]).

% Inicia el archivo LaTeX con su estructura base y encabezado
start_latex_report(File) :-
    open(File, write, Stream),
    format(Stream, '\\documentclass[12pt]{article}~n', []),
    format(Stream, '\\usepackage[utf8]{inputenc}~n', []),
    format(Stream, '\\usepackage{amsmath,amssymb,amsthm}~n', []),
    format(Stream, '\\usepackage{bussproofs}~n', []),
    format(Stream, '\\usepackage{geometry}~n', []),
    format(Stream, '\\geometry{margin=2.5cm}~n~n', []),
    format(Stream, '\\begin{document}~n', []),
    format(Stream, '\\section*{Resultados de las fórmulas lógicas y secuentes}~n~n', []),
    close(Stream).

% Agrega una fórmula al reporte (sin árbol)
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

% Agrega una fórmula con árbol de derivación
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

% Agrega una fórmula indicando su estado lógico
add_formula_status(File, Formula, valid, none) :-
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Válida (tautología)}~n~n', [Tex]),
    close(Stream).

add_formula_status(File, Formula, valid, Tree) :-
    Tree \= none,
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Válida (tautología)}~n', [Tex]),
    print_tree(Stream, Tree),
    format(Stream, '\\vspace{1em}~n', []),
    close(Stream).

add_formula_status(File, Formula, satisfacible(Model), _) :-
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    model_to_tex(Model, ModelTex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Satisfacible (contingente)}\\\\~n', [Tex]),
    format(Stream, '\\small Un modelo es: $~w$.~n~n', [ModelTex]),
    close(Stream).

add_formula_status(File, Formula, insatisfacible, _) :-
    open(File, append, Stream),
    formula_to_tex(Formula, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Insatisfacible (contradicción)}~n~n', [Tex]),
    close(Stream).

% Finaliza el documento LaTeX
finish_latex_report(File) :-
    open(File, append, Stream),
    format(Stream, '\\end{document}~n', []),
    close(Stream).

% Imprime un árbol de derivación (para fórmulas)
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

% Convierte una asignación de modelo a notación LaTeX
model_to_tex(Assign, Tex) :-
    maplist(assign_atom, Assign, Pairs),
    atomic_list_concat(Pairs, ', ', Body),
    format(atom(Tex), '\\{~w\\}', [Body]).

assign_atom(Var=true,  S) :- format(atom(S), '~w\\mapsto\\top', [Var]).
assign_atom(Var=false, S) :- format(atom(S), '~w\\mapsto\\bot', [Var]).

% Indentación para representar jerarquía visual de árboles
indent(Depth, Indent) :-
    Spaces is Depth * 6,
    length(L, Spaces),
    maplist(=(' '), L),
    atom_chars(Indent, L).

% Convierte estructuras Prolog a notación matemática LaTeX
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

% Agrega resultados de secuentes LKP al reporte
add_sequent(File, Sequent, true) :-
    open(File, append, Stream),
    sequent_to_tex(Sequent, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Derivable}~n~n', [Tex]),
    close(Stream).

add_sequent(File, Sequent, false) :-
    open(File, append, Stream),
    sequent_to_tex(Sequent, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{No derivable}~n~n', [Tex]),
    close(Stream).

add_sequent(File, Sequent, true, Tree) :-
    open(File, append, Stream),
    sequent_to_tex(Sequent, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{Derivable}~n', [Tex]),
    print_lkp_tree(Stream, Tree),
    format(Stream, '\\vspace{1em}~n', []),
    close(Stream).

add_sequent(File, Sequent, false, Tree) :-
    open(File, append, Stream),
    sequent_to_tex(Sequent, Tex),
    format(Stream, '\\textit{$~w$} $\\Rightarrow$ \\textbf{No derivable}~n', [Tex]),
    print_tree(Stream, Tree),
    format(Stream, '\\vspace{1em}~n', []),
    close(Stream).

% Convierte un secuente a notación LaTeX (Γ ⊢ Δ)
sequent_to_tex(seq(G, D), T) :-
    maplist(formula_to_tex, G, GL),
    maplist(formula_to_tex, D, DL),
    atomic_list_concat(GL, ', ', Gamma),
    atomic_list_concat(DL, ', ', Delta),
    format(atom(T), '(~w \\vdash ~w)', [Gamma, Delta]).

% Imprime el árbol de derivación para secuentes (LKP)
print_lkp_tree(Stream, Proof) :-
    format(Stream, '\\begin{center}\\renewcommand{\\arraystretch}{1.1}~n', []),
    print_lkp_level(Stream, Proof, 0),
    format(Stream, '\\end{center}~n', []).

print_lkp_level(Stream, node(_Rule, Seq, []), Depth) :-
    indent(Depth, Indent),
    sequent_to_tex(Seq, TexS),
    format(Stream, '{~w\\underline{$~w$}}\\\\~n', [Indent, TexS]).

print_lkp_level(Stream, node(Rule, Seq, Subs), Depth) :-
    NewDepth is Depth + 1,
    forall(member(Sub, Subs),
        print_lkp_level(Stream, Sub, NewDepth)
    ),
    indent(Depth, Indent),
    sequent_to_tex(Seq, TexS),
    format(atom(RuleLabel), ' \\;\\scriptsize(\\textsf{~w})', [Rule]),
    format(Stream, '{~w\\underline{$~w$}~w}\\\\~n', [Indent, TexS, RuleLabel]).

