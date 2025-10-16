% latex.pl
% Convert proof tree into LaTeX using bussproofs
:- module(latex, [proof_to_latex_file/2, sequent_to_math/2]).
:- use_module(library(lists)).

sequent_to_math(sequent(L,R), Math) :-
    maplist(formula_to_tex, L, LT),
    maplist(formula_to_tex, R, RT),
    atomic_list_concat(LT, ', ', Ls),
    atomic_list_concat(RT, ', ', Rs),
    format(atom(Math), "\\ensuremath{~w \\vdash ~w}", [Ls, Rs]).

formula_to_tex(atom(A), T) :- format(atom(T), "~w", [A]).
formula_to_tex(neg(F), T) :- formula_to_tex(F, TF), format(atom(T), "\\neg ~w", [TF]).
formula_to_tex(and(A,B), T) :- formula_to_tex(A, TA), formula_to_tex(B, TB), format(atom(T), "(~w \\land ~w)", [TA, TB]).
formula_to_tex(or(A,B), T) :- formula_to_tex(A, TA), formula_to_tex(B, TB), format(atom(T), "(~w \\lor ~w)", [TA, TB]).
formula_to_tex(implies(A,B), T) :- formula_to_tex(A, TA), formula_to_tex(B, TB), format(atom(T), "(~w \\to ~w)", [TA, TB]).
formula_to_tex(dimplies(A,B), T) :- formula_to_tex(A, TA), formula_to_tex(B, TB), format(atom(T), "(~w \\leftrightarrow ~w)", [TA, TB]).

% Convert proof tree into bussproofs prooftree commands
proof_to_buss(P, Lines) :-
    P = proof(Sequent, Rule, Subs),
    sequent_to_math(Sequent, SMath),
    ( Subs = [] ->
        format(atom(L), "\\AxiomC{~w}", [SMath]),
        Lines = [L]
    ; Subs = [S] ->
        proof_to_buss(S, Ls),
        atomic_list_concat(Ls, '\n', SubTex),
        format(atom(L), "~w\n\\UnaryInfC{~w}", [SubTex, SMath]),
        Lines = [L]
    ; Subs = [S1,S2] ->
        proof_to_buss(S1, L1), proof_to_buss(S2, L2),
        atomic_list_concat(L1, '\n', T1), atomic_list_concat(L2, '\n', T2),
        format(atom(L), "~w\n~w\n\\BinaryInfC{~w}", [T1, T2, SMath]),
        Lines = [L]
    ; % more subs - general (left to right)
        maplist(proof_to_buss, Subs, Many),
        atomic_list_concat(Many, '\n', M),
        format(atom(L), "~w\n\\TrinaryInfC{~w}", [M, SMath]),
        Lines = [L]
    ).

proof_to_latex_file(Proof, Path) :-
    open(Path, write, S),
    write(S, "\\documentclass{article}\n"),
    write(S, "\\usepackage{bussproofs}\n"),
    write(S, "\\usepackage{amssymb}\n"),
    write(S, "\\begin{document}\n"),
    write(S, "\\begin{center}\n"),
    proof_to_buss(Proof, Lines),
    atomic_list_concat(Lines, '\n', Body),
    format(S, "\\begin{prooftree}\n~w\n\\end{prooftree}\n", [Body]),
    write(S, "\\end{center}\n"),
    write(S, "\\end{document}\n"),
    close(S).

