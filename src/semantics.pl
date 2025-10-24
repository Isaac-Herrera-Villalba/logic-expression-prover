/*
 src/semantics.pl
 ------------------------------------------------------------
 Descripción:

 Módulo de evaluación semántica para fórmulas proposicionales.
 Permite determinar si una fórmula es válida (tautología),
 satisfacible o insatisfacible, y generar todas las posibles
 asignaciones de valores de verdad para sus átomos.
 ------------------------------------------------------------
*/

:- module(semantics, [tautology/1, eval_formula/3, satisfiable/2, unsatisfiable/1]).
:- use_module(logic).

% Evalúa una fórmula bajo una asignación de valores de verdad
eval_formula(atom(X), Val, V) :-
    member(X=V, Val), !.

eval_formula(neg(F), Val, true)  :-
    eval_formula(F, Val, false), !.
eval_formula(neg(F), Val, false) :-
    eval_formula(F, Val, true), !.

eval_formula(and(A,B), Val, true)  :-
    eval_formula(A, Val, true),
    eval_formula(B, Val, true), !.
eval_formula(and(_, _), _, false).

eval_formula(or(A,B), Val, true)  :-
    (eval_formula(A, Val, true) ; eval_formula(B, Val, true)), !.
eval_formula(or(_, _), _, false).

eval_formula(implies(A,B), Val, true) :-
    (eval_formula(A, Val, false)
    ; eval_formula(B, Val, true)), !.
eval_formula(implies(_, _), _, false).

eval_formula(dimplies(A,B), Val, true) :-
    eval_formula(A, Val, VA),
    eval_formula(B, Val, VB),
    VA = VB, !.
eval_formula(dimplies(_, _), _, false).

% Determina si una fórmula es tautología (verdadera en todas las asignaciones)
tautology(F) :-
    logic:atoms(F, Atoms),
    findall(Assign, all_assignments(Atoms, Assign), All),
    forall(member(A, All), eval_formula(F, A, true)).

% Determina si una fórmula es satisfacible (verdadera en al menos una asignación)
satisfiable(F, Model) :-
    logic:atoms(F, Atoms),
    all_assignments(Atoms, Model),
    eval_formula(F, Model, true), !.

% Determina si una fórmula es insatisfacible (falsa en todas las asignaciones)
unsatisfiable(F) :-
    \+ satisfiable(F, _).

% Genera todas las posibles asignaciones de valores de verdad para los átomos
all_assignments([], []).
all_assignments([A|As], [A=true|R])  :- all_assignments(As, R).
all_assignments([A|As], [A=false|R]) :- all_assignments(As, R).

