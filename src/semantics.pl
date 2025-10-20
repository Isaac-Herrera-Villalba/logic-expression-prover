% semantics.pl
:- module(semantics, [tautology/1, eval_formula/3]).
:- use_module(logic).

% ------------------------------
% Evaluador semántico de fórmulas
% ------------------------------

eval_formula(atom(X), Val, true)  :- member(X=true, Val), !.
eval_formula(atom(X), Val, false) :- member(X=false, Val), !.

eval_formula(neg(F), Val, true)  :- eval_formula(F, Val, false), !.
eval_formula(neg(F), Val, false) :- eval_formula(F, Val, true), !.

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

% ------------------------------
% tautología = true para todas las asignaciones posibles
tautology(F) :-
    logic:atoms(F, Atoms),
    findall(Assign, all_assignments(Atoms, Assign), All),
    forall(member(A, All), eval_formula(F, A, true)).

all_assignments([], []).
all_assignments([A|As], [A=true|R])  :- all_assignments(As, R).
all_assignments([A|As], [A=false|R]) :- all_assignments(As, R).
% ------------------------------

