/*
 /src/prover.pl
--------------------------------------------------
Módulo de razonamiento lógico (Prover)
--------------------------------------------------
*/


% Aquí van todas las definiciones de prove/1 y derivation/2
:- module(prover, [prove/1, derivation/2]).
:- use_module('src/semantics.pl').

/*
--------------------------------------------------
Declaración de operadores lógicos
--------------------------------------------------
*/

:- op(1, fx,  neg).
:- op(2, xfy,  and).
:- op(2, xfy,  or).
:- op(2, xfy,  implies).
:- op(2, xfy,  dimplies).

/*
--------------------------------------------------
Probar una expresión booleana bajo todas las interpretaciones
--------------------------------------------------
*/
prove(Expr) :-
    get_atoms(Expr, Atoms),
    all_interpretations(Atoms, Interps),
    forall(member(Interp, Interps),
           ( eval(Expr, Interp, true) )).

/*
--------------------------------------------------
Obtención de átomos (variables lógicas)
--------------------------------------------------
*/
get_atoms(Expr, Atoms) :-
    get_atoms(Expr, [], Raw),
    sort(Raw, Atoms).

get_atoms(neg(A), Acc, Out) :- !, get_atoms(A, Acc, Out).
get_atoms(A and B, Acc, Out) :- !, get_atoms(A, Acc, T1), get_atoms(B, T1, Out).
get_atoms(A or B, Acc, Out) :- !, get_atoms(A, Acc, T1), get_atoms(B, T1, Out).
get_atoms(A implies B, Acc, Out) :- !, get_atoms(A, Acc, T1), get_atoms(B, T1, Out).
get_atoms(A dimplies B, Acc, Out) :- !, get_atoms(A, Acc, T1), get_atoms(B, T1, Out).
get_atoms(A, Acc, [A | Acc]) :- atom(A).

/*
--------------------------------------------------
Generar todas las interpretaciones posibles
--------------------------------------------------
*/
all_interpretations([], [[]]).
all_interpretations([A | As], Interps) :-
    all_interpretations(As, Rest),
    findall([(A, true) | R], member(R, Rest), T1),
    findall([(A, false) | R], member(R, Rest), T2),
    append(T1, T2, Interps).
% --------------------------------------------------

% derivation(F, Tree)
% Tree representa el árbol de evaluación de F
derivation(atom(A), tree(atom(A), [])).
derivation(neg(F), tree(neg(F), [Sub])) :-
    derivation(F, Sub).
derivation(and(A,B), tree(and(A,B), [SubA, SubB])) :-
    derivation(A, SubA),
    derivation(B, SubB).
derivation(or(A,B), tree(or(A,B), [SubA, SubB])) :-
    derivation(A, SubA),
    derivation(B, SubB).
derivation(implies(A,B), tree(implies(A,B), [SubA, SubB])) :-
    derivation(A, SubA),
    derivation(B, SubB).
derivation(dimplies(A,B), tree(dimplies(A,B), [SubA, SubB])) :-
    derivation(A, SubA),
    derivation(B, SubB).

% derivation(+Formula, -Tree)
% Genera un árbol de derivación simple
derivation(F, tree(F, Subs)) :-
    F =.. [Op,A,B],
    member(Op,[and,or,implies,dimplies]),
    derivation(A, TA),
    derivation(B, TB),
    Subs = [TA, TB].
derivation(F, tree(F, [])) :-
    atomic(F) ; F = neg(_).

