/*
 src/logic.pl
 ------------------------------------------------------------
 Descripción:

 Módulo que implementa funciones auxiliares para el análisis de
 fórmulas lógicas. Permite obtener el conjunto de átomos y el
 conjunto de subfórmulas que componen una expresión lógica
 proposicional.
 ------------------------------------------------------------
*/

:- module(logic, [atoms/2, subformulas/2]).

% Obtiene la lista de átomos presentes en una fórmula
atoms(atom(X), [X]).
atoms(neg(F), A) :- atoms(F, A).
atoms(and(A,B), R) :- atoms(A, L1), atoms(B, L2), union(L1, L2, R).
atoms(or(A,B), R) :- atoms(A, L1), atoms(B, L2), union(L1, L2, R).
atoms(implies(A,B), R) :- atoms(A, L1), atoms(B, L2), union(L1, L2, R).
atoms(dimplies(A,B), R) :- atoms(A, L1), atoms(B, L2), union(L1, L2, R).

% Obtiene todas las subfórmulas de una fórmula
subformulas(atom(X), [atom(X)]).
subformulas(neg(F), [neg(F)|SF]) :-
    subformulas(F, SF).
subformulas(and(A,B), [and(A,B)|SF]) :-
    subformulas(A, SA),
    subformulas(B, SB),
    append(SA, SB, SF).
subformulas(or(A,B), [or(A,B)|SF]) :-
    subformulas(A, SA),
    subformulas(B, SB),
    append(SA, SB, SF).
subformulas(implies(A,B), [implies(A,B)|SF]) :-
    subformulas(A, SA),
    subformulas(B, SB),
    append(SA, SB, SF).
subformulas(dimplies(A,B), [dimplies(A,B)|SF]) :-
    subformulas(A, SA),
    subformulas(B, SB),
    append(SA, SB, SF).

