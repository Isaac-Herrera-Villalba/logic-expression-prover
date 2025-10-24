/*
  src/lkp.pl
  ------------------------------------------------------------
  Descripción:

  Implementa el cálculo de secuentes proposicional (LKP) para
  fórmulas construidas con los conectores:
    atom(X), neg(F), and(F,G), or(F,G), implies(F,G), dimplies(F,G).

  Permite determinar si un secuente es demostrable aplicando
  recursivamente las reglas del cálculo de secuentes.

  Estructura de prueba generada:
    node(Regla, Sequent, Subpruebas)
  ------------------------------------------------------------
*/

:- module(lkp, [
  lkp_valid/2,
  lkp_prove/2
]).

% Comprueba si una fórmula es tautología (⊢ F)
lkp_valid(F, Proof) :-
  lkp_prove(seq([], [F]), Proof).

% Determina si un secuente es demostrable mediante las reglas del LKP
lkp_prove(Seq, node(ax, Seq, [])) :-
  Seq = seq(Gamma, Delta),
  member(A, Gamma),
  member(B, Delta),
  A == B, !.

% Negación izquierda: Γ ⊢ Δ, A  →  Γ, ¬A ⊢ Δ
lkp_prove(seq([neg(A)|G], D), node(negL, seq([neg(A)|G], D), [P])) :- !,
  lkp_prove(seq(G, [A|D]), P).

% Negación derecha: Γ, A ⊢ Δ  →  Γ ⊢ Δ, ¬A
lkp_prove(seq(G, [neg(A)|D]), node(negR, seq(G, [neg(A)|D]), [P])) :- !,
  lkp_prove(seq([A|G], D), P).

% Conjunción izquierda: Γ, A, B ⊢ Δ  →  Γ, A∧B ⊢ Δ
lkp_prove(seq([and(A,B)|G], D), node(andL, seq([and(A,B)|G], D), [P])) :- !,
  lkp_prove(seq([A,B|G], D), P).

% Conjunción derecha: Γ ⊢ Δ, A  y  Γ ⊢ Δ, B  →  Γ ⊢ Δ, A∧B
lkp_prove(seq(G, [and(A,B)|D]), node(andR, seq(G, [and(A,B)|D]), [P1,P2])) :- !,
  lkp_prove(seq(G, [A|D]), P1),
  lkp_prove(seq(G, [B|D]), P2).

% Disyunción izquierda: Γ, A ⊢ Δ  y  Γ, B ⊢ Δ  →  Γ, A∨B ⊢ Δ
lkp_prove(seq([or(A,B)|G], D), node(orL, seq([or(A,B)|G], D), [P1,P2])) :- !,
  lkp_prove(seq([A|G], D), P1),
  lkp_prove(seq([B|G], D), P2).

% Disyunción derecha: Γ ⊢ Δ, A, B  →  Γ ⊢ Δ, A∨B
lkp_prove(seq(G, [or(A,B)|D]), node(orR, seq(G, [or(A,B)|D]), [P])) :- !,
  lkp_prove(seq(G, [A,B|D]), P).

% Implicación izquierda: Γ ⊢ Δ, A  y  Γ, B ⊢ Δ  →  Γ, A→B ⊢ Δ
lkp_prove(seq([implies(A,B)|G], D), node(impL, seq([implies(A,B)|G], D), [P1,P2])) :- !,
  lkp_prove(seq(G, [A|D]), P1),
  lkp_prove(seq([B|G], D), P2).

% Implicación derecha: Γ, A ⊢ Δ, B  →  Γ ⊢ Δ, A→B
lkp_prove(seq(G, [implies(A,B)|D]), node(impR, seq(G, [implies(A,B)|D]), [P])) :- !,
  lkp_prove(seq([A|G], [B|D]), P).

% Doble implicación izquierda: (A↔B) ≡ (A→B) ∧ (B→A)
lkp_prove(seq([dimplies(A,B)|G], D), node(iffL, seq([dimplies(A,B)|G], D), [P])) :- !,
  lkp_prove(seq([and(implies(A,B), implies(B,A))|G], D), P).

% Doble implicación derecha: Γ ⊢ Δ, A→B  y  Γ ⊢ Δ, B→A  →  Γ ⊢ Δ, A↔B
lkp_prove(seq(G, [dimplies(A,B)|D]), node(iffR, seq(G, [dimplies(A,B)|D]), [P1,P2])) :- !,
  lkp_prove(seq(G, [implies(A,B)|D]), P1),
  lkp_prove(seq(G, [implies(B,A)|D]), P2).

% Contracción izquierda: elimina duplicados en Γ
lkp_prove(seq(G,D), node(ctrL, seq(G,D), [P])) :-
  duplicate_in(G, X),
  remove_one(X, G, G1),
  lkp_prove(seq(G1, D), P).

% Contracción derecha: elimina duplicados en Δ
lkp_prove(seq(G,D), node(ctrR, seq(G,D), [P])) :-
  duplicate_in(D, X),
  remove_one(X, D, D1),
  lkp_prove(seq(G, D1), P).

% Detecta elementos duplicados en una lista
duplicate_in(List, X) :- select(X, List, Rest), member(X, Rest).

% Elimina una única aparición de un elemento en una lista
remove_one(X, [X|T], T) :- !.
remove_one(X, [H|T], [H|R]) :- remove_one(X, T, R).

% Calcula el tamaño estructural de una fórmula
size(atom(_), 1).
size(neg(A), N)      :- size(A, N1), N is N1 + 1.
size(and(A,B), N)    :- size(A, N1), size(B, N2), N is N1+N2+1.
size(or(A,B), N)     :- size(A, N1), size(B, N2), N is N1+N2+1.
size(implies(A,B),N) :- size(A, N1), size(B, N2), N is N1+N2+1.
size(dimplies(A,B),N):- size(A, N1), size(B, N2), N is N1+N2+1.

