% prover.pl
% Simple sequent calculus prover (constructs proof trees)
:- module(prover, [prove_formula/3]).
:- use_module(parser).

% proof tree representation:
% proof(Sequent, RuleName, Subs) where Subs is list of child proofs.
% Sequent: sequent(LeftList, RightList) where lists of formulas

% Top-level: attempt to prove ⊢ F  (i.e., Left=[], Right=[F])
% Depth-limited search to avoid infinite loops.
prove_formula(F, Proof, ok) :-
    DepthLimit = 10,
    initial_sequent(Seq, F),
    prove(Seq, DepthLimit, [], Proof),
    !.
prove_formula(_, _, fail).

initial_sequent(sequent([], [F]), F).

% prove(Sequent, Depth, Visited, proof(...))
prove(Sequent, _, _, proof(Sequent, axiom, [])) :-
    Sequent = sequent(L, R),
    member(X, L), member(Y, R), X == Y, !.   % axiom: A ⊢ A

prove(Sequent, Depth, _Visited, proof(Sequent, 'neg-left', [P])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(neg(A), L, Lrest), !,
    % rule: from Γ ⊢ A, Δ  infer  ¬A, Γ ⊢ Δ
    NewSeq = sequent(Lrest, [A|R]),
    D2 is Depth - 1,
    prove(NewSeq, D2, [], P).

prove(Sequent, Depth, _Visited, proof(Sequent, 'neg-right', [P])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(neg(A), R, Rrest), !,
    % rule: from A, Γ ⊢ Δ  infer Γ ⊢ ¬A, Δ
    NewSeq = sequent([A|L], Rrest),
    D2 is Depth - 1,
    prove(NewSeq, D2, [], P).

% and-left: (A ∧ B), Γ ⊢ Δ  from A,B,Γ ⊢ Δ
prove(Sequent, Depth, _V, proof(Sequent, 'and-left', [P])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(and(A,B), L, Lrest), !,
    NewSeq = sequent([A,B|Lrest], R),
    D2 is Depth - 1,
    prove(NewSeq, D2, [], P).

% and-right: Γ ⊢ A and B, Δ  from Γ ⊢ A,Δ  and Γ ⊢ B,Δ  (two subproofs)
prove(Sequent, Depth, _V, proof(Sequent, 'and-right', [P1,P2])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(and(A,B), R, Rrest), !,
    New1 = sequent(L, [A|Rrest]),
    New2 = sequent(L, [B|Rrest]),
    D2 is Depth - 1,
    prove(New1, D2, [], P1),
    prove(New2, D2, [], P2).

% or-left: A ∨ B, Γ ⊢ Δ  from A,Γ ⊢ Δ  and B,Γ ⊢ Δ
prove(Sequent, Depth, _V, proof(Sequent, 'or-left', [P1,P2])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(or(A,B), L, Lrest), !,
    New1 = sequent([A|Lrest], R),
    New2 = sequent([B|Lrest], R),
    D2 is Depth - 1,
    prove(New1, D2, [], P1),
    prove(New2, D2, [], P2).

% or-right: Γ ⊢ A ∨ B, Δ from Γ ⊢ A, B, Δ (single rule)
prove(Sequent, Depth, _V, proof(Sequent, 'or-right', [P])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(or(A,B), R, Rrest), !,
    New = sequent(L, [A,B|Rrest]),
    D2 is Depth - 1,
    prove(New, D2, [], P).

% implies-left: (A -> B), Γ ⊢ Δ  from Γ ⊢ A,Δ and B,Γ ⊢ Δ
prove(Sequent, Depth, _V, proof(Sequent, 'implies-left', [P1,P2])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(implies(A,B), L, Lrest), !,
    New1 = sequent(Lrest, [A|R]),   % Γ ⊢ A, Δ
    New2 = sequent([B|Lrest], R),   % B, Γ ⊢ Δ
    D2 is Depth - 1,
    prove(New1, D2, [], P1),
    prove(New2, D2, [], P2).

% implies-right: Γ ⊢ A -> B, Δ  from A, Γ ⊢ B, Δ
prove(Sequent, Depth, _V, proof(Sequent, 'implies-right', [P])) :-
    Depth > 0,
    Sequent = sequent(L, R),
    select(implies(A,B), R, Rrest), !,
    New = sequent([A|L], [B|Rrest]),
    D2 is Depth - 1,
    prove(New, D2, [], P).

% dimplies / biconditional: treat as (A->B) and (B->A)
prove(Sequent, Depth, V, proof(Sequent, 'dimplies', Subs)) :-
    Depth > 0,
    Sequent = sequent(L, R),
    ( select(dimplies(A,B), L, Lrest) ->
        % expand on left as two conjucts
        NewL = [implies(A,B), implies(B,A) | Lrest],
        NewSeq = sequent(NewL, R),
        D2 is Depth - 1,
        prove(NewSeq, D2, V, P), Subs = [P]
    ; select(dimplies(A,B), R, Rrest) ->
        % on right: need to prove ⊢ (A<->B) by showing both directions on right
        New = sequent(L, [implies(A,B), implies(B,A) | Rrest]),
        D2 is Depth - 1,
        prove(New, D2, V, P), Subs = [P]
    ).

% fallback: try weakening or exchange by moving same atoms (very naive)
prove(_Sequent, _Depth, _V, _) :-
    % if no rule matched, fail
    fail.

