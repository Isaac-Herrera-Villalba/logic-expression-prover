%% parser.pl
%% Parser para fórmulas lógicas con operadores en texto:
%% neg, and, or, implies, dimplies
%:- module(parser, [parse_line_formula/2, parse_formula/2]).
%
%% Representación interna:
%% atom(Name)
%% neg(F)
%% and(A,B)
%% or(A,B)
%% implies(A,B)
%% dimplies(A,B)
%
%parse_line_formula(Line, Formula) :-
%    normalize_space(string(Clean), Line),
%    Clean \= "",
%    string_codes(Clean, Cs),
%    phrase(expr(F), Cs),
%    !,
%    Formula = F.
%
%% alias compatible
%parse_formula(String, Formula) :-
%    parse_line_formula(String, Formula).
%
%% ------------------------------------------------------------
%% Gramática (tolerante a espacios)
%% ------------------------------------------------------------
%
%expr(F) --> blanks, disjunction(F), blanks.
%
%disjunction(F) -->
%    implication(A), blanks, keyword("or"), blanks, disjunction(B),
%    { F = or(A,B) }.
%disjunction(F) --> implication(F).
%
%implication(F) -->
%    equivalence(A), blanks, keyword("implies"), blanks, implication(B),
%    { F = implies(A,B) }.
%implication(F) -->
%    equivalence(A), blanks, keyword("dimplies"), blanks, implication(B),
%    { F = dimplies(A,B) }.
%implication(F) --> equivalence(F).
%
%equivalence(F) -->
%    conjunction(A), blanks, keyword("and"), blanks, equivalence(B),
%    { F = and(A,B) }.
%equivalence(F) --> conjunction(F).
%
%conjunction(F) --> unary(F).
%
%unary(neg(F)) -->
%    keyword("neg"), blanks, unary(F).
%unary(F) -->
%    "(", blanks, expr(F), blanks, ")".
%unary(atom(Name)) -->
%    identifier(Name).
%
%% ------------------------------------------------------------
%% Palabras clave insensibles a mayúsculas/minúsculas
%% ------------------------------------------------------------
%keyword(K) -->
%    string_without(" ()\t\r\n", S),
%    { string_lower(S, L), string_lower(K, L) }.
%
%% ------------------------------------------------------------
%% Identificadores
%% ------------------------------------------------------------
%identifier(Name) -->
%    letter(L), id_rest(Cs),
%    { atom_codes(A, [L|Cs]), Name = A }.
%
%letter(L) --> [L], { code_type(L, alpha) }.
%id_rest([C|Cs]) -->
%    [C],
%    { code_type(C, alnum) ; C = 0'_; C = 0'- },
%    !, id_rest(Cs).
%id_rest([]) --> [].
%
%blanks --> [W], { code_type(W, space) }, !, blanks.
%blanks --> [].

% parser.pl
:- module(parser, [parse_formula/2]).
:- op(1, fx, neg).
:- op(2, xfy, or).
:- op(2, xfy, and).
:- op(2, xfy, implies).
:- op(2, xfy, dimplies).

parse_formula(Line, Formula) :-
    normalize_space(string(Clean), Line),
    ( Clean = "" -> fail ; true ),
    catch(read_term_from_atom(Clean, Formula, []), _, fail).

