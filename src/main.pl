/*
src/main.pl
------------------------------------------------------------
Punto de entrada principal
Genera PDF con fórmulas lógicas y árbol de derivación para tautologías
------------------------------------------------------------
*/

:- module(main, [run_all/0]).
:- use_module(semantics).
:- use_module(prover).
:- use_module(latex).

% ------------------------------------------------------------
% Predicado auxiliar para generar árbol simple de derivación
% ------------------------------------------------------------
formula_tree(F, tree(F, Subs)) :-
    F =.. [Op,A,B],
    member(Op,[and,or,implies,dimplies]),
    formula_tree(A, TA),
    formula_tree(B, TB),
    Subs = [TA,TB].
formula_tree(F, tree(F, [])) :-
    atomic(F) ; F = neg(_).

% ------------------------------------------------------------
% Ejecutar todo el proceso:
%   1. Cargar queries.pl
%   2. Evaluar tautologías
%   3. Generar PDF con árbol de derivación
% ------------------------------------------------------------

run_all :-
    QueriesFile = 'tmp/queries.pl',
    LatexFile  = 'output/report.tex',
    (exists_file(QueriesFile) ->
        consult(QueriesFile),
        start_latex_report(LatexFile),
        forall(query(F),
            (
                (tautology(F) ->
                    derivation(F, Tree),          % <-- aquí usamos derivation/2
                    add_formula(LatexFile, F, true, Tree)
                ;
                    add_formula(LatexFile, F, false)
                )
            )
        ),
        finish_latex_report(LatexFile),
        writeln('Archivo generado: output/report.tex')
    ;
        writeln('No se encontró el archivo tmp/queries.pl.'),
        fail
    ).

% ------------------------------------------------------------
% Procesar todas las queries de forma segura
% ------------------------------------------------------------
query_loop(LatexFile) :-
    forall(query(F),
        catch(process_formula(F, LatexFile), E,
              (print_message(error, E), writeln('Error procesando fórmula'), fail))
    ).

% ------------------------------------------------------------
% Procesa una fórmula individual
% ------------------------------------------------------------
process_formula(F, LatexFile) :-
    (tautology(F) ->
        (formula_tree_safe(F, Tree) ->
            add_formula(LatexFile, F, true, Tree)
        ;
            add_formula(LatexFile, F, true)   % si no se puede generar árbol, se muestra solo la fórmula
        )
    ;
        add_formula(LatexFile, F, false)
    ).

% ------------------------------------------------------------
% Wrapper seguro para formula_tree/2
% ------------------------------------------------------------
formula_tree_safe(F, Tree) :-
    catch(formula_tree(F, Tree), _, fail).
% ------------------------------------------------------------

