/*
 src/main.pl
 ------------------------------------------------------------
 Descripción:

 Punto de entrada principal del sistema. Procesa las consultas
 definidas en queries.pl, evalúa tanto fórmulas proposicionales
 como secuentes (LKP) y genera un reporte unificado en formato
 PDF con los resultados y árboles de derivación.
 ------------------------------------------------------------
*/

:- module(main, [run_all/0]).
:- use_module(semantics).
:- use_module(prover).
:- use_module(latex).
:- use_module(lkp).

% Ejecuta el flujo completo:
%  1. Carga las consultas desde tmp/queries.pl
%  2. Evalúa fórmulas o secuentes
%  3. Genera el reporte LaTeX y compila el PDF
run_all :-
    QueriesFile = 'tmp/queries.pl',
    LatexFile  = 'output/report.tex',
    (exists_file(QueriesFile) ->
        consult(QueriesFile),
        start_latex_report(LatexFile),
        forall(query(Type, F),
            catch(process_entry(Type, F, LatexFile), E,
                  (print_message(error, E),
                   writeln('Error procesando entrada'),
                   fail))
        ),
        finish_latex_report(LatexFile),
        writeln('✅ Archivo generado: output/report.tex')
    ;
        writeln('❌ No se encontró el archivo tmp/queries.pl.'),
        fail
    ).

% Procesa cada consulta del archivo queries.pl según su tipo

% Fórmulas proposicionales:
%  - Determina si son válidas, satisfacibles o insatisfacibles.
%  - Incluye árbol de derivación si está disponible.
process_entry(formula, F, LatexFile) :-
    ( semantics:tautology(F) ->
        ( catch(derivation(F, Tree), _, fail) -> T = Tree ; T = none ),
        add_formula_status(LatexFile, F, valid, T)
    ; semantics:satisfiable(F, Model) ->
        add_formula_status(LatexFile, F, satisfacible(Model), none)
    ;
        add_formula_status(LatexFile, F, insatisfacible, none)
    ).

% Secuentes (LKP):
%  - Determina si son derivables y genera su árbol de prueba.
process_entry(sequent, Seq, LatexFile) :-
    ( lkp_prove(Seq, ProofTree) ->
        add_sequent(LatexFile, Seq, true, ProofTree)
    ;
        add_sequent(LatexFile, Seq, false)
    ).

% Captura entradas desconocidas
process_entry(_, Unknown, _) :-
    format('⚠️ Tipo de entrada desconocido: ~w~n', [Unknown]).

