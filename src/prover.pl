% -----------------------------------------
% prover.pl
% Módulo principal de evaluación lógica
% -----------------------------------------

:- module(prover, [run_all/0]).
:- dynamic query/1.

% Cargar las queries generadas por Python
load_queries(File) :-
    exists_file(File),
    consult(File),
    writeln('✔ Consultadas las queries desde:'), writeln(File).

% Evaluar una sola fórmula
evaluate(Query, Result) :-
    ( prove(Query) -> Result = true ; Result = false ).

% Semántica básica de operadores lógicos
prove(and(A, B)) :- prove(A), prove(B).
prove(or(A, _)) :- prove(A), !.
prove(or(_, B)) :- prove(B).
prove(implies(A, B)) :- \+ prove(A) ; prove(B).
prove(dimplies(A, B)) :- prove(implies(A, B)), prove(implies(B, A)).
prove(neg(A)) :- \+ prove(A).
prove(true).
prove(_):- fail.

% Procesar todas las queries
process_queries :-
    forall(query(Q), (
        evaluate(Q, R),
        format('\\( ~w \\) : ~w~n', [Q, R])
    )).

% Ejecutar todo el proceso y generar LaTeX
run_all :-
    load_queries('tmp/queries.pl'),
    open('output/report.tex', write, Stream),
    with_output_to(Stream, (
        format('\\documentclass[12pt]{article}~n'),
        format('\\usepackage[utf8]{inputenc}~n'),
        format('\\usepackage{amsmath,amssymb}~n'),
        format('\\begin{document}~n'),
        format('\\section*{Resultados de las fórmulas lógicas}~n'),
        format('\\noindent~n'),
        process_queries,
        format('\\end{document}~n')
    )),
    close(Stream),
    writeln('✅ Archivo generado: output/report.tex').

