/*
main.pl
------------------------------------------------------------
*/

:- module(main, [run_all/0]).
:- use_module(semantics).

/*
 ------------------------------------------------------------
Ejecutar todo el proceso:
  - Cargar queries.pl
  - Evaluar tautologías
  - Generar reporte LaTeX
------------------------------------------------------------
*/

run_all :-
    QueriesFile = 'tmp/queries.pl',
    OutFile = 'output/report.tex',
    (exists_file(QueriesFile) ->
        consult(QueriesFile),
        open(OutFile, write, S),
        format(S, '\\documentclass[12pt]{article}~n', []),
        format(S, '\\usepackage[utf8]{inputenc}~n', []),
        format(S, '\\usepackage{amsmath,amssymb}~n', []),
        format(S, '\\begin{document}~n', []),
        format(S, '\\section*{Resultados de las fórmulas lógicas}~n~n', []),
        forall(query(F),
            (
                (semantics:tautology(F) ->
                    format(S, '\\( ~w \\) es una \\textbf{tautología}.~n~n', [F])
                ;
                    format(S, '\\( ~w \\) \\textbf{no} es una tautología.~n~n', [F])
                )
            )
        ),
        format(S, '\\end{document}~n', []),
        close(S),
        writeln('Archivo generado: output/report.tex')
    ;
        writeln('No se encontró el archivo tmp/queries.pl.'),
        fail
    ).
% ------------------------------------------------------------

