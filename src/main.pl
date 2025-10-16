% main.pl
:- use_module(parser).
:- use_module(prover).
:- use_module(latex).
:- use_module(library(readutil)).

main :-
    % asumimos que se ejecuta desde la raíz del proyecto
    QPath = 'queries.txt',
    ( exists_file(QPath) ->
        read_file_to_strings(QPath, Lines),
        ensure_output_dir,
        process_lines(Lines, 1),
        halt
    ; writeln('No queries.txt found in project root.'), halt
    ).

ensure_output_dir :-
    ( exists_directory('output') -> true ; make_directory('output') ).

read_file_to_strings(Path, Lines) :-
    read_file_to_string(Path, Content, []),
    split_string(Content, "\n", "\r\t ", Lines).

process_lines([], _).
process_lines([L|Ls], N) :-
    (L = "" -> N1 is N + 1, process_lines(Ls, N1) ; true),
    ( catch(parser:parse_line_formula(L, F), _, fail) ->
        format('Parsed: ~w~n', [F]),
        ( prover:prove_formula(F, Proof, ok) ->
            format('Query ~w : TRUE -> generating LaTeX...~n', [N]),
            atomic_list_concat(['output/proof_', N, '.tex'], Path),
            latex:proof_to_latex_file(Proof, Path)
        ; format('Query ~w : FALSE -> generating LaTeX message...~n', [N]),
            atomic_list_concat(['output/proof_', N, '.tex'], Path),
            open(Path, write, S),
            write(S, '\\documentclass{article}\n\\begin{document}\n'),
            write(S, '\\textbf{Query: } '), write(S, L), write(S, '\\\\[1em]\n'),
            write(S, '\\textbf{Resultado: } \\textit{FALSO.}\n'),
            write(S, '\\end{document}\n'),
            close(S)
        )
    ; format('Line ~w: cannot parse: ~s~n', [N, L])
    ),
    N1 is N + 1,
    process_lines(Ls, N1).

:- initialization(main, main).

