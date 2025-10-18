% main.pl
% ------------------------------------------------------------
% Programa principal (solo lectura y parseo por ahora)
% Lee queries.txt y muestra las fórmulas parseadas.
% ------------------------------------------------------------
:- use_module(parser).
:- use_module(library(readutil)).

main :-
    File = 'queries.txt',
    ( exists_file(File)
    ->  read_file_to_string(File, Content, []),
        split_string(Content, "\n", "\s\t\r", Lines),
        process_lines(Lines, 1),
        halt
    ;   format('No se encontró ~w~n', [File]),
        halt(1)
    ).

process_lines([], _).
process_lines([L|Ls], N) :-
    ( parser:parse_formula(L, F) ->
        format('Línea ~d: OK  →  ~w~n', [N, F])
    ; L = "" ->
        true  % ignorar líneas vacías
    ; format('Línea ~d: ERROR al parsear → "~s"~n', [N, L])
    ),
    N2 is N + 1,
    process_lines(Ls, N2).

