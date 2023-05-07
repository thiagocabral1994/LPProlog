% Verifica se o elemento do tabuleiro é vazio (empty).
assert_valid_play(Element) :- Element = e.

% Garante a troca de símbolos de um jogador para outro.
toggle_symbol(x, o).
toggle_symbol(o, x).

% Calcula a pontuação baseado no tabuleiro `Board` fornecido.
% Caso o tabuleiro represente uma condição de vitória, ele terá uma pontuação
% positiva caso o jogador `o` ganhe. Caso o jogador `x` ganhe, a pontuação será negativa.
%
% O valor da pontuação é relativa também à profundidade em que a condição de vitória é encontrada.
% Quanto maior a profundidade `Depth` em que se encontra a condição, menor será a pontuação de vitória!
get_score(Board, Score, Depth) :-
        get_number_of_rows(NumberOfRows),
        get_number_of_cols(NumberOfColumns),
        MaxScore is NumberOfRows * NumberOfColumns,
        (
                (
                        assert_victory(Board, x), 
                        Score is Depth - MaxScore
                ), !;
                (
                        assert_victory(Board, o),
                        Score is MaxScore - Depth
                ), !;
                Score = 0
        ).

% Compara pontuações. Caso o jogador seja `o`, a menor pontuação será a maior. 
% Caso o contrário, a menor será preferível!
evaluate_score(Score, BestScore, o) :- Score > BestScore.
evaluate_score(Score, BestScore, x) :- Score < BestScore.

% Dado uma lista de tuplas `[(Board, Score)]`, seleciona o melhor par `(Board, Score)` baseado
% no evaluate_score do respective símbolo.
find_best_score(Symbol, [Head|Tail], BestState) :-
        find_best_score(Symbol, Tail, Head, BestState), !.

find_best_score(_, [], Acc, Acc).

find_best_score(Symbol, [Head|Tail], Acc, BestState) :-
        Acc = (_, BestScore),
        Head = (_, Score),
        evaluate_score(Score, BestScore, Symbol) -> 
                find_best_score(Symbol, Tail, Head, BestState);
                find_best_score(Symbol, Tail, Acc, BestState).


% Mapeia uma lista de coordenadas disponívels `e` para serem jogadas dado um tabuleiro `Board`. 
map_valid_board_play(Version, Board, Coordinates) :-
        (   isNormalPlay(Version)
        ->  map_valid_board_normal_play(Board, 1, [], Coordinates)
        ;   
        map_valid_board_simple_play(Board, Coordinates)
        ).


map_valid_board_simple_play(Board, Coordinates) :-
        transpose(Board, TransposedBoard),
        find_last_rows(TransposedBoard, 1, [], Coordinates).


% Encontra a última linha preenchida de cada coluna
find_last_rows([], _, Acc, Acc).
find_last_rows([Col|Tail], ColIndex, Acc, Coordinates) :-
        reverse(Col, ReversedCol),
        (
                nth0(RowReverseIndex, ReversedCol, e), ! ->
                (
                        length(Col,L),
                        RowIndex is L - RowReverseIndex,
                        append(Acc, [(RowIndex, ColIndex)], Result)
                );
                append(Acc, [], Result)
        ),
        NewColIndex is ColIndex + 1,
        find_last_rows(Tail, NewColIndex, Result, Coordinates).



map_valid_board_normal_play([], _, Acc, Acc).
map_valid_board_normal_play([Row|Tail], RowIndex, Acc, Coordinates) :-
        map_valid_row_play(Row, RowIndex, RowCoordinates),
        append(Acc, RowCoordinates, Result),
        NewRowIndex is RowIndex + 1,
        map_valid_board_normal_play(Tail, NewRowIndex, Result, Coordinates).

% Mapeia uma lista de coordenadas disponívels `e` para serem jogadas dada uma linha `Row`. 
% Este predicado varre casa por casa em uma linha de forma recursiva.
map_valid_row_play(Row, RowIndex, Coordinates) :-
        map_valid_row_play(Row, RowIndex, 1, [], Coordinates).
map_valid_row_play([], _, _, Acc, Acc).
map_valid_row_play([Element|Tail], RowIndex, ColIndex, Acc, Coordinates) :-
        (
                assert_valid_play(Element) ->
                append(Acc, [(RowIndex, ColIndex)], Result);
                append(Acc, [], Result)
        ),
        NewColIndex is ColIndex + 1,
        map_valid_row_play(Tail, RowIndex, NewColIndex, Result, Coordinates).

% Verifica que a linha está preenchida.
assert_row_is_full(Row) :-
        not(member(e, Row)).

% Verifica se o tabuleiro está preenchido.
assert_board_is_full(Board) :-
        flatten(Board, FlattenBoard),
        assert_row_is_full(FlattenBoard).

% Verifica qualquer condição de vitória de um símbolo `Symbol` em um tabuleiro.
assert_victory(Board, Symbol) :-
        check_row_victory(Board, Symbol), !;
        check_col_victory(Board, Symbol), !;
        check_diagonal_victory(Board, Symbol), !;
        check_anti_diagonal_victory(Board, Symbol).

% Verifica qualquer condição de vitória em coluna de um símbolo `Symbol` em um tabuleiro.
%
% Devido à natureza do tabuleiro, uma vitória em coluna é garantida 
% quando todos os elementos da coluna são `Symbol`. 
check_col_victory(Board, Symbol) :-
        transpose(Board, TransposedBoard),
        length(TransposedBoard, NumberOfRows),
        NumberOfColumns is NumberOfRows - 1,
        length(VictoryRow, NumberOfColumns),
        maplist(=(Symbol), VictoryRow),
        member(VictoryRow, TransposedBoard),
        !.

% Verifica qualquer condição de vitória em linha de um símbolo `Symbol` em um tabuleiro.
%
% Caracteriza uma vitória em Linha quando apenas um elemento de uma das extremidades
% da linha NÃO é `Symbol`. Para isso, contamos com predicados auxiliares `check_end_of_row`
% e `check_start_of_row`.
check_row_victory(Board, Symbol) :-
        check_row_victory(Board, 1, Symbol).

check_row_victory(Board, RowIndex, Symbol) :-
        length(Board, N),
        RowIndex =< N,
        nth1(RowIndex, Board, Row),
        (
                check_start_of_row(Row, Symbol), !;
                check_end_of_row(Row, Symbol), !;
                NewRowIndex is RowIndex + 1,
                check_row_victory(Board, NewRowIndex, Symbol)
        ).


% Verifica final da linha.
check_end_of_row([_|Tail], Symbol) :-
        maplist(==(Symbol), Tail).

% Verifica início da linha.
check_start_of_row(Row, Symbol) :-
        reverse(Row, ReversedRow),
        check_end_of_row(ReversedRow, Symbol).

% Verifica vitória em anti-diagonal. Definimos anti-diagonal como a diagonal da matrix
% reversa.
check_anti_diagonal_victory(Board, Symbol) :-
        reverse(Board, ReversedBoard),
        assert_diagonal(ReversedBoard, 1, Symbol).

% Verifica vitória em diagonal.
check_diagonal_victory(Board, Symbol) :-
        assert_diagonal(Board, 1, Symbol).

% Verifica se existe pelo menos uma diagonal válida. Só é válida uma diagonal de 
% tamanho N em um tabuleiro Nx(N+1).
assert_diagonal(Board, I, Symbol) :-
        length(Board, NumberOfRows),
        I =< NumberOfRows,
        (
                get_diagonal(Board, I, Diagonal),
                length(Diagonal, NumberOfRows),
                maplist(==(Symbol), Diagonal), !;
                I1 is I + 1,
                assert_diagonal(Board, I1, Symbol)
        ).
