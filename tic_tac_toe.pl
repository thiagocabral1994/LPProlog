:- use_module(library(clpfd)).
:- use_module(library(lists)).

play(NumberOfRows) :-
        initial_board(NumberOfRows, InitialBoard),
        board(InitialBoard),
        writeln('Tabuleiro Inicial:'),
        print_board(InitialBoard),
        repeat,
        (
                human_play;
                computer_play;
                board(Board),
                assert_full_board(Board),
                writeln('Empate!'), !
        ).

human_play :-
        board(Board),
        read_play(Board),
        % read_play2(Board),
        board(NewBoard),
        print_board(NewBoard),
        nl,
        assert_victory(NewBoard, x),
        writeln('Você ganhou o jogo da Velha!').

computer_play :-
        board(Board),
        minimax(Board, NewBoard),
        replace_board(NewBoard),
        writeln('Computador Jogou:'),
        print_board(NewBoard),
        nl,
        assert_victory(NewBoard, o),
        writeln('Computador ganhou o jogo da Velha!').

initial_board(N, Board) :-
        N > 1,
        NumberOfColumns is N + 1,
        length(Row, NumberOfColumns),
        maplist(=(e), Row),
        length(Board, N),
        maplist(=(Row), Board),
        replace_board(Board).

print_board([]).
print_board([Board|Tail]) :-
        write(Board),
        nl,
        print_board(Tail).

read_play(Board) :-
        length(Board, NumberOfRows),
        NumberOfColumns is NumberOfRows + 1,
        repeat,
        format('Digite a linha (1 a ~w): ', [NumberOfRows]),
        read(Row),
        format('Digite a coluna (1 a ~w): ', [NumberOfColumns]),
        read(Col),
        get_board_element(Board, Row, Col, Element),
        assert_valid_play(Element),
        replace_board_element(Board, Row, Col, x, NewBoard),
        replace_board(NewBoard),
        !.

read_play2(Board) :-
        length(Board, NumberOfRows),
        NumberOfColumns is NumberOfRows + 1,
        format('Digite a coluna (1 a ~w): ', [NumberOfColumns]),
        read(Col),
        get_board_row(Board, Row, Col),
        get_board_element(Board, Row, Col, Element),
        assert_valid_play(Element),
        replace_board_element(Board, Row, Col, x, NewBoard),
        replace_board(NewBoard),
        !.

get_board_element(Board, RowIndex, ColumnIndex, Element) :-
        nth1(RowIndex, Board, Row),
        nth1(ColumnIndex, Row, Element).

get_board_row(Board, RowIndex, ColumnIndex) :-
	transpose(Board, TransposedBoard),
        nth1(ColumnIndex, TransposedBoard, Column),
        findall(Index, nth1(Index, Column, e), RowPositions),
	max_list(RowPositions, RowIndex).

replace_board_element(Board, RowIndex, ColumnIndex, NewElement, NewBoard) :-
        nth1(RowIndex, Board, OldRow, TransferBoard),
        nth1(ColumnIndex, OldRow, e, TransferRow),
        nth1(ColumnIndex, NewRow, NewElement, TransferRow),
        nth1(RowIndex, NewBoard, NewRow, TransferBoard).

assert_full_row(Row) :-
        not(member(e, Row)).

assert_full_board(Board) :-
        flatten(Board, FlattenBoard),
        assert_full_row(FlattenBoard).

assert_victory(Board, Symbol) :-
        check_victory_state(Board, Symbol).

check_victory_state(Board, Symbol) :-
        check_row_victory(Board, Symbol), !;
        check_col_victory(Board, Symbol), !;
        check_diagonal_victory(Board, Symbol), !;
        check_anti_diagonal_victory(Board, Symbol).

check_col_victory(Board, Symbol) :-
        transpose(Board, TransposedBoard),
        length(TransposedBoard, NumberOfRows),
        NumberOfColumns is NumberOfRows - 1,
        length(VictoryRow, NumberOfColumns),
        maplist(=(Symbol), VictoryRow),
        member(VictoryRow, TransposedBoard),
        !.

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


check_end_of_row([_|Tail], Symbol) :-
        maplist(==(Symbol), Tail).

check_start_of_row(Row, Symbol) :-
        reverse(Row, ReversedRow),
        check_end_of_row(ReversedRow, Symbol).

check_anti_diagonal_victory(Board, Symbol) :-
        reverse(Board, ReversedBoard),
        assert_diagonal(ReversedBoard, 1, Symbol).

check_diagonal_victory(Board, Symbol) :-
        assert_diagonal(Board, 1, Symbol).

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

get_diagonal([], _, []).
    
get_diagonal([Row|Rest], I, [Element|Diagonal]) :-
        nth1(I, Row, Element),
        I1 is I + 1,
        get_diagonal(Rest, I1, Diagonal).

replace_board(NewBoard) :-
        retractall(board(_)),
        asserta(board(NewBoard)).

assert_valid_play(Element) :- Element = e.

minimax(Board, BestBoard) :-
        map_valid_board_play(Board, Coordinates),
        minimax(Board, o, 0, Coordinates, [], BoardStates),
        find_best_score(o, BoardStates, BestState),
        BestState = (BestBoard, _).

minimax(_, _, _, [], Acc, Acc).
minimax(Board, Symbol, Depth, [(Row, Col) | Tail], Acc, BoardStates) :-
        replace_board_element(Board, Row, Col, Symbol, NewBoard),
        get_score(NewBoard, Score, Depth),
        (
                (
                        % Condição de Vitória
                        assert_victory(NewBoard, Symbol),
                        append(Acc, [(NewBoard, Score)], BoardStates)
                ), !;
                (
                        (
                                % Empate com Tabuleiro Cheio
                                ( 
                                        assert_full_board(NewBoard);
                                        Depth > 2
                                ),
                                append(Acc, [(NewBoard, Score)], Results)
                        ), !;

                        % Empate com profundidade disponível
                        toggle_symbol(Symbol, NewSymbol),
                        NewDepth is Depth + 1,

                        map_valid_board_play(NewBoard, ChildCoordinates),
                        minimax(NewBoard, NewSymbol, NewDepth, ChildCoordinates, [], ChildBoardStates),
                        find_best_score(NewSymbol, ChildBoardStates, BestChildState),
                        BestChildState = (_, BestChildScore),
                        append(Acc, [(NewBoard, BestChildScore)], Results)
                ),
                minimax(Board, Symbol, Depth, Tail, Results, BoardStates)
        ), !.

find_best_score(Symbol, [Head|Tail], BestState) :-
        find_best_score(Symbol, Tail, Head, BestState), !.

find_best_score(_, [], Acc, Acc).

find_best_score(Symbol, [Head|Tail], Acc, BestState) :-
        Acc = (_, BestScore),
        Head = (_, Score),
        evaluate_score(Score, BestScore, Symbol) -> 
                find_best_score(Symbol, Tail, Head, BestState);
                find_best_score(Symbol, Tail, Acc, BestState).

map_valid_board_play(Board, Coordinates) :-
        map_valid_board_play(Board, 1, [], Coordinates).
map_valid_board_play([], _, Acc, Acc).
map_valid_board_play([Row|Tail], RowIndex, Acc, Coordinates) :-
        map_valid_row_play(Row, RowIndex, RowCoordinates),
        append(Acc, RowCoordinates, Result),
        NewRowIndex is RowIndex + 1,
        map_valid_board_play(Tail, NewRowIndex, Result, Coordinates).

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


toggle_symbol(x, o).
toggle_symbol(o, x).

evaluate_score(Score, BestScore, o) :- Score > BestScore.
evaluate_score(Score, BestScore, x) :- Score < BestScore.

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

get_number_of_rows(NumberOfRows) :-
        board(Board),
        length(Board, NumberOfRows).

get_number_of_cols(NumberOfColumns) :-
        get_number_of_rows(NumberOfRows),
        NumberOfColumns is NumberOfRows + 1.

