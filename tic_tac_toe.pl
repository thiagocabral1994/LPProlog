:- use_module(library(clpfd)).
:- use_module(library(lists)).

play(NumberOfRows) :-
        initial_board(NumberOfRows, InitialBoard),
        board(InitialBoard),
        writeln('Tabuleiro Inicial:'),
        print_board(InitialBoard),
        repeat,
        human_play;
        computer_play.

human_play :-
        board(Board),
        % read_play(Board),
        read_play2(Board),
        board(NewBoard),
        print_board(NewBoard),
        assert_victory(NewBoard, x),
        writeln('Você ganhou o jogo da Velha!').

computer_play :-
        % board(Board),
        % read_play(Board),
        board(NewBoard),
        print_board(NewBoard),
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

% MINMAX para o jogador 'o'
minmax :-
        writeln('TODO').

get_next_available_board_state(NewBoard) :-
        board(Board),
        get_next_available_board_state(Board, o, NewBoard),
        print_board(NewBoard).

% Isso aqui ainda não tá certo. To tentando fazer sentido aqui.
get_next_available_board_state(Board, Symbol, NewBoard) :- 
        loop_through(Row, Col),
        get_board_element(Board, Row, Col, Element),
        Element = e,
        replace_board_element(Board, Row, Col, Symbol, NewBoard),
        print_board(NewBoard),
        nl,
        (
                assert_victory(NewBoard, x), fail;
                assert_victory(NewBoard, o);
                (
                        Symbol = x -> get_next_available_board_state(NewBoard, o, _);
                        get_next_available_board_state(NewBoard, x, _), fail
                )
        ), !.

get_number_of_rows(NumberOfRows) :-
        board(Board),
        length(Board, NumberOfRows).

get_number_of_cols(NumberOfColumns) :-
        get_number_of_rows(NumberOfRows),
        NumberOfColumns is NumberOfRows + 1.

loop_through(Row, Col) :-
        get_number_of_rows(NumberOfRows),
        get_number_of_cols(NumberOfColumns),
        between(1, NumberOfRows, Row),
        between(1, NumberOfColumns, Col).
