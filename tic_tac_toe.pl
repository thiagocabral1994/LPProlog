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
        read_play(Board),
        board(NewBoard),
        print_board(NewBoard),
        assert_victory(NewBoard, x).

computer_play :-
        % board(Board),
        % read_play(Board),
        board(NewBoard),
        print_board(NewBoard),
        assert_victory(NewBoard, o).

initial_board(N, Board) :-
        length(Row, N),
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
        repeat,
        format('Digite a linha (1 a ~w): ', [NumberOfRows]),
        read(Row),
        format('Digite a coluna (1 a ~w): ', [NumberOfRows]),
        read(Col),
        get_board_element(Board, Row, Col, Element),
        assert_valid_play(Element),
        replace_board_element(Board, Row, Col, x),
        !.

get_board_element(Board, RowIndex, ColumnIndex, Element) :-
        nth1(RowIndex, Board, Row),
        nth1(ColumnIndex, Row, Element).

replace_board_element(Board, RowIndex, ColumnIndex, NewElement) :-
        nth1(RowIndex, Board, OldRow, TransferBoard),
        nth1(ColumnIndex, OldRow, e, TransferRow),
        nth1(ColumnIndex, NewRow, NewElement, TransferRow),
        nth1(RowIndex, NewBoard, NewRow, TransferBoard),
        replace_board(NewBoard).

assert_victory(Board, Symbol) :-
        check_victory_state(Board, Symbol),
        format('Jogador ~w ganhou o Jogo da Velha Clássico!~n', [Symbol]).

check_victory_state(Board, Symbol) :-
        check_row_victory(Board, Symbol);
        check_col_victory(Board, Symbol);
        check_diagonal_victory(Board, Symbol);
        check_anti_diagonal_victory(Board, Symbol).

check_col_victory(Board, Symbol) :-
        transpose(Board, TransposedBoard),
        check_row_victory(TransposedBoard, Symbol).

check_row_victory(Board, Symbol) :-
        length(Board, NumberOfRows),
        length(VictoryRow, NumberOfRows),
        maplist(=(Symbol), VictoryRow),
        member(VictoryRow, Board),
        !.

check_anti_diagonal_victory(Board, Symbol) :-
        reverse(Board, ReversedBoard),
        check_diagonal_victory(ReversedBoard, Symbol).

check_diagonal_victory(Board, Symbol) :-
        get_diagonal(Board, Diagonal),
        maplist(==(Symbol), Diagonal).

replace_board(NewBoard) :-
        retractall(board(_)),
        asserta(board(NewBoard)).

assert_valid_play(Element) :- Element = e.


get_diagonal(Matrix, Diagonal) :-
    get_diagonal(Matrix, 1, Diagonal).

get_diagonal([], _, []).
    
get_diagonal([Row|Rest], I, [Element|Diagonal]) :-
    nth1(I, Row, Element),
    I1 is I + 1,
    get_diagonal(Rest, I1, Diagonal).

