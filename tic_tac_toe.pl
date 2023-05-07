:- use_module(library(clpfd)), use_module(library(lists)).

:-      consult('utils.pl'), 
        use_module(utils, all), 
        consult('rules.pl'), 
        use_module(rules, all),
        consult('minimax.pl'), 
        use_module(minimax, all).

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
                assert_board_is_full(Board),
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

