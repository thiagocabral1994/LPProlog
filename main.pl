% Desenvolvedores:
%
% Daniel Augusto Machado Baeta - 201965122C
% Thiago do Vale Cabral - 201965220AC


:- use_module(library(clpfd)), use_module(library(lists)).

:-      consult('utils.pl'), 
        use_module(utils, all), 
        consult('rules.pl'), 
        use_module(rules, all),
        consult('minimax.pl'), 
        use_module(minimax, all).

play(Version, NumberOfRows) :-
        initial_board(NumberOfRows, InitialBoard),
        board(InitialBoard),
        writeln('Tabuleiro Inicial:'),
        print_board(InitialBoard),
        repeat,
        (
                human_play(Version);
                computer_play(Version);
                board(Board),
                assert_board_is_full(Board),
                writeln('Empate!'), !
        ).

human_play(Version):-
        board(Board),
        (   isNormalPlay(Version)
        ->  read_normal_play(Board)
        ;  
        read_simple_play(Board)
        ),
        board(NewBoard),
        print_board(NewBoard),
        nl,
        assert_victory(NewBoard, x),
        writeln('Você ganhou o jogo da Velha!').

computer_play(Version) :-
        board(Board),
        minimax(Version, Board, NewBoard),
        replace_board(NewBoard),
        writeln('Computador Jogou:'),
        print_board(NewBoard),
        nl,
        assert_victory(NewBoard, o),
        writeln('Computador ganhou o jogo da Velha!').

read_normal_play(Board) :-
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

read_simple_play(Board) :-
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

