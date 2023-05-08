% Desenvolvedores:
%
% Daniel Augusto Machado Baeta - 201965122C
% Thiago do Vale Cabral - 201965220AC

% Inicializa um tabuleiro de tamanho Nx(N+1). Remove tabuleiro existente.
initial_board(N, Board) :-
        N > 1,
        NumberOfColumns is N + 1,
        length(Row, NumberOfColumns),
        maplist(=(e), Row),
        length(Board, N),
        maplist(=(Row), Board),
        replace_board(Board).

% Imprime o tabuleiro, linha por linha.
print_board([]).
print_board([Board|Tail]) :-
        write(Board),
        nl,
        print_board(Tail).

% Retorna uma lista com a diagonal. O índice `I` indica a coluna de início!
get_diagonal([], _, []).
get_diagonal([Row|Rest], I, [Element|Diagonal]) :-
        nth1(I, Row, Element),
        I1 is I + 1,
        get_diagonal(Rest, I1, Diagonal).

% Limpa com retractall o tabuleiro existente a adiciona um novo tabuleiro
replace_board(NewBoard) :-
        retractall(board(_)),
        asserta(board(NewBoard)).

% Retorna o número de linhas de um tabuleiro existente.
get_number_of_rows(NumberOfRows) :-
        board(Board),
        length(Board, NumberOfRows).

% Retorna o número de colunas em um tabuleiro existente. 
% É uma premissa de que o número de colunas será sempre o `NumberOfRows + 1`
get_number_of_cols(NumberOfColumns) :-
        get_number_of_rows(NumberOfRows),
        NumberOfColumns is NumberOfRows + 1.

% Retorna um elemento de um tabuleiro dado os índexes de linha e coluna
get_board_element(Board, RowIndex, ColumnIndex, Element) :-
        nth1(RowIndex, Board, Row),
        nth1(ColumnIndex, Row, Element).

% Retorna uma linha do tabuleiro, dado o índice da linha.
get_board_row(Board, RowIndex, ColumnIndex) :-
	transpose(Board, TransposedBoard),
        nth1(ColumnIndex, TransposedBoard, Column),
        findall(Index, nth1(Index, Column, e), RowPositions),
	max_list(RowPositions, RowIndex).

% Substitui um elemento de um tabuleiro, retornando um novo tabuleiro.
replace_board_element(Board, RowIndex, ColumnIndex, NewElement, NewBoard) :-
        nth1(RowIndex, Board, OldRow, TransferBoard),
        nth1(ColumnIndex, OldRow, e, TransferRow),
        nth1(ColumnIndex, NewRow, NewElement, TransferRow),
        nth1(RowIndex, NewBoard, NewRow, TransferBoard).

% Verifica se é versão normal.
isNormalPlay(X) :-
        X = normal.

