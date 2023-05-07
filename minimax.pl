
% Encontra a melhor jogada, baseado na busca minimax.
%
% A busca é inicializada com o símbolo `o`, respectivo à jogada do computador.
%
% Premissas:
%
% 1) Caso a nova jogada é uma condição de vitória, é realizado o corte naquele nível. 
% Isto ocorre porque a gente precisa de apenas uma vitória para validar a pontuação 
% daquele estado.
%
% 2) O empate só é considerado caso esgote jogadas no tabuleiro OU a busca ultrapassa o 
% segundo nível de busca. O limite de profundidade é necessário devido à complexidade da busca.
%
% 3) Caso seja permitido aprofundar na profundidade, minmax será invocado recursivamente.
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
                                        assert_board_is_full(NewBoard);
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

