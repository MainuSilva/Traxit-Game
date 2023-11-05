:-ensure_loaded('logic.pl').

% Choose a move for the computer player based on the specified level.
choose_move([_, CB, _, _, _, _, _], P, 1, C, M) :-
    random_move([P, CB, _, _, _, _, _], P, C, M).

choose_move([_, CB, _, _, _, _, _], P, 2, C, M) :-
    best_move([_, CB, _, _, _, _, _], P, C, M).

% Generates a random move for the computer player.
random_move([_, CB, _, _, _, _, _], P, M) :-
    %igual a baixo mas a escolha é random
    

% Finds the best move for the computer player.
best_move([_, CB, _, _, _, _, _], P, C, M) :-
    pawns_card_paths(C, P, CB, VP),
    %recursão para ir por todos os da VP (all valid Path's) e fazer o calculate score
    %calculate_score
    %encapsular a calculate_score numa função que guarde o score máximo e o move correspondente que vai pro M
    
all_scores([]).
all_scores([Path|Rest], AS) :- 
    last(Path, X),
    nth0(0, X, CoordX),
    nth0(1, X, last(Path, X), CoordY),
    position_score(CoordX, CoordY, S),
    all_scores(Rest).
    