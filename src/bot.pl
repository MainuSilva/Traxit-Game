:-ensure_loaded('logic.pl').

% Choose a move for the computer player based on the specified level.
choose_move(GS, CP, 1, M) :-
    random_move(GS, CP, M).

choose_move(GS, CP, 2, M) :-
    best_move(GS, CP, M).

% Generates a random move for the computer player.
random_move(GS, CP, M) :-
    

% Finds the best move for the computer player.
best_move(GS, CP, M) :-
    