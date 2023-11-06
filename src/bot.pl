:-use_module(library(random)).

move_bot([CP, CB, R, WC, BC, WS, BS], SC-SR-EC-ER, [CP, NB, R, WC, BC, WS, BS]):-
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB).

choose_move([CP, CB, _, _, _, _, _], 3, C, M) :-
    best_move([CP, CB, _, _, _, _, _], C, M).
choose_move([CP, CB, _, _, _, _, _], 2, C, M) :-
    random_move([CP, CB, _, _, _, _, _], C, M).

% Finds the best greedy move for the computer player.
best_move([CP, CB, _, _, _, _, _], C, M) :-
    valid_moves([_, CB, _, _, _, _, _], CP, C, AP),
    (AP = [] -> 
        M = []
    ;
        find_best_move(AP, -1, [], M)
    ).

parse_path(M, SC-SR-EC-ER):-
    M = [[SC, SR] | Rest],
    last(Rest, [EC, ER]).
  
find_best_move([], _BS, BM, BM).    
find_best_move([P | Rest], BS, BM, FM) :-
    calculate_path_score(P, S),
    (S > BS ->
        find_best_move(Rest, S, P, FM)
    ;
        find_best_move(Rest, BS, BM, FM)
    ).

calculate_path_score(P, S) :-
    last(P, [R, C]),
    position_score(R, C, S). 
    
random_move([CP, CB, _, _, _, _, _], C, M) :-
    valid_moves([_, CB, _, _, _, _, _], CP, C, AP),
    
    (AP = [] -> 
        M = []
    ;
        random_member(M, AP)
    ).  

random_card('w', _WC, BC, C):-
    random_member(C, BC).
random_card('b', WC, _BC, C):-
    random_member(C, WC).
    
bot_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS):-
    random_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS).
              
random_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS):-
    get_pawn_positions(CB, CP, Pos),
    random_member( [SR, SC], Pos),
    random_member([ER, EC], [[0, 0], [0, 7], [7, 0], [7, 7]]),
    nl,
    write('TRAXIT!'),
    nl,
    format('Player ~w moves the enemy pawn', [CP]),
    move_bot([CP, CB, R, WC, BC, WS, BS], SC-SR-EC-ER , NGS).                                    
    
    
