:-use_module(library(random)).

%% move_bot(+GameState, +Move, -NewGameState)
%
% Executes a computer move using a game state and returning a new state.
%
% @param GameState
% @param Move
% @param NewGameState
move_bot([CP, CB, R, WC, BC, WS, BS], SC-SR-EC-ER, [CP, NB, R, WC, BC, WS, BS]):-
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB).

% choose_move(+GameState, +Level, +Card, -Move)
%
% Performs the move choice by the computer. Computer chooses best move for level 3 and a random move for level 2
%
% @param GameState
% @param Level
% @param Card
% @param Move
choose_move([CP, CB, _, _, _, _, _], 3, C, M) :-
    best_move([CP, CB, _, _, _, _, _], C, M).
choose_move([CP, CB, _, _, _, _, _], 2, C, M) :-
    random_move([CP, CB, _, _, _, _, _], C, M).

% best_move(+GameState, +Card, -Move)
%
% Finds the best greedy move for the computer player.
%
% @param GameState
% @param Card
% @param Move
best_move([CP, CB, _, _, _, _, _], C, M) :-
    valid_moves([_, CB, _, _, _, _, _], CP, C, AP),
    (AP = [] -> 
        M = []
    ;
        find_best_move(AP, -1, [], M)
    ).

% parse_path(+Path, -Move)
%
% Converts the ending position of a path in a move
%
% @param Path
% @param Move
parse_path(M, SC-SR-EC-ER):-
    M = [[SC, SR] | Rest],
    last(Rest, [EC, ER]).

% find_best_move (+ValidPaths, +BestScore, +BestMove, -FinalMove)
%
%  Chooses the best move for the computer based on which one generates the higher score
%
% @param ValidPaths
% @param BestScore
% @param BestMove
% @param FinalMove
find_best_move([], _BS, BM, BM).    
find_best_move([P | Rest], BS, BM, FM) :-
    calculate_path_score(P, S),
    (S > BS ->
        find_best_move(Rest, S, P, FM)
    ;
        find_best_move(Rest, BS, BM, FM)
    ).

% calculate_path_score(+Path, -Score)
%
% Calculates the resulting score of following a determined path.
%
% @param Path
% @param Score                                        
calculate_path_score(P, S) :-
    last(P, [R, C]),
    position_score(R, C, S). 

%% calculate_average_score(+Paths, -AverageScore)
%
% Calculates the average score of a list of paths.
%
% @param Paths
% @param AverageScore 
calculate_average_score(AP, AS) :-
    calculate_average_score(AP, 0, 0, AS).
calculate_average_score([], TS, NP, AS) :-
    (NP = 0 -> AS = 0; AS is TS / NP).
calculate_average_score([P | Rest], TS, NP, AS) :-
    calculate_path_score(P, S),
    NTS is TS + S,
    NNP is NP + 1,
    calculate_average_score(Rest, NTS, NNP, AS).

% random_move(+GameState, +Card, -Move)
%
% Chooses a random valid move for the computer.
%
% @param GameState
% @param Card
% @param Move    
random_move([CP, CB, _, _, _, _, _], C, M) :-
    valid_moves([_, CB, _, _, _, _, _], CP, C, AP),
    
    (AP = [] -> 
        M = []
    ;
        random_member(M, AP)
    ).  

%% get_bot_card(+GameState, +Level, -ChosenCard)
%
% Chooses a card for the opponent of the computer player.
%
% @param GameState 
% @param Level
% @param ChosenCard 
get_bot_card([CP, _CB, _, WC, BC, _, _], 2, C):-
    random_card(CP, WC, BC, C).
get_bot_card([CP, CB, _, WC, BC, _, _], 3, C):-
    greedy_choose_card([CP, CB, _, WC, BC, _, _], C).

%random_card(+Player, +WhiteCards, +BlackCards, -Card)
%
% Chooses a random card for the opponent
%
% @param Player
% @param WhiteCards
% @param BlackCards
% @param Card
random_card('w', _WC, BC, C):-
    random_member(C, BC).
random_card('b', WC, _BC, C):-
    random_member(C, WC).

%% greedy_choose_card(+GameState, -Card)
%
% Chooses the worst card for the opponent player based on the current game state.
%
% @param GameState
% @param Card
greedy_choose_card(['w', CB, _, _WC, BC, _, _], C):-
    BC = [UC | Rest],
    valid_moves([_, CB, _, _, _, _, _], 'w', C, VP),
    calculate_average_score(VP, AP),
    choose_worst_card(['w', CB, _, _, _, _, _], Rest, AP, UC).
greedy_choose_card(['b', CB, _, WC, _BC, _, _], C):-
    WC = [UC | Rest],
    valid_moves([_, CB, _, _, _, _, _], 'b', C, VP),
    calculate_average_score(VP, AP),
    choose_worst_card(['b', CB, _, _, _, _, _], Rest, AP, UC).

%% choose_worst_card(+GameState, +CardList, +BestAverage, -BestCard)
%
% Finds the worst card for the opponent player based on the game state and available cards.
%
% @param GameState 
% @param CardList 
% @param BestAverage
% @param BestCard 
choose_worst_card(_, [], _, _).
choose_worst_card([CP, CB, _, _, _, _, _], [C | Rest], BA, MC) :-
    valid_moves([_, CB, _, _, _, _, _], CP, C, VP),
    calculate_average_score(VP, AP),
    (AP < BA ->  NMC = C, NBA = AP; NMC = MC, NBA = BA),
    choose_worst_card([CP, CB, _, _, _, _, _], Rest, NBA, NMC).    
       
% bot_traxit_move(+GameState, +Level, -NewGameState)
%
% Performs a computer move when the computer is in traxit
%
% @param GameState
% @param Level
% @param NewGameState
bot_traxit_move([CP, CB, R, WC, BC, WS, BS], 2,  NGS):-
    random_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS).
bot_traxit_move([CP, CB, R, WC, BC, WS, BS], 3,  NGS):-
    greedy_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS).

%random_traxit_move(+GameState, -NewGameState)
%
% Chooses a random move for the computer when it is in traxit, and displays it.
%
% @param GameState
% @param NewGameState              
random_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS):-
    get_pawn_positions(CB, CP, Pos),
    random_member( [SR, SC], Pos),
    random_member([ER, EC], [[0, 0], [0, 7], [7, 0], [7, 7]]),
    parse_move(SS-ES, SC-SR-EC-ER),
    display_bot_traxit(CP,SS-ES),
    move_bot([CP, CB, R, WC, BC, WS, BS], SC-SR-EC-ER , NGS). 

% greedy_traxit_move(+GameState, -NewGameState)
%
% Chooses a Traxit move for the computer based on greediness, and displays it.
%
% @param GameState
% @param NewGameState
greedy_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS):-
    get_pawn_positions(CB, CP, Pos),
    random_member([ER, EC], [[0, 0], [0, 7], [7, 0], [7, 7]]),                                
    
    nth0(0, Pos, P1),
    calculate_path_score(P1, S1),

    nth0(1, Pos, P2),
    calculate_path_score(P2, S2),

    (S1 > S2 -> [SR, SC] = P1; [SR, SC] = P2),
     
    parse_move(SS-ES, SC-SR-EC-ER),
    display_bot_traxit(CP,SS-ES),
    move_bot([CP, CB, R, WC, BC, WS, BS], SC-SR-EC-ER , NGS).                                    
                                           
        
