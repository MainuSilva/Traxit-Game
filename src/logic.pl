%% move(+GameState, ?Move, ?NewGameState)
%
% Executes a move using a game state and returning a new state.
% A move has the form NumberCard-CurrentPosition-NewPosition
% A position has the form Column-Row.
%
%
% @param Game state
% @param Move to execute
% @param Resulting game state
move([CP, CB, R, WS, BS], C-SC-SR-EC-ER, [CP, NB, R, WS, BS]):-
    can_move([CP, CB, _, _, _], C-SC-SR-EC-ER),
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB).

%% can_move(+GameState, ?Move)
%
% Checks if a move is possible given the current game state
%
% @param Game state
% @param Move to verify
can_move([CP, CB, _, _, _], C-SC-SR-EC-ER):-
    nth0_nested(SR, SC, CB, CP),
    nth0_nested(ER, EC, CB, 'o'),
    card_move(CB, C-SC-SR-EC-ER).

% Generate all possible paths by rotating the given path.
generate_all_moves(P, AP) :-
    generate_all_moves(P, AP, 0).

generate_all_moves(_, _, 4).

% Rotate a path to generate a new path and collect it in the result list.
generate_all_moves(P, [P | Rest], R) :-
    rotate_positions(P, RP),
    NR is R + 1,
    generate_all_moves(RP, Rest, NR).

% Rotate a list of positions (list of lists) by 90 degrees.
rotate_positions([], []).
rotate_positions([P | Rest], [RP | RRest]) :-
    rotate_coordinates(P, RP),
    rotate_positions(Rest, RRest).

% Rotate coordinates based on your requirements.
rotate_coordinates([C, R], [NC, NR]) :-
    rotate90cw(C, R, NC, NR). 

rotate90cw(C, R, NC, NR) :-
    NC is R,
    NR is -C.

generate_all_paths(_, _, [], []).

generate_all_paths(SC, SR, [M | RestAM], [AM | RestAP]) :-
    adjust_coordinates(SC, SR, M, AM),
    generate_all_paths(SC, SR, RestAM, RestAP).

% Adjust coordinates by adding SC and SR.
adjust_coordinates(_, _, [], []).
adjust_coordinates(SC, SR, [[SC0, SR0] | RestM], [[SC1, SR1] | RestAM]) :-
    SC1 is SC0 + SC,
    SR1 is SR0 + SR,
    adjust_coordinates(SC, SR, RestM, RestAM).

% filter_paths(+Paths, +FinalX, +FinalY, +GameBoard, -ValidPaths)
%
% Filters paths based on two conditions:
% 1. Whether a path ends in the final position [FinalX, FinalY].
% 2. Whether all positions in the path are empty on the GameBoard.
%
% @param P: List of paths
% @param FC: Final X-coordinate
% @param FR: Final Y-coordinate
% @param CB: The game board
% @param VP: List of valid paths
filter_paths([], _, _, _, []).
filter_paths([P|Rest], FC, FR, CB, VP) :-
    (path_ends_in(P, FC, FR), is_empty_path(P, CB) ->
        VP = [P|VR]
    ; 
        VP = VR
    ),
    filter_paths(Rest, FC, FR, CB, VR).

% is_empty_path(+Path, +GameBoard)
%
% Checks if all positions in the path are empty on the GameBoard.
%
% @param P: The path
% @param CB: The game board
is_empty_path([], _).
is_empty_path([[C, R]|Rest], CB) :-
    nth0_nested(R, C, CB, 'o'),
    is_empty_path(Rest, CB). 

% path_ends_in(+Path, +FinalX, +FinalY)
%
% Checks if a path ends in the final position [FinalCollumn, FinalRow].
%
% @param P: The path
% @param FC: Final Collumn
% @param FR: Final Row
path_ends_in(P, FC, FR) :-
    last(P, [FC, FR]).
                   
%% card_move(+Move)
%
% Checks if the given move is the card move associated
%
% @param Move�
card_move(CB, 1-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0]], AM),
    generate_all_paths(SC, SR, AM, AP),!,
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].
    
card_move(CB, 2-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0], [2, 0], [3, 0], [3, -1]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].                        
      
card_move(CB, 3-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0], [2, 0], [3, 0], [3, 1]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].
        
card_move(CB, 4-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0], [2, 0], [3, 0], [4, 0]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].

card_move(CB, 5-SC-SR-EC-ER) :-
    generate_all_moves([[0, 1], [1, 1], [2, 1], [3, 1] , [3, 0]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].

card_move(CB, 6-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0], [2, 0], [2, -1]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].

card_move(CB, 7-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0], [2, 0], [2, 1]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].

card_move(CB, 8-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0], [2, 0]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].

card_move(CB, 9-SC-SR-EC-ER) :-
    generate_all_moves([[1, 0], [2, 0], [3, SR], [3, -1], [3, -2]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].

card_move(CB, 10-SC-SR-EC-ER) :-
    generate_all_moves([[1, 1]], AM),
    generate_all_paths(SC, SR, AM, AP),
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].


%% nth0_nested(?Row, ?Col, ?List, ?Elem)
%
% Executes the nth0 predicate in a 2 dimensional list, allowing
%   for verification of elements.
%
% @param Row
% @param Col
% @param List
% @param Elem 
nth0_nested(R, C, L, E):-
    nth0(R, L, F),
    nth0(C, F, E).

% parse_move(+AlgebraicNotation, ?Move)
%
% Parses a move in algebraic notation to a game move
%
% @param Algebraic notation
% @param Card number
% @param Move
parse_move(SS-ES, C, C-SC-SR-EC-ER):-
    nonvar(SS), nonvar(ES), !,
    atom_chars(SS, SS_),
    atom_chars(ES, ES_),
    parse_square(SS_, SC, SR),
    parse_square(ES_, EC, ER).

%% parse_square(+AlgebraicNotation, ?Square)
%
%  Parses a move in algebraic notation to a game move
%
% @param Algebraic notation
% @param Collumn
% @param Row
parse_square([H|T], C, R):-
    char_code('a', AC),
    char_code(H, CC), 
    C is CC - AC, % determine H with ascci code
    catch(number_chars(IR,T), _, fail),
    R is 8 - IR. % calculate row index

change_round_score([_, _, _, WS, BS], SNGS):-
    value([_, _, _, _, WS, BS], w, WV),
    value([_, _, _, _, WS, BS], b, BV),
    FWS is WS + WV,
    FBS is BS + BV,
    SNGS = [_, _, _, _, FWS, FBS].

value([_, CB, _, _, _], P, V) :-
    get_pawn_positions(CB, P, Pos), % Get pawn positions for the player
    calculate_score(Pos, V). % Calculate the value based on positions

% Extract the two pawn positions of color P from the current board
get_pawn_positions(CB, P, Pos) :-
    findall([X, Y], (nth0(X, CB, Row), nth0(Y, Row, P)), Pos).

% Calculate the score based on two pawn positions
calculate_score(Pos, V) :-
    % Calculate the score for the first position
    nth0(0, Pos, [X1, Y1]),
    position_score(X1, Y1, S1),
    
    % Calculate the score for the second position
    nth0(1, Pos, [X2, Y2]),
    position_score(X2, Y2, S2),
    
    % Sum the scores for the two positions
    V is S1 + S2.

% Define a predicate to calculate the score for a single position
position_score(X, Y, Score) :-
    % Define the scoring logic based on the position (X, Y)
    % Here's an example scoring logic based on your description:
    (X >= 3, X =< 4, Y >= 3, Y =< 4 -> Score = 100;  % Center: 100 points
    X >= 2, X =< 5, Y >= 2, Y =< 5 -> Score = 75;    % Next layer: 75 points
    X >= 1, X =< 6, Y >= 1, Y =< 6 -> Score = 50;    % Outer layer: 50 points
    Score = 25).  % Outermost layer: 25 points (default)
                                   
%% switch_player(?CurrentToPlay, ?Next)
%
% Indicates who is the next player given the current player and
%   vice-versa.
%
% @param Current player
% @param Next player
switch_player(w, b).
switch_player(b, w).