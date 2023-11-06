%% move(+GameState, +Move, -NewGameState)
%
% Executes a move using a game state and returning a new state.
% The move is specified in the form of `NumberCard-CurrentPosition-EndPosition`.
% A position consists of a `Column-Row` pair.
%
%
% @param GameState
% @param Move
% @param NewGameState
move([CP, CB, R, WC, BC, WS, BS], C-SC-SR-EC-ER, [CP, NB, R, WC, BC, WS, BS]):-
    can_move([CP, CB, _, _, _, _, _], C-SC-SR-EC-ER),
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB).

%% move_traxit(+GameState, +StartColumn-StartRow-EndColumn-EndRow, -NewGameState)
%
% Executes a Traxit move using a game state and returns a new game state.
% The move is specified in the form of `StartColumn-StartRow-EndColumn-EndRow`.
%
% @param GameState
% @param Traxit Move
% @param NewGameState
move_traxit([CP, CB, R, WC, BC, WS, BS], SC-SR-EC-ER, [CP, NB, R, WC, BC, WS, BS]):-
    can_move_traxit([CP, CB, _, _, _, _, _], SC-SR-EC-ER),
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB).

%% can_move(+GameState, +Move)
%
% Checks if a move is possible given the current game state
%
% @param GameState
% @param Move
can_move([CP, CB, _, _, _, _, _], C-SC-SR-EC-ER):-
    nth0_nested(SR, SC, CB, CP),
    nth0_nested(ER, EC, CB, 'o'),
    card_paths(C, SC, SR, AP),!,
    filter_paths(AP, EC, ER, CB, VP),
    VP \= [].
  
%% can_move_traxit(+GameState, +Move)
%
% Checks if a Traxit move is possible given the current game state.
% It checks if the move can be legally executed based on the game board and the specific rules of Traxit move, which require the final position to be on the edge of the board.
%
% @param GameState
% @param Move  
can_move_traxit([CP, CB, _, _, _, _, _], SC-SR-EC-ER):-
    nth0_nested(SR, SC, CB, CP),
    nth0_nested(ER, EC, CB, 'o'),
    (EC = 0, ER = 0;
     EC = 7, ER = 0;
     EC = 0, ER = 7;
     EC = 7, ER = 7).


%% generate_all_moves(+Path, -AllPaths)
%
% Generates all possible paths (based on the card moves) by rotating the given path. The recursion terminates when four rotations (360 degrees) have been applied.
%
% @param Path The original path to rotate.
% @param AllPaths The list of all possible paths obtained by rotating the original path.
generate_all_moves(P, AP) :-
    generate_all_moves(P, AP, 0).
generate_all_moves(_, _, 4).
generate_all_moves(P, [P | Rest], R) :-
    rotate_positions(P, RP),
    NR is R + 1,
    generate_all_moves(RP, Rest, NR).

%% rotate_positions(+Positions, -RotatedPositions)
%
% Rotates a list of positions by 90 degrees.
%
% @param Positions The original list of positions to rotate.
% @param RotatedPositions The list of positions obtained by rotating the original positions.
rotate_positions([], []).
rotate_positions([P | Rest], [RP | RRest]) :-
    rotate_coordinates(P, RP),
    rotate_positions(Rest, RRest).

%% rotate_coordinates(+Position, -RotatedPosition)
%
% Rotates a pair of coordinates (a list representing the column and row) by 90 degrees clockwise based on specified requirements.
%
% @param Position
% @param RotatedPosition
rotate_coordinates([C, R], [NC, NR]) :-
    rotate90cw(C, R, NC, NR). 

%% rotate90cw(+CurrentColumn, +CurrentRow, -NewColumn, -NewRow)
%
% Rotates coordinates 90 degrees clockwise.
%
% @param CurrentColumn
% @param CurrentRow
% @param NewColumn 
% @param NewRow
rotate90cw(C, R, NC, NR) :-
    NC is R,
    NR is -C.

%% generate_all_paths(+StartColumn, +StartRow, +Moves, -AllPaths)
%
% Generates all possible paths starting from a given position. It takes a list of moves, which represent card paths, and adds the initial position `[StartColumn, StartRow]` to each move to obtain the paths on the board based on the current position.
%
% @param StartColumn
% @param StartRow
% @param Moves
% @param AllPaths
generate_all_paths(_, _, [], []).
generate_all_paths(SC, SR, [M | RestAM], [AM | RestAP]) :-
    adjust_coordinates(SC, SR, M, AM),
    generate_all_paths(SC, SR, RestAM, RestAP).

%% adjust_coordinates(+StartColumn, +StartRow, +Moves, -AdjustedMoves)
%
% Adjusts coordinates by the starting values to each pair.
%
% @param StartColumn
% @param StartRow
% @param Moves The list of coordinate pairs to adjust.
% @param AdjustedMoves The list of adjusted coordinate pairs obtained by adding the starting values to each pair.
adjust_coordinates(_, _, [], []).
adjust_coordinates(SC, SR, [[SC0, SR0] | RestM], [[SC1, SR1] | RestAM]) :-
    SC1 is SC0 + SC,
    SR1 is SR0 + SR,
    adjust_coordinates(SC, SR, RestM, RestAM).

%% filter_paths(+Paths, +FinalColumn, +FinalRow, +CurrentBoard, -ValidPaths)
%
% Filters paths based on specified criteria. It checks if a path ends in the specified final column and row and if it is a valid path.
%
% @param Paths The list of paths to filter.
% @param FinalColumn
% @param FinalRow.
% @param CurrentBoard
% @param ValidPaths
filter_paths([], _, _, _, []).
filter_paths([P|Rest], FC, FR, CB, VP) :-
    (path_ends_in(P, FC, FR), is_valid_path(P, CB) ->
        VP = [P|VR]
    ; 
        VP = VR
    ),
    filter_paths(Rest, FC, FR, CB, VR).

%% filter_paths(+Paths, +CurrentBoard, -ValidPaths)
%
% Filters paths based on specified criteria. It checks if a path is a valid path on the current game board.
%
% @param Paths
% @param CurrentBoard
% @param ValidPaths
filter_paths([], _, []).
filter_paths([P|Rest], CB, VP) :-
    (is_valid_path(P, CB) ->
        VP = [P|VR]
    ; 
        VP = VR
    ),
    filter_paths(Rest, CB, VR).

% is_valid_path(+Path, +GameBoard)
%
% Checks if all positions in the path are empty and empty on the GameBoard, excluding the first element.
%
% @param Path
% @param GameBoard
is_valid_path(P, CB) :-
    skip_first_element(P, Rest),
    check_valid_path(Rest, CB).

% skip_first_element(+List, -Rest)
%
% Skips the first element of a list, returning the rest of the list.
%
% @param List: The input list
% @param Rest: The list without the first element
skip_first_element([_ | Rest], Rest).

% check_valid_path(+Path, +GameBoard)
%
% Checks if all positions in the path are valid and empty on the GameBoard.
% It ensures that the path does not intersect with occupied positions on the board and that each position's column and row are non-negative.
%
% @param Path
% @param GameBoard
check_valid_path([], _).
check_valid_path([[C, R] | Rest], CB) :-
    C >= 0, 
    R >= 0, 
    nth0_nested(R, C, CB, 'o'),
    check_valid_path(Rest, CB).

%% path_ends_in(+Path, +FinalColumn, +FinalRow)
%
% Checks if a path ends in the final position [FinalCollumn, FinalRow].
%
% @param Path
% @param FinalColumn
% @param FinalRow 
path_ends_in(P, FC, FR) :-
    last(P, [FC, FR]).
                   
% card_paths(+CardNumber, +StartColumn, +StartRow, -AllPaths)
%
% Generates all paths for the specified card number and starting position.
%
% @param Card
% @param StartColumn 
% @param StartRow 
% @param AllPaths 
card_paths(1, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0]], AM),
    generate_all_paths(SC, SR, AM, AP), !.
card_paths(2, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0], [2, 0], [3, 0], [3, 1]], AM1),
    generate_all_moves([[0, 0], [0, -1], [-1, -1], [-2, -1], [-3, -1]], AM2),
    generate_all_paths(SC, SR, AM1, AP1),
    generate_all_paths(SC, SR, AM2, AP2),
    append(AP1, AP2, AP), !.                     
card_paths(3, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0], [2, 0], [3, 0], [3, -1]], AM1),
    generate_all_moves([[0, 0], [0, 1], [-1, 1], [-2, 1], [-3, 1]], AM2),
    generate_all_paths(SC, SR, AM1, AP1),
    generate_all_paths(SC, SR, AM2, AP2),
    append(AP1, AP2, AP), !.   
card_paths(4, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]], AM),
    generate_all_paths(SC, SR, AM, AP).
card_paths(5, SC, SR, AP) :-
    generate_all_moves([[0, 0], [0, -1], [1, -1], [2, -1], [3, -1] , [3, 0]], AM1),
    generate_all_moves([[0, 0], [0, -1], [-1, -1], [-2, -1], [-3, -1] , [-3, 0]], AM2),
    generate_all_paths(SC, SR, AM1, AP1),
    generate_all_paths(SC, SR, AM2, AP2),
    append(AP1, AP2, AP), !.
card_paths(6, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0], [2, 0], [2, 1]], AM1),
    generate_all_moves([[0, 0], [0, -1], [-1, -1], [-2, -1]], AM2),
    generate_all_paths(SC, SR, AM1, AP1),
    generate_all_paths(SC, SR, AM2, AP2),
    append(AP1, AP2, AP), !. 
card_paths(7, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0], [2, 0], [2, -1]], AM1),
    generate_all_moves([[0, 0], [0, 1], [-1, 1], [-2, 1]], AM2),
    generate_all_paths(SC, SR, AM1, AP1),
    generate_all_paths(SC, SR, AM2, AP2),
    append(AP1, AP2, AP), !. 
card_paths(8, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0], [2, 0]], AM),
    generate_all_paths(SC, SR, AM, AP), !.
card_paths(9, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, 0], [2, 0], [3, 0], [3, 1], [3, 2]], AM1),
    generate_all_moves([[0, 0], [0, -1], [0, -2], [-1, -2], [-2, -2], [-3, -2]], AM2),
    generate_all_paths(SC, SR, AM1, AP1),
    generate_all_paths(SC, SR, AM2, AP2),
    append(AP1, AP2, AP), !. 
card_paths(10, SC, SR, AP) :-
    generate_all_moves([[0, 0], [1, -1]], AM),
    generate_all_paths(SC, SR, AM, AP), !.

%% valid_moves(+GameState, +Player, +Card, -ListOfMoves)
%
% Generates all valid paths for the two player's pawns using the specified card and current game board.
%
% @param GameState
% @param Player 
% @param Card
% @param ListOfMoves
valid_moves([_, CB, _, _, _, _, _], P, C, VP) :-
   get_pawn_positions(CB, P, Pos),
   nth0(0, Pos, [R1, C1]),
   nth0(1, Pos, [R2, C2]),
   card_paths(C, C1, R1, AP1),!,
   card_paths(C, C2, R2, AP2),!,
   append(AP1, AP2, AP),
   filter_paths(AP, CB, VP).
           
%% verify_traxit(+Card, +Player, +CurrentBoard)
%
% Verifies if a Traxit move is applicable for a player using the specified card and current game board.
% If there are no valid paths for a player's pawns using the specified card and considering the current game board. If there are no valid paths, the Traxit move is considered valid.
%
% @param Card
% @param Player 
% @param CurrentBoard              
verify_traxit([_, CB, _, _, _, _, _], P, C) :- valid_moves([_, CB, _, _, _, _, _], P,  C, []).

   
%% nth0_nested(?Row, ?Col, ?List, ?Elem)
%
% Executes the nth0 predicate in a 2 dimensional list, allowing for verification of elements.
%
% @param Row
% @param Col
% @param List
% @param Elem 
nth0_nested(R, C, L, E):-
    nth0(R, L, F),
    nth0(C, F, E).

%% parse_move(+AlgebraicNotation, +Card, -Move)
%
% Parses a move in algebraic notation to a game move
%
% @param Algebraic notation
% @param Card
% @param Move
parse_move(SS-ES, C, C-SC-SR-EC-ER):-
    nonvar(SS), nonvar(ES), !,
    atom_chars(SS, SS_),
    atom_chars(ES, ES_),
    parse_square(SS_, SC, SR),
    parse_square(ES_, EC, ER).
parse_move(SS-ES, SC-SR-EC-ER):-
    char_code('a', AC),
    char_code('1', ZC),
    ISR is 8 - SR - 1,
    IER is 8 - ER - 1,
    SCC is AC + SC,
    ECC is AC + EC,
    SRC is ZC + ISR,
    ERC is ZC + IER,
    atom_codes(SS, [SCC,SRC]),
    atom_codes(ES, [ECC,ERC]).

%% traxit_parse_move(+StartSquare-EndSquare, -StartColumn-StartRow-EndColumn-EndRow)
%
% Parses a Traxit move in algebraic notation to extract starting and ending positions.
%
% @param Algebraic notation
% @param Traxit move
traxit_parse_move(SS-ES, SC-SR-EC-ER):-
    nonvar(SS), nonvar(ES), !,
    atom_chars(SS, SS_),
    atom_chars(ES, ES_),
    parse_square(SS_, SC, SR),
    parse_square(ES_, EC, ER).

%% parse_square(+AlgebraicNotation, -Column, -Row)
%
% Parses a move in algebraic notation to a game move
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

%% change_round_score(+GameState, -NewGameState)
%
% Calculates and updates the scores at the end of a round in the game.
%
% @param GameState
% @param NewGameState
change_round_score([CP, CB, R, WC, BC, WS, BS], [CP, CB, R, WC, BC, FWS, FBS]):-
    value([CP, CB, R, WC, BC, WS, BS], w, WV),
    value([CP, CB, R, WC, BC, WS, BS], b, BV),
    FWS is WS + WV,
    FBS is BS + BV.

%% value(+GameState, +Player, -Score)
%
% Calculates the score for a specific player based on their pawn positions on the game board.
% It retrieves the positions of the player's pawns and calculates the score according to the game's rules.
%
% @param GameState 
% @param Player 
% @param Score
value([_, CB, _, _, _, _, _], P, V) :-
    get_pawn_positions(CB, P, Pos), % Get pawn positions for the player
    calculate_score(Pos, V). % Calculate the value based on positions

%% calculate_score(+PawnPositions, -Score)
%
% Calculates the score based on the positions of two pawns.
%
% @param PawnPositions
% @param Score
calculate_score(Pos, V) :-
    % Calculate the score for the first position
    nth0(0, Pos, [R1, C1]),
    position_score(R1, C1, S1),
    
    % Calculate the score for the second position
    nth0(1, Pos, [R2, C2]),
    position_score(R2, C2, S2),
    
    % Sum the scores for the two positions
    V is S1 + S2.

%% position_score(+Row, +Column, -Score)
%
% Calculates the score for a single position on the game board.
%
% @param Row
% @param Column
% @param Score
position_score(R, C, Score) :-
    % Define the scoring logic based on the position (X, Y)
    % Here's an example scoring logic based on your description:
    (R >= 3, R =< 4, C >= 3, C =< 4 -> Score = 100;  % Center: 100 points
    R >= 2, R =< 5, C >= 2, C =< 5 -> Score = 50;    % Next layer: 75 points
    R >= 1, R =< 6, C >= 1, C =< 6 -> Score = 25;    % Outer layer: 50 points
    Score = 0).  % Outermost layer: 25 points (default)

%% get_pawn_positions(+CurrentBoard, +Player, -PawnPositions)
%
% Extracts the positions of the two pawns of a specified color from the current game board.
%
% @param CurrentBoard
% @param Player
% @param PawnPositions
get_pawn_positions(CB, P, Pos) :-
    findall([X, Y], (nth0(X, CB, R), nth0(Y, R, P)), Pos).
                                   
%% switch_player(?CurrentToPlay, ?Next)
%
% Indicates who is the next player given the current player and
%   vice-versa.
%
% @param Current player
% @param Next player
switch_player(w, b).
switch_player(b, w).