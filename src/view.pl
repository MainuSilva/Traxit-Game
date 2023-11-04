%% display_logo
% 
% Displays the game logo and contact info
%
display_logo:-
    write('  _____                           _____ '), nl,
    write(' |_   _|                         |_   _| '), nl,
    write('   | |    __   __ _     __   __ _  | | '), nl,
    write('   | | |   _ \\ / _` | | \'_ \\ / _| | '), nl,
    write('   | |_| | | | (_| | | | | | (_| | | |'), nl,
    write('   |___|_| |_|\\__,_| |_| |_|\\__,___|'), nl,
    write('Welcome to Traxit!'), nl.

%% display_player_modes(+Color)
%
% Display player mode choice prompt
%
% @param Color of the player this prompt refers to
display_player_modes(Color):-
    nl,
    write(' ( Input the option number to select it ) \n'),
    (Color == 'w' -> PlayerColor = 'White'; PlayerColor = 'Black'),
    format(" Choose the type of player for ~w: \n", [PlayerColor]),
    write('       1. Human Player            \n'),
    write('       2. Easy Computer           \n'),
    write('       3. Hard Computer           \n').

%% display_board(+GameState)
%
% Displays the CurentBoard in a game state and its evaluation. 
%
% @param Game state
display_board([CP, CB, _, _, _, _, _]):-
    nl,
    write('  |  A B C D E F G H  |'),
    nl,
    display_separator,
    display_rows(CB),
    nl,
    (CP == 'b' ->
     write('Black to play\n');
     write('White to play\n')
    ).

%% display_separator(+Size)
%
% Displays a horizontal separator of the specified width
%
% @param Size
display_separator:-
    write('  +-------------------+'),
    nl.

%% display_rows(+Board, +Row)
%
% Displays the rows of the board.
%
% @param Board The game board
% @param Row The current row
display_rows([]).
display_rows([Row | Rest]):-
    length([Row | Rest], N),
    display_row(Row, N),
    display_separator,
    display_rows(Rest).

%% display_row(+Row, +RowNumber)
%
% Displays a single row of the board.
%
% @param Row The row to display
% @param RowNumber The row number (0-7)
display_row(Row, N):-
    write(N),
    write(' |  '),
    display_pieces(Row),
    write(' |'),
    nl.

%% display_pieces(+Row)
%
% Displays the individual pieces on a row.
%
% @param Row The row to display
display_pieces([]).
display_pieces([w | Rest]):-
    put_char('w'),
    put_char(' '),
    display_pieces(Rest), !.
display_pieces([b | Rest]):-
    put_char('b'),
    put_char(' '),
    display_pieces(Rest), !.
display_pieces([Piece | Rest]):-
    put_char(Piece),
    put_char(' '),
    display_pieces(Rest).
        
%% display_winner(+Winner)
%
% Displays a string announcing the game winner and the final scores
%
% @param Winner to congratulate
display_winner([_, _, _, _, _, WS, BS], W) :-
    nl, nl,
    (W == 'Tie' ->
        format("It's a tie! Final Score: ~d - ~d", [WS, BS])
    ;
        format(" ~w has won the game! Final Score: ~d - ~d", [W, WS, BS])
    ),
    display_logo, nl, nl.

% Define a predicate to display the score at the end of a scoring period.
display_period_score([_, _, R, _, _, NWS, NBS]) :-
    nl, nl,
    format("END OF ROUND ~d: Period Score: White: ~d - Black: ~d\n", [R, NWS, NBS]).
 
display_first_player:-
    nl,
    write('Who plays first? (w/b): ').

display_invalid_player:-
    write('Invalid input. Defaulting to white.'),
    nl.

display_choose_card('b', WC, _BC) :-
    nl,
    write('______________________________________________________________________ '), nl,
    write('|      '),(member(1,WC)-> write('1');write('-')),write('      |      '),(member(2,WC) -> write('2');write('-')),write('      |      '),(member(3,WC) -> write('3');write('-')),write('      |      '),(member(4,WC) -> write('4');write('-')),write('      |      '),(member(5,WC) -> write('5');write('-')),write('      |'), nl,
    write('|     ___     |   _______   |         _   |  _________  |   _______   |'), nl,
    write('|    |.|.|    |  |.|_|_|_|  |   _____|.|  | |.|_|_|_|.| |  |_|_|_|_|  |'), nl,
    write('|             |        |.|  |  |.|_|_|_|  |             |  |.|   |.|  |'), nl,
    write('|_____________|_____________|_____________|_____________|_____________|'), nl,
    write('|      '),(member(6,WC)-> write('6');write('-')),write('      |      '),(member(7,WC) -> write('7');write('-')),write('      |      '),(member(8,WC) -> write('8');write('-')),write('      |      '),(member(9,WC)->write('9');write('-')),write('      |      '),(member(10,WC)->write('10'); write('--')),write('     |'), nl,
    write('|    _____    |        _    |    _____    |   _______   |        _    |'), nl,
    write('|   |.|_|_|   |    ___|.|   |   |.|_|.|   |  |.|_|_|_|  |      _|.|   |'), nl,
    write('|       |.|   |   |.|_|_|   |             |        |_|  |     |.|     |'), nl,
    write('|_____________|_____________|_____________|________|.|__|_____________|'), nl,
    nl, 
    write('White player, choose a card for the black player: '). 

display_choose_card('w', _WC, BC) :-
    nl,
    write('______________________________________________________________________ '), nl,
    write('|      '),(member(1,BC)-> write('1');write('-')),write('      |      '),(member(2,BC) -> write('2');write('-')),write('      |      '),(member(3,BC) -> write('3');write('-')),write('      |      '),(member(2,BC) -> write('4');write('-')),write('      |      '),(member(5,BC) -> write('5');write('-')),write('      |'), nl,
    write('|     ___     |   _______   |         _   |  _________  |   _______   |'), nl,
    write('|    |.|.|    |  |.|_|_|_|  |   _____|.|  | |.|_|_|_|.| |  |_|_|_|_|  |'), nl,
    write('|             |        |.|  |  |.|_|_|_|  |             |  |.|   |.|  |'), nl,
    write('|_____________|_____________|_____________|_____________|_____________|'), nl,
    write('|      '),(member(6,BC)-> write('6');write('-')),write('      |      '),(member(7,BC) -> write('7');write('-')),write('      |      '),(member(8,BC) -> write('8');write('-')),write('      |      '),(member(9,BC)->write('9');write('-')),write('      |      '),(member(10,BC)->write('10'); write('--')),write('     |'), nl,
    write('|    _____    |        _    |    _____    |   _______   |        _    |'), nl,
    write('|   |.|_|_|   |    ___|.|   |   |.|_|.|   |  |.|_|_|_|  |      _|.|   |'), nl,
    write('|       |.|   |   |.|_|_|   |             |        |_|  |     |.|     |'), nl,
    write('|_____________|_____________|_____________|________|.|__|_____________|'), nl,
    nl, 
    write('Black player, choose a card for the white player: '). 


display_choose_move(CP, C):-
    nl,
    format('Choose a move in algebraic notation using card ~w: ', [C]),
    write('e.g., c1-d3'), nl,
    (CP == 'w' ->
        write('White player, make your move: ')
    ;
     CP == 'b' ->
        write('Black player, make your move: ')
    ).

display_traxit(P1) :-
    nl,
    (P1 == 'w' -> OP = 'Black'; OP = 'White'),
    write('TRAXIT!'),
    nl,
    format("~w player choose one of your adversary's pawns to move to one of the corners in algebraic notation (corners: a1, h1, a8, h8): ", [OP]).



    
   