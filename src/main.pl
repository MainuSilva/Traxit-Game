:-ensure_loaded('view.pl').
:-ensure_loaded('state.pl'). 
:-ensure_loaded('logic.pl').


%% valid_player_mode(+Mode)
%
% Checks if the mode corresponds to a valid player mode
%
% @param Mode
valid_player_mode(M):-
   nonvar(M), M >= 1, M =< 3.

%% valid_card(+Card)
%
% Checks if the card corresponds to a valid card
%
% @param Mode
valid_card(C):-
   nonvar(C), C >= 1, C =< 10.

%%% Main menu %%%
%% menu
%
% Greets the user and gathers game settings, adding them to the knowlegde base
%
:- dynamic player/2, state/1.
menu:-
    display_logo,
    display_player_modes(w),
    repeat,
    
    catch((get_player_mode(MW)),_,(write('Invalid input. Try again\n'), fail)),
    assertz(player(w, MW)),
    display_player_modes(b),
    repeat,
    
    catch((get_player_mode(MB)),_,(write('Invalid input. Try again\n'), fail)),
    assertz(player(b, MB)),
    initial_state(GS),
    assertz(state(GS)).


%% play
%
% Clears the knowledge base of previous playthroughts and controls flow with fail based loop
%
play:-
    retractall(player(_,_)),
    retractall(state(_)),
    menu,
    state(GS),
    play_game(GS).

% Define a predicate to play a single round.
play_game([CP, CB, R, WS, BS]):-
    (R > 16 ->
        game_over([_, _, R, WS, BS], W),
        display_winner([_, _, R, WS, BS], W),
        retractall(state(_)),
        retractall(player(_,_))
    ;
        NR is R + 1,
        play_round([CP, CB, NR, WS, BS], NGS),
        assertz(state(NGS)),

        % Check if we've played 4 rounds (end of a scoring period).
        (0 =:= NR mod 4 ->
            % Calculate and display the score at the end of the scoring period.
            change_round_score(NGS, SNGS),
            display_period_score(SNGS),
                
            play_game(SNGS)
        ;
            % Continue playing the next round without scoring.
            play_game(NGS)
        )
    ).

%% play_round(+GameState, -NewGameState)
%
% Retrieve a Move from the player and execute it. Loops if Human player input is invalid.
% 
%
% @param Game State
% @param New game state
play_round([CP, CB, R, WS, BS], NGS) :-
    get_current_player([CP, _, R, _, _], P1),
    player(P1, 1), % human player
    display_board([P1, CB, _,_ ,_]),
    display_choose_card(P1), % Last player chooses next player's move.
    repeat,
    catch((get_card(C)),_,(write('Invalid input. Try again\n'), fail)),

    (verify_traxit(C, P1, CB) ->
       display_traxit(P1),
       repeat,
       catch((get_traxit_move(M)),_,(write('Invalid input. Try again\n'), fail)),
       traxit_move([P1, CB, R, WS, BS], M, NGS)
    ;                         
       display_choose_move(P1, C),
       repeat,
       catch((get_move(M, C)),_,(write('Invalid input. Try again\n'), fail)),
       try_move([P1, CB, R, WS, BS], M, NGS)
    ),
    skip_line, !.
   
   
%% get_player_mode(-Mode)
%
% Retrieves the game mode for a Player from the user
%
% @param Mode choosen
get_player_mode(M):-
    read(M),
    (valid_player_mode(M) -> !; (write('Invalid Mode. Try again\n'), get_player_mode(M))).

%% get_card(+Card)
%
%
% @param Card number
get_card(C):-
   read(C),
   (valid_card(C)-> !; (write('Invalid card number. Try Again\n'), fail)).

%% try_move(+GameState, +Move, -NewGameState)
%
% Tries to execute the desired move. If successful outputs a new sate, fails otherwise
%
% @param GameState to apply Move
% @param Move to execute
% @param New game state generated by Move
try_move([CP, CB, R, WS, BS], M, NGS):-
   (move([CP, CB, R, WS, BS], M, NGS)-> !; (write('Invalid move!\n'), fail)).

traxit_move([CP, CB, R, WS, BS], M, NGS):-
   (move([CP, CB, R, WS, BS], M, NGS)-> !; (write('Invalid move!\n'), fail)).

%% get_move(-Move, +CardNumber)
%
% Reads a Move with valid notation from input and parses it. Called recursively until succeds
%
% @param Move with valid notation
get_move(M, C):-
    read(AN),
    (parse_move(AN, C, M) -> !; (write('Invalid algebraic notation. Try again\n'), get_move(M, C))).

get_traxit_move(M):-
    read(AN),
    (parse_move(AN, M) -> !; (write('Invalid algebraic notation. Try again\n'), get_traxit_move(M))).
                          
% Define a predicate to get the current player based on the round.
get_current_player([CP, _, R, _,_], NP) :-
    (R =:= 1 -> 
        display_first_player,
        read(PC),
        (PC == 'w' -> NP = w;
        PC == 'b' -> NP = b;
        display_invalid_player,
        NP = w
        )
    ;
        switch_player(CP, NP)
    ).
  

