:-ensure_loaded('view.pl').
:-ensure_loaded('state.pl'). 
:-ensure_loaded('logic.pl').
:-ensure_loaded('bot.pl').

%% valid_player_mode(+Mode)
%
% Checks if the mode corresponds to a valid player mode
%
% @param Mode
valid_player_mode(M):-
   nonvar(M), M >= 1, M =< 3.

% valid_card(+Player, +Card, +WC, +BC)
%
% Checks if the `Card` corresponds to a valid card for the specified `Player`.
%
% @param Player
% @param Card
% @param WC (list of white cards)
% @param BC (list of black cards)
valid_card('w', C, _WC, BC):-
   nonvar(C),
   member(C, BC).
valid_card('b', C, WC, _BC):-
   nonvar(C),
   member(C, WC).


%%% Main menu %%%
%% menu
%
% Displays the game logo and collects game settings, storing them in the knowledge base.
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
% Initiates the game, clearing previous player and state information, displaying the main menu, and starting the game.
%
play:-
    retractall(player(_,_)),
    retractall(state(_)),
    menu,
    state(GS),
    play_game(GS).

%% play_game(+GameState)
%
% Manages the gameplay, including rounds and scoring in your game.
%
% @param GameState A list representing the current game state, including current player, cards, round count, card lists, and player scores.
play_game([CP, CB, R, WC, BC, WS, BS]):-
    (R > 16 ->
        game_over([_, _, R, _, _, WS, BS], W),
        display_winner(WS, BS, W),
        retractall(state(_)),
        retractall(player(_,_))
    ;
        NR is R + 1,
        get_current_player(CP, NR, P1),
        play_round([P1, CB, NR, WC, BC, WS, BS], NGS),
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
% Manages a single round of gameplay, retrieving a move from the player and executing it. Loops if human player input is invalid.
%
% @param Game State
% @param New game state
play_round([CP, CB, R, WC, BC, WS, BS], NGS) :-
    player(CP, 1), % human player
    display_board([CP, CB, _, _, _, _,_]),
    choose_card(CP, WC, BC, NWC, NBC, C),

    (verify_traxit([_, CB, _, _, _, _, _], CP, C) ->
       choose_traxit_move([CP, CB, R, NWC, NBC, WS, BS], NGS)
    ;                         
       display_choose_move(CP, C),
       repeat,
       catch((get_move(C, M)),_,(write('Invalid input. Try again\n'), fail)),
       try_move([CP, CB, R, NWC, NBC, WS, BS], M, NGS)
    ),
    skip_line, !.
play_round([CP, CB, R, WC, BC, WS, BS], NGS) :-
    player(CP, L), % computer player
    display_board([CP, CB, _, _, _, _,_]),
    choose_card(CP, WC, BC, NWC, NBC, C),
    choose_move([CP, CB, _, _, _, _, _], L, C, P),
    (P = [] ->
        choose_traxit_move([CP, CB, R, NWC, NBC, WS, BS], NGS)
    ;
    parse_path(P, M),
    display_move(CP,C),
    move_bot([CP, CB, R, NWC, NBC, WS, BS], M, NGS)
    ).


choose_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS):-
     (CP == 'w'  -> LP = 'b' ; LP = 'w'),
     player(LP, 1),
     display_traxit(CP),
     repeat,
     catch((get_traxit_move(M)),_,(write('Invalid input. Try again\n'), fail)),
     traxit_move([CP, CB, R, WC, BC, WS, BS], M, NGS).
choose_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS):-
     (CP == 'w'  -> LP = 'b' ; LP = 'w'),
     (player(LP, 2);
      player(LP, 3)),
     bot_traxit_move([CP, CB, R, WC, BC, WS, BS],  NGS).
     
choose_card(CP, WC, BC, NWC, NBC, C):-
     (CP == 'w'  -> LP = 'b' ; LP = 'w'),
     player(LP, 1),
     display_choose_card(CP, WC, BC), % Last player chooses next player's move.
     repeat,
     catch((get_card(CP, WC, BC, C)),_,(write('Invalid input. Try again\n'), fail)),
     remove_card(CP, C, WC, BC, NWC, NBC),
     skip_line, !.
choose_card(CP, WC, BC, NWC, NBC, C) :-
     (CP == 'w'  -> LP = 'b' ; LP = 'w'),
     (player(LP, 2);
      player(LP, 3)),
     random_card(CP, WC, BC, C),
     remove_card(CP, C, WC, BC, NWC, NBC).
         
%% remove_card(+Player, +Card, +WhiteCards, +BlackCards, -NewWhiteCards, -NewBlackCards)
%
% Removes a card from the specified card list (WhiteCards or BlackCards) based on the player type.
%
% This predicate removes a card from the appropriate card list (WhiteCards for black player or BlackCards for white player) and returns the updated card lists.
%
% @param Player.
% @param Card The card to be removed.
% @param WhiteCards The list of white cards (unused if Player is 'b').
% @param BlackCards The list of black cards (unused if Player is 'w').
% @param NewWhiteCards The updated list of white cards after removal.
% @param NewBlackCards The updated list of black cards after removal.
remove_card('w', C, WC, BC, WC, NBC) :-
    remove_element(C, BC, NBC).
remove_card('b', C, WC, BC, NWC, BC) :-
    remove_element(C, WC, NWC).

%% remove_element(+Element, +List, -NewList)
%
% Removes a specific element from the list.
%
% @param Element.
% @param List.
% @param NewList.
remove_element(_, [], []).
remove_element(C, [C|T], T).
remove_element(C, [H|T], [H|NT]) :- remove_element(C, T, NT).

   
%% get_player_mode(-Mode)
%
% Retrieves the game mode for a Player from the user
%
% @param Mode The chosen game mode.
get_player_mode(M):-
    read(M),
    (valid_player_mode(M) -> !; (write('Invalid Mode. Try again\n'), fail)).

%% get_card(+Player, +WhiteCards, +BlackCards, -Card,)
%
% Retrieves a card number from the user for the specified player.
%
% @param Player 
% @param WhiteCards
% @param BlackCards
% @param Card
get_card(P1, WC, BC, C):-
   read(C),
   (valid_card(P1, C, WC, BC)-> !; (write('Invalid card number. Try Again\n'), fail)).

%% try_move(+GameState, +Move, -NewGameState)
%
% Tries to execute the desired move. If successful outputs a new state; otherwise, it fails.
%
% @param GameState The current game state to apply the move.
% @param Move The move to be executed.
% @param NewGameState The new game state generated by applying the move.
try_move([CP, CB, R, WC, BC, WS, BS], M, NGS):-
   (move([CP, CB, R, WC, BC, WS, BS], M, NGS)-> !; (write('Invalid move!\n'), fail)).

%% traxit_move(+GameState, +Move, -NewGameState)
%
% Tries to execute a Traxit move. If successful, it outputs a new state; otherwise, it fails.
%
%
% @param GameState The current game state to apply the Traxit move.
% @param Move The Traxit move to be executed.
% @param NewGameState The new game state generated by applying the Traxit move.
traxit_move([CP, CB, R, WC, BC, WS, BS], M, NGS):-
   (move_traxit([CP, CB, R, WC, BC, WS, BS], M, NGS)-> !; (write('Invalid move!\n'), fail)).

%% get_move(+Card, -Move)
%
% Reads a Move with valid notation from input and parses it. Called recursively until successful.
%
% @param Card
% @param Move
get_move(C, M):-
    read(AN),
    (parse_move(AN, C, M) -> !; (write('Invalid algebraic notation. Try again\n'), get_move(C, M))).

%% get_traxit_move(-Move)
%
% Reads a Traxit move with valid algebraic notation from input and parses it. Called recursively until successful.
%
% @param Move
get_traxit_move(M):-
    read(AN),
    (traxit_parse_move(AN, M) -> !; (write('Invalid algebraic notation. Try again\n'), get_traxit_move(M))).
                          
%% get_current_player(+CurrentPlayer, +Round, -NextPlayer)
%
% Determines the current player based on the round. If it's the first round, it prompts the user to specify the first player. Otherwise, it switches to the next player based on the current player.
%
% @param CurrentPlayer.
% @param Round.
% @param NextPlayer.
get_current_player(CP, R, NP) :-
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
  

