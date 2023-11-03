:-use_module(library(lists)).

%% initial_state(-GameState)
%
% Generates an initial game state for Traxit
% Uses a list containing 5 elements: Current player, Current board, Round, White Score and Black Score.
%
% @param Initial game state
initial_state([_, B, 0, C, C, 0, 0]):-
    S = 8, % Board size 8x8
    C = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    replicate_nested(S, S, o, SB),
    % Place two black pawns in C8 and F8
    replace_nested(0, 2, SB, b, TB1),
    replace_nested(0, 5, TB1, b, TB2),
    % Place two white pawns in C1 and F1
    replace_nested(7, 2, TB2, w, TB3),
    replace_nested(7, 5, TB3, w, TB4),
    % Place the spaces you can't go
    replace_nested(2, 2, TB4, x, TB5),
    replace_nested(2, 5, TB5, x, TB6),
    replace_nested(5, 2, TB6, x, TB7),
    replace_nested(5, 5, TB7, x, B).

game_over([_, _, _, _, _, WS, BS], W) :-
    (WS > BS -> W = 'White';
     BS > WS -> W = 'Black';
     W = 'Tie').

%% replicate_nested(?Height, ?Width, ?Elem, ?List)
%
% Creates/verifies 2 dimensional lists with a given element
%
% @param Height
% @param Width 
% @param Elem
% @param List
replicate_nested(H, W, E, L):-
    replicate(W, E, R),
    replicate(H, R, L).

% replicate(?Size, ?Elem, ?List)
%
% Creates/verifies a list with a given size filled with a given element
%
% @param Size
% @param Elem
% @param List
replicate(S, E, L):-
    length(L, S),
    maplist(=(E), L).


%% replace_nested(?Row, ?Column, ?List, ?Elem, ?NewList)
%
% Replaces/verifies an element in a 2-dimensional list
%
% @param Row
% @param Column
% @param List
% @param Elem
% @param NewList
replace_nested(R, C, L, E, NL):-
    nth0(R, L, F),
    replace(C, F, E, NF),
    nth0(R, L, _, K),
    nth0(R, NL, NF, K).

%% replace(?Index, ?List, ?Elem, ?NewList)
%
% Replaces/verifies a given element
%
% @param Index
% @param List
% @param Elem
% @param NewList
replace(I, L, E, NL):-
    nth0(I, L, _, R),
    nth0(I, NL, E, R).

