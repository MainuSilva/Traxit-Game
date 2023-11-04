:-use_module(library(lists)).

%% initial_state(-GameState)
%
% Generates an initial game state for Traxit
% Uses a list containing 7 elements: Current player, Current board, Round, White Cards, Black Cards, White Score and Black Score.
%
% @param Initial GameState
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

%% game_over(+GameState, -Winner)
%
% Determines the winner of the Traxit game or if it's a tie based on the scores of White (WS) and Black (BS).
%
% @param GameState
% @param Winner The winner of the game or 'Tie' in case of a tie.
game_over([_, _, _, _, _, WS, BS], W) :-
    (WS > BS -> W = 'White';
     BS > WS -> W = 'Black';
     W = 'Tie').

%% replicate_nested(+Height, +Width, +Element, -List)
%
% Creates a 2-dimensional list with the given height and width, where each element is the specified element.
%
% @param Height 
% @param Width
% @param Element 
% @param List 
replicate_nested(H, W, E, L):-
    replicate(W, E, R),
    replicate(H, R, L).

%% replicate(+Size, +Element, -List)
%
% Creates a list with the given size, where each element is the specified element.
%
% @param Size
% @param Element
% @param List
replicate(S, E, L):-
    length(L, S),
    maplist(=(E), L).

%% replace_nested(+Row, +Column, +List, +NewElement, -NewList)
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

