% initial_state(+GameConfig, -GameState)

% display_game(+GameState)

% move(+GameState, +Move, -NewGameState)

% valid_moves(+GameState,  -ListOfMoves)

% game_over(+GameState,  -Winner)

% value(+GameState, +Player, -Value)

% choose_move(+GameState,  +Level,  -Move)


:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(ordsets)).

:- consult('display.pl').

% Generate empty board
generate_empty_board(Rows, Cols, Board) :-
    length(Board, Rows),
    maplist(generate_empty_row(Cols), Board).

generate_empty_row(Cols, Row) :-
    length(Row, Cols),
    maplist(=(empty), Row).

% Generate random letters for rows
generate_row_letters(N, Letters) :-
    findall(L, (between(1, N, X), 
                char_code('A', Start), 
                Code is Start + X - 1, 
                char_code(L, Code)), 
            Alphabet),
    random_permutation(Alphabet, Letters).

% Generate random numbers for columns
generate_col_numbers(N, Numbers) :-
    numlist(1, N, List),
    random_permutation(List, Numbers).

% Initial state creation
initial_state([Rows, Cols, Mode], GameState) :-
    % Generate empty boards
    generate_empty_board(Rows, Cols, Board1),
    generate_empty_board(Rows, Cols, Board2),
    
    % Generate random coordinates
    generate_row_letters(Rows, RowLetters1),
    generate_row_letters(Rows, RowLetters2),
    generate_col_numbers(Cols, ColNumbers1),
    generate_col_numbers(Cols, ColNumbers2),
    
    % Create game state
    GameState = game_state(
        1,              % Current player
        0,              % Player1 score
        0,              % Player2 score
        Board1,         % Player1 board
        RowLetters1,    % Player1 row labels
        ColNumbers1,    % Player1 column numbers
        Board2,         % Player2 board
        RowLetters2,    % Player2 row labels
        ColNumbers2,    % Player2 column numbers
        Mode,          % Game mode
        Rows,          % Board rows
        Cols           % Board columns
    ).

% Convert coordinates to letter-number format using row and column labels
coord_to_letter_number((Row, Col), RowLetters, ColNumbers, (A, B)) :-
    nth1(Row, RowLetters, A),
    nth1(Col, ColNumbers, B).

% Valid moves predicate
valid_moves(game_state(_, _, _, Board1, RowLetters, ColNumbers, _, _, _, _, _, _), ListOfMoves) :-
    find_empty_spaces(Board1, EmptySpaces1),
    maplist(coord_to_letter_number_with_labels(RowLetters, ColNumbers), EmptySpaces1, ListOfMoves).

% Helper predicate to pass row and column labels to coord_to_letter_number/4
coord_to_letter_number_with_labels(RowLetters, ColNumbers, (Row, Col), (A, B)) :-
    coord_to_letter_number((Row, Col), RowLetters, ColNumbers, (A, B)).

state(menu).
state(setup).
state(player1_turn).
state(player2_turn).
state(game_over).

% Setup state
run_state(setup, GameState) :-
    setup_game(GameState).
    %run_state(player1_turn, GameState).

game_loop :-
    run_state(setup, GameState),
    valid_moves(GameState, ListOfMoves),
    display_coords(ListOfMoves).

