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

state(menu).
state(setup).
state(player1_turn).
state(player2_turn).
state(game_over).

% Setup state
run_state(setup, GameState) :-
    setup_game(GameState),
    run_state(player1_turn, GameState).

run_state(player1_turn, GameState) :-
    display_game(GameState),
    get_player_move(1, Move),
    make_move(GameState, Move, NewState),
    (game_over(NewState) -> 
     run_state(game_over, NewState);
     run_state(player2_turn, NewState)).

run_state(player2_turn, GameState) :-
    display_game(GameState),
    get_player_move(2, Move),
    make_move(GameState, Move, NewState),
    (game_over(NewState) -> 
     run_state(game_over, NewState);
     run_state(player1_turn, NewState)).

run_state(game_over, GameState) :-
    display_game(GameState),
    display_winner(GameState),
    run_state(menu, []).



game_loop :-
    setup_game.