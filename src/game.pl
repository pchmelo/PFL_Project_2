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
:- use_module(library(between)).

:- consult('display.pl').
:- consult('score.pl').

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

find_empty_spaces(Board, EmptySpaces) :-
    findall((RowIndex, ColIndex), (nth1(RowIndex, Board, Row), nth1(ColIndex, Row, empty)), EmptySpaces).

% Convert coordinates to letter-number format using row and column labels
coord_to_letter_number((Row, Col), RowLetters, ColNumbers, (A, B)) :-
    nth1(Row, RowLetters, A),
    nth1(Col, ColNumbers, B).

letter_number_to_coord((A, B), RowLetters, ColNumbers, (Row, Col)) :-
    nth1(Row, RowLetters, A),
    nth1(Col, ColNumbers, B).

% Valid moves predicate
valid_moves(game_state(_, _, _, Board1, RowLetters, ColNumbers, _, _, _, _, _, _), ListOfMoves) :-
    find_empty_spaces(Board1, EmptySpaces1),
    maplist(coord_to_letter_number_with_labels(RowLetters, ColNumbers), EmptySpaces1, ListOfMoves).

% Helper predicate to pass row and column labels to coord_to_letter_number/4
coord_to_letter_number_with_labels(RowLetters, ColNumbers, (Row, Col), (A, B)) :-
    coord_to_letter_number((Row, Col), RowLetters, ColNumbers, (A, B)).

change_turn((1, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), 
            (2, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)).

change_turn((2, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), 
            (1, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)).


state(menu).
state(setup).
state(player1_turn).
state(player2_turn).
state(bot_easy_turn).
state(bot_medium_turn).
state(game_over).

% Setup state
run_state(setup, GameState) :-
    setup_game(GameState),
    run_state(player1_turn, GameState).

change_score(game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), ((X, Y), Char), game_state(Turn, NewPlayer1Score, NewPlayer2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)) :-
    letter_number_to_coord((X, Y), RowLetters1, ColNumbers1, (A1t, B1t)),
    letter_number_to_coord((X, Y), RowLetters2, ColNumbers2, (A2t, B2t)),

    A1 is A1t - 1,
    B1 is B1t - 1,
    A2 is A2t - 1,
    B2 is B2t - 1,

    format('X: ~w Y: ~w', [X, Y]), nl,
    format('A1: ~w B1: ~w', [A1, B1]), nl,
    format('A2: ~w B2: ~w', [A2, B2]), nl,
    
    check_all_score(Board1, Player1Score, A1, B1, Char, NewPlayer1Score),
    check_all_score(Board2, Player2Score, A2, B2, Char, NewPlayer2Score).

run_state(player1_turn, GameState) :-
    read_user_input(1, GameState, x, (A1, B1)),
    move(GameState, ((A1, B1), x), NewGameStateX),
    change_score(NewGameStateX, ((A1, B1), x), NewGameStateXScored),
    display_game(NewGameStateXScored),

    read_user_input(1, NewGameStateXScored, o, (A2, B2)),
    move(NewGameStateXScored, ((A2, B2), o), NewGameStateO),
    change_score(NewGameStateO, ((A2, B2), o), NewGameStateOScored),
    display_game(NewGameStateOScored),

    %change_turn(NewGameStateO, NextNewGameState),
    run_state(player2_turn, NewGameStateOScored).

run_state(player2_turn, GameState) :-
    read_user_input(2, GameState, x, (A1, B1)),
    move(GameState, ((A1, B1), x), NewGameStateX),
    change_score(NewGameStateX, ((A1, B1), x), NewGameStateXScored),
    display_game(NewGameStateXScored),

    read_user_input(2, NewGameStateXScored, o, (A2, B2)),
    move(NewGameStateXScored, ((A2, B2), o), NewGameStateO),
    change_score(NewGameStateO, ((A2, B2), o), NewGameStateOScored),
    display_game(NewGameStateOScored),

    %change_turn(NewGameStateO, NextNewGameState),
    run_state(player1_turn, NewGameStateO).



game_loop :-
    run_state(setup, GameState),
    
    write('FIMMMM'), nl.

is_a_valid_move((Row, Col), [(Row, Col) | Tail], 1).
is_a_valid_move((_, _), [], 0).

is_a_valid_move((Row, Col), [(A, B) | Tail], Result):-
    is_a_valid_move((Row, Col), Tail, Result).

update_board(Board1, Board2, (A1, B1), (A2, B2), Char, NewBoard1, NewBoard2) :-
    replace_char_board_collum(Board1, B1, A1, 1, Char, NewBoard1),
    replace_char_board_collum(Board2, B2, A2, 1, Char, NewBoard2).

% Replace character in the specified column of the board
replace_char_board_collum([Line|Tail], Row, Col, Col, Char, [New_Row|Tail]) :-
    replace_char_board_row(Line, Row, 1, Char, New_Row).

replace_char_board_collum([Line|Tail], Row, Col, Actual_Col, Char, [Line|Res]) :-
    Next_Col is Actual_Col + 1,
    replace_char_board_collum(Tail, Row, Col, Next_Col, Char, Res).

% Replace character in the specified row of the board
replace_char_board_row([_|Tail], Row, Row, Char, [Char|Tail]).
replace_char_board_row([Head|Tail], Row, Actual_Index, Char, [Head|Res]) :-
    Next_Index is Actual_Index + 1,
    replace_char_board_row(Tail, Row, Next_Index, Char, Res).

move(game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), ((Row, Col), Char), game_state(Turn, Player1Score, Player2Score, NewBoard1, RowLetters1, ColNumbers1, NewBoard2, RowLetters2, ColNumbers2, Mode, Rows, Cols)) :- 
    valid_moves(game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), ListOfMoves),
    is_a_valid_move((Row, Col), ListOfMoves, 1),

    letter_number_to_coord((Row, Col), RowLetters1, ColNumbers1, (A1, B1)),
    letter_number_to_coord((Row, Col), RowLetters2, ColNumbers2, (A2, B2)),

    format('A1: ~w, B1: ~w, A2: ~w, B2: ~w~n', [A1, B1, A2, B2]),
    update_board(Board1, Board2, (A1, B1), (A2, B2), Char, NewBoard1, NewBoard2).