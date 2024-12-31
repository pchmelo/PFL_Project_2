choose_move(GameState,  2,  ((X, Y), Char)) :-
    easy_bot_move(GameState, X, Y).

choose_move(GameState,  3,  ((X, Y), Char)) :-
    medium_bot_move(GameState, Char, X, Y).

easy_bot_move(GameState, X, Y) :-
    valid_moves(GameState,  ListOfMoves),
    random_member((X, Y), ListOfMoves).

% Define the medium bot move
medium_bot_move(GameState, Char, X, Y) :-
    valid_moves(GameState, ListOfMoves),
    find_best_move(GameState, Char, ListOfMoves, (X, Y)).

% Find the best move from the list of valid moves
find_best_move(GameState, Char, [(X, Y)|Tail], (Final_X, Final_Y)) :-
    move(GameState, ((X, Y), Char), NewGameState),
    change_score(NewGameState, ((X, Y), Char), NewGameStateScored),
    value(NewGameStateScored, 2, Value),
    find_best_move(GameState, Char, Tail, (X, Y), Value, (Final_X, Final_Y)).

% Base case: no more moves to check
find_best_move(_, _, [], (Best_X, Best_Y), _, (Best_X, Best_Y)).

% Recursive case: check the next move
find_best_move(GameState, Char, [(X, Y)|Tail], (Current_Best_X, Current_Best_Y), Current_Best_Value, (Final_X, Final_Y)) :-
    move(GameState, ((X, Y), Char), NewGameState),
    change_score(NewGameState, ((X, Y), Char), NewGameStateScored),
    value(NewGameStateScored, 2, Value),
    (Value > Current_Best_Value ->
        find_best_move(GameState, Char, Tail, (X, Y), Value, (Final_X, Final_Y))
    ;
        find_best_move(GameState, Char, Tail, (Current_Best_X, Current_Best_Y), Current_Best_Value, (Final_X, Final_Y))
    ).

% Evaluate the game state base on the player 1
value(game_state(_, Player1Score, Player2Score, _, _, _, _, _, _, _, _, _), 1, Value) :-
    Value is Player2Score - Player1Score.

% Evaluate the game state base on the player 2
value(game_state(_, Player1Score, Player2Score, _, _, _, _, _, _, _, _, _), 2, Value) :-
    Value is Player1Score - Player2Score.


example_game_state(GameState) :-
    Board1 = [
        [x, x, empty, empty, x, empty],
        [x, x, empty, empty, empty, empty],
        [x, x, o, o, empty, empty],
        [empty, x, empty, x, empty, empty],
        [x, empty, empty, empty, x, empty],
        [empty, empty, empty, empty, empty, empty]
    ],
    RowLetters1 = ['A', 'B', 'C', 'D', 'E', 'F'],
    ColNumbers1 = [1, 2, 3, 4, 5, 6],
    Board2 = [
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty]
    ],
    RowLetters2 = ['A', 'B', 'C', 'D', 'E', 'F'],
    ColNumbers2 = [1, 2, 3, 4, 5, 6],
    Mode = 'medium',  % Example game mode
    Rows = 6,
    Cols = 6,
    GameState = game_state(
        1,              % Current player
        3,              % Player1 score
        0,              % Player2 score
        Board1,         % Player1 board
        RowLetters1,    % Player1 row labels
        ColNumbers1,    % Player1 column numbers
        Board2,         % Player2 board
        RowLetters2,    % Player2 row labels
        ColNumbers2,    % Player2 column numbers
        Mode,           % Game mode
        Rows,           % Board rows
        Cols            % Board columns
    ).