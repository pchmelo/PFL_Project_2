/*
    parameters: GameState
    return: coordinates
    this predicate returns a random valid move from the list of valid moves, for the easy bot
*/
easy_bot_move(GameState, X, Y) :-
    valid_moves(GameState, ListOfMoves),
    random_member((X, Y), ListOfMoves).

/*
    parameters: GameState, Player, Char
    return: coordinates
    this predicate returns the best move from the list of valid moves, for the medium bot. The best move is obtaind by evaluating the game state after each move possible and selecting the move that gives the best value for the Player (Medium Bot)
*/
medium_bot_move(GameState, Player, Char, X, Y) :-
    valid_moves(GameState, ListOfMoves),
    find_best_move(GameState, Player, Char, ListOfMoves, (X, Y)).

/*
    parameters: GameState, Player, Char, ListOfMoves
    return: coordinates
    this predicate returns the best move from the list of valid moves. First gets the Gamestate resulted from each move possible, then evaluates the game state and save it in a list. Finally, it selects a random move the received list of best moves 
*/
find_best_move(GameState, Player, Char, [(X, Y)|Tail], (Final_X, Final_Y)) :-
    move(GameState, ((X, Y), Char), NewGameState),
    change_score(NewGameState, ((X, Y), Char), NewGameStateScored),
    value(NewGameStateScored, Player, Value),
    find_best_move(GameState, Player, Char, Tail, [(X, Y)], Value, BestMoves),
    random_member((Final_X, Final_Y), BestMoves).

/*
    parameters: GameState, Player, Char, ListOfMoves, CurrentBestMoves, CurrentBestValue, BestMoves
    return: coordinates
    this predicate finds the best move from the list of valid moves. It evaluates the score of the game state, if the score is better than the current best score, it updates the best score and the best move. If the score is equal to the current best score, it adds the move to the list of best moves. If the score is worse than the current best score, it keeps the current best score and the current best moves. It uses recursion to check the next move
*/
find_best_move(_, _, _, [], BestMoves, _, BestMoves).

% Recursive case: check the next move
find_best_move(GameState, Player, Char, [(X, Y)|Tail], CurrentBestMoves, CurrentBestValue, BestMoves) :-
    move(GameState, ((X, Y), Char), NewGameState),
    change_score(NewGameState, ((X, Y), Char), NewGameStateScored),
    value(NewGameStateScored, Player, Value),
    compare_moves(Value, CurrentBestValue, GameState, Player, Char, Tail, (X, Y), CurrentBestMoves, BestMoves).

/*
    parameters: Value, CurrentBestValue, GameState, Player, Char, Tail, (X, Y), BestMoves
    return: CurentBestMoves
    this predicate checks if the value of the game state is better than the current best value. If it is, it updates the best value and the best moves. If the value is equal to the current best value, it adds the move to the list of best moves. If the value is worse than the current best value, it keeps the current best value and the current best moves
*/
compare_moves(Value, CurrentBestValue, GameState, Player, Char, Tail, (X, Y), _, BestMoves) :-
    Value > CurrentBestValue,
    find_best_move(GameState, Player, Char, Tail, [(X, Y)], Value, BestMoves).

compare_moves(Value, Value, GameState, Player, Char, Tail, (X, Y), CurrentBestMoves, BestMoves) :-
    find_best_move(GameState, Player, Char, Tail, [(X, Y)|CurrentBestMoves], Value, BestMoves).

compare_moves(Value, CurrentBestValue, GameState, Player, Char, Tail, _, CurrentBestMoves, BestMoves) :-
    Value < CurrentBestValue,
    find_best_move(GameState, Player, Char, Tail, CurrentBestMoves, CurrentBestValue, BestMoves).

/*
    parameters: GameState, Player (1 - Player 1 and 2 - Player 2)
    return: Value
    this predicate returns the value of the game state based on the score of the player 1 or player 2 
*/
value(game_state(_, Player1Score, Player2Score, _, _, _, _, _, _, _, _, _), 1, Value) :-
    Value is Player2Score - Player1Score.

% Evaluate the game state based on the player 2
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