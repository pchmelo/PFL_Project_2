:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(ordsets)).

:- consult('display.pl').
:- consult('score.pl').
:- consult('bot.pl').
/*
    parameters: Number of row and number of columns
    returns: empty board
    this predicate is used to generate an empty board
*/
generate_empty_board(Rows, Cols, Board) :-
    length(Board, Rows),
    maplist(generate_empty_row(Cols), Board).


/*
    parameters: Number of columns
    returns: empty row
    this predicate is used to generate an empty row
*/
generate_empty_row(Cols, Row) :-
    length(Row, Cols),
    maplist(=(empty), Row).


/*
    parameters: Number of rows
    returns: list of letters
    this predicate is used to generate a list of letters in random order. It is used as row labels for the board
*/
generate_row_letters(N, Letters) :-
    findall(L, (between(1, N, X), 
                char_code('A', Start), 
                Code is Start + X - 1, 
                char_code(L, Code)), 
            Alphabet),
    random_permutation(Alphabet, Letters).

/*
    parameters: Number of columns
    returns: list of numbers
    this predicate is used to generate a list of numbers in random order. It is used as column labels for the board
*/
generate_col_numbers(N, Numbers) :-
    numlist(1, N, List),
    random_permutation(List, Numbers).

/*
    parameters: Number of rows, number of columns, game mode
    returns: initial game state
    this predicate is used to generate the initial game state based on the number of rows, number of columns and game mode selected
*/
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

/*
    parameters: Board to check the empty spaces
    returns: list of empty spaces
    this predicate is used to find all the empty spaces in the board
*/
find_empty_spaces(Board, EmptySpaces) :-
    findall((RowIndex, ColIndex), (nth1(RowIndex, Board, Row), nth1(ColIndex, Row, empty)), EmptySpaces).

/*
    parameters: Cord to convert, row labels, column labels
    returns: converted cord
    this predicate is used to convert index coordinates to letter and number coordinates labels
*/
coord_to_letter_number((Row, Col), RowLetters, ColNumbers, (A, B)) :-
    nth1(Row, RowLetters, A),
    nth1(Col, ColNumbers, B).

/*
    parameters: Cord to convert, row labels, column labels
    returns: converted cord
    this predicate is used to convert letter and number coordinates labels to index coordinates
*/
letter_number_to_coord((A, B), RowLetters, ColNumbers, (Row, Col)) :-
    nth1(Row, RowLetters, A),
    nth1(Col, ColNumbers, B).

/*
    parameters: Game state
    returns: list of valid moves
    this predicate is used to find all the valid moves in the current game state based on the empty spaces in the board
*/
valid_moves(game_state(_, _, _, Board1, RowLetters, ColNumbers, _, _, _, _, _, _), ListOfMoves) :-
    find_empty_spaces(Board1, EmptySpaces1),
    maplist(coord_to_letter_number_with_labels(RowLetters, ColNumbers), EmptySpaces1, ListOfMoves).

/*
    parameters: Row labels, column labels, coordinates to convert
    returns: converted coordinates
    this predicate is used to convert index coordinates to letter and number coordinates labels
*/
coord_to_letter_number_with_labels(RowLetters, ColNumbers, (Row, Col), (A, B)) :-
    coord_to_letter_number((Row, Col), RowLetters, ColNumbers, (A, B)).

/*
    parameters: Game state
    returns: new game state
    this predicate is used to change the turn of the game state 1 -> 2 or 2 -> 1
*/
change_turn(game_state(1, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), 
            game_state(2, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)).

change_turn(game_state(2, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), 
            game_state(1, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)).


/*
    parameters: Game state, coordinates to move
    returns: new game state
    this predicate is used to change the score of the game state based on the move made
*/
change_score(game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), ((X, Y), Char), game_state(Turn, NewPlayer1Score, NewPlayer2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)) :-
    letter_number_to_coord((X, Y), RowLetters1, ColNumbers1, (A1t, B1t)),
    letter_number_to_coord((X, Y), RowLetters2, ColNumbers2, (A2t, B2t)),

    A1 is A1t - 1,
    B1 is B1t - 1,
    A2 is A2t - 1,
    B2 is B2t - 1,
    
    check_all_score(Board1, Player1Score, A1, B1, Char, NewPlayer1Score),
    check_all_score(Board2, Player2Score, A2, B2, Char, NewPlayer2Score).

/*
    parameters: state of the game(setup)
    returns: new game state
    this predicate is used to setup the game state and then start the game with player 1's turn
*/
run_state(setup, GameState) :-
    setup_game(GameState, ModeP1, ModeP2),
    run_state(player1_turn, ModeP1, ModeP2, GameState).

/*
    parameters: state of the game(game over), winner(draw)
    returns: none
    this predicate is used to display the end of the game, in this case a draw, and then ask the user if they want to retry the game
*/
run_state(game_over, 0) :-
    write('Game over! Its a Draw!'), nl,
    retry_game(Option),
    run_state(retry_game, Option).


/*
    parameters: state of the game(game over), winner(player)
    returns: none
    this predicate is used to display the end of the game, in this case a player has won, and then ask the user if they want to retry the game
*/
run_state(game_over, Winner) :-
    format('Game over! Winner: Player ~w~n', [Winner]),
    retry_game(Option),
    run_state(retry_game, Option).

/*
    parameters: state of the game(retry game), option to retry the game
    returns: none
    this predicate is used to ask the user if they want to retry the game
*/
run_state(retry_game, 1) :-
    run_state(setup, _).

/*
    parameters: state of the game(retry game), option to retry the game
    returns: none
    this predicate is used to end the game
*/
run_state(retry_game, 2) :-
    write('End of the Game'), nl.

/*
    parameters: state of the game(player 1's turn), player 1's mode, player 2's mode
    returns: new game state
    this predicate is used to handle player 1's turn and then check if the game is over or continue with player 2's turn
*/
run_state(player1_turn, ModeP1, ModeP2, GameState) :-
    make_player1_move_x(GameState, ModeP1, NewGameStateXScored),
    handle_player1_x_outcome(NewGameStateXScored, ModeP1, ModeP2).

/*
    parameters: state of the game(player 2's turn), player 1's mode, player 2's mode
    returns: new game state
    this predicate is used to handle player 2's turn and then check if the game is over or continue with player 1's turn
*/
run_state(player2_turn, ModeP1, ModeP2, GameState) :-
    make_player2_move_x(GameState, ModeP2, NewGameStateXScored),
    handle_player2_x_outcome(NewGameStateXScored, ModeP1, ModeP2).

/*  
    parameters: state of the game(player 1's turn), player 1's mode
    returns: new game state
    this predicate is used to handle player 1's turn first move, choose a move and then update the game state with the move made and score, then display the game state
*/
make_player1_move_x(GameState, ModeP1, NewGameStateXScored) :-
    choose_move(GameState, ModeP1, 1, ((A1, B1), x)),
    move(GameState, ((A1, B1), x), NewGameStateX),
    change_score(NewGameStateX, ((A1, B1), x), NewGameStateXScored),
    display_game(NewGameStateXScored).

/*
    parameters: state of the game(player 1's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used to check if the game is over and then call the game over state after the first move made by player 1
*/
handle_player1_x_outcome(GameState, _, _) :-
    game_over(GameState, Winner),
    run_state(game_over, Winner).

/*
    parameters: state of the game(player 1's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used when the first move made by player 1 is not a game over move. It handles player 1's second move and then checks if the game is over or continue with player 2's turn
*/
handle_player1_x_outcome(GameState, ModeP1, ModeP2) :-
    \+ game_over(GameState, _),
    make_player1_move_o(GameState, ModeP1, NewGameStateOScored),
    handle_player1_o_outcome(NewGameStateOScored, ModeP1, ModeP2).

/*  
    parameters: state of the game(player 1's turn), player 1's mode
    returns: new game state
    this predicate is used to handle player 1's turn second move, choose a move and then update the game state with the move made and score, then display the game state
*/
make_player1_move_o(GameState, ModeP1, NewGameStateOScored) :-
    choose_move(GameState, ModeP1, 1, ((A2, B2), o)),
    move(GameState, ((A2, B2), o), NewGameStateO),
    change_score(NewGameStateO, ((A2, B2), o), GameStateOScored),
    change_turn(GameStateOScored, NewGameStateOScored),
    display_game(NewGameStateOScored).

/*
    parameters: state of the game(player 1's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used to check if the game is over and then call the game over state after the second move made by player 1
*/
handle_player1_o_outcome(GameState, _, _) :-
    game_over(GameState, Winner),
    run_state(game_over, Winner).

/*
    parameters: state of the game(player 1's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used when the second move made by player 1 is not a game over move. It calls player 2's turn
*/
handle_player1_o_outcome(GameState, ModeP1, ModeP2) :-
    \+ game_over(GameState, _),
    run_state(player2_turn, ModeP1, ModeP2, GameState).

/*  
    parameters: state of the game(player 2's turn), player 1's mode
    returns: new game state
    this predicate is used to handle player 2's turn first move, choose a move and then update the game state with the move made and score, then display the game state
*/
make_player2_move_x(GameState, ModeP2, NewGameStateXScored) :-
    choose_move(GameState, ModeP2, 2, ((A1, B1), x)),
    move(GameState, ((A1, B1), x), NewGameStateX),
    change_score(NewGameStateX, ((A1, B1), x), NewGameStateXScored),
    display_game(NewGameStateXScored).

/*
    parameters: state of the game(player 2's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used to check if the game is over and then call the game over state after the first move made by player 2
*/
handle_player2_x_outcome(GameState, _, _) :-
    game_over(GameState, Winner),
    run_state(game_over, Winner).

/*
    parameters: state of the game(player 2's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used when the first move made by player 2 is not a game over move. It handles player 2's second move and then checks if the game is over or continue with player 1's turn
*/
handle_player2_x_outcome(GameState, ModeP1, ModeP2) :-
    \+ game_over(GameState, _),
    make_player2_move_o(GameState, ModeP2, NewGameStateOScored),
    handle_player2_o_outcome(NewGameStateOScored, ModeP1, ModeP2).

/*  
    parameters: state of the game(player 2's turn), player 1's mode
    returns: new game state
    this predicate is used to handle player 2's turn second move, choose a move and then update the game state with the move made and score, then display the game state
*/
make_player2_move_o(GameState, ModeP2, NewGameStateOScored) :-
    choose_move(GameState, ModeP2, 2, ((A2, B2), o)),
    move(GameState, ((A2, B2), o), NewGameStateO),
    change_score(NewGameStateO, ((A2, B2), o), GameStateOScored),
    change_turn(GameStateOScored, NewGameStateOScored),
    display_game(NewGameStateOScored).

/*
    parameters: state of the game(player 2's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used to check if the game is over and then call the game over state after the second move made by player 2
*/
handle_player2_o_outcome(GameState, _, _) :-
    game_over(GameState, Winner),
    run_state(game_over, Winner).

/*
    parameters: state of the game(player 2's turn), player 1's mode, player 2's mode
    returns: none
    this predicate is used when the second move made by player 2 is not a game over move. It calls player 1's turn
*/
handle_player2_o_outcome(GameState, ModeP1, ModeP2) :-
    \+ game_over(GameState, _),
    run_state(player1_turn, ModeP1, ModeP2, GameState).

/*
    parameters: none
    returns: none
    this predicate is used to start the game
*/
play :-
    run_state(setup, _).

/*
    parameters: cords of the move, List of moves
    returns: Result
    this predicate use recursion to check if the move is in the list of valid moves, return 1 if it is and 0 if it is not
*/
is_a_valid_move((Row, Col), [(Row, Col) | _], 1).
is_a_valid_move((_, _), [], 0).

is_a_valid_move((Row, Col), [(_, _) | Tail], Result):-
    is_a_valid_move((Row, Col), Tail, Result).

/*
    parameters: Board1, Board2, cords to update Board 1, cords to update Board 2, character to update the board
    returns: NewBoard1, NewBoard2
    this predicate is used to update the two boards with the new move made
*/
update_board(Board1, Board2, (A1, B1), (A2, B2), Char, NewBoard1, NewBoard2) :-
    replace_char_board_collum(Board1, B1, A1, 1, Char, NewBoard1),
    replace_char_board_collum(Board2, B2, A2, 1, Char, NewBoard2).

/*
    parameters: Board, Row, Col, Char
    returns: NewRow
    this predicate is used to find the row to update and then update the row with the new character by the usage of recursion
*/
replace_char_board_collum([Line|Tail], Row, Col, Col, Char, [New_Row|Tail]) :-
    replace_char_board_row(Line, Row, 1, Char, New_Row).

replace_char_board_collum([Line|Tail], Row, Col, Actual_Col, Char, [Line|Res]) :-
    Next_Col is Actual_Col + 1,
    replace_char_board_collum(Tail, Row, Col, Next_Col, Char, Res).

/*
    parameters: Row, Index, Char
    returns: NewRow
    this predicate is used to find the char to be updated in the row and then update the char with the new character by the usage of recursion
*/
replace_char_board_row([_|Tail], Row, Row, Char, [Char|Tail]).
replace_char_board_row([Head|Tail], Row, Actual_Index, Char, [Head|Res]) :-
    Next_Index is Actual_Index + 1,
    replace_char_board_row(Tail, Row, Next_Index, Char, Res).

/*
    parameters: Game state, move to make
    returns: new game state
    this predicate is used to make a move in the game state and then update the game state with the new move made. Checks if the move is valid before making the move
*/
move(game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), ((Row, Col), Char), game_state(Turn, Player1Score, Player2Score, NewBoard1, RowLetters1, ColNumbers1, NewBoard2, RowLetters2, ColNumbers2, Mode, Rows, Cols)) :- 
    valid_moves(game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), ListOfMoves),
    is_a_valid_move((Row, Col), ListOfMoves, 1),

    letter_number_to_coord((Row, Col), RowLetters1, ColNumbers1, (A1, B1)),
    letter_number_to_coord((Row, Col), RowLetters2, ColNumbers2, (A2, B2)),

    update_board(Board1, Board2, (A1, B1), (A2, B2), Char, NewBoard1, NewBoard2).

/*
    parameters: Game state
    returns: Winner(0 - draw, 1 - player 1, 2 - player 2)
    this predicate is used to check if the game is over based on the number of empty spaces in the board
*/
game_over(game_state(_, Score1, Score2, Board1, _, _, _, _, _, _, _, _), Winner) :-
    find_empty_spaces(Board1, EmptySpaces),
    length(EmptySpaces, 0),
    find_winner(Score1, Score2, Winner).

/*
    parameters: Score of player 1, Score of player 2, Winner
    returns: Winner(0 - draw, 1 - player 1, 2 - player 2)
    this predicate uses the score of the two players to find the winner of the game
*/
find_winner(Score1, Score2, 1) :-
    Score1 < Score2, !.
    
find_winner(Score1, Score2, 2) :-
    Score2 < Score1, !.

find_winner(_, _, 0).

/*
    parameters: Game state, mode of the player (1 - player 1, 2 - easy bot and 3 - medium bot), player number (1 - Player 1 and 2 - Player 2)
    returns: move to make
    this predicate is used to choose a move based on the mode of the player
*/
choose_move(GameState,  1,  Player, ((X, Y), Char)) :-
    read_user_input(Player, GameState, Char, (X, Y)).

choose_move(GameState,  2, _, ((X, Y), _)):-
    easy_bot_move(GameState, X, Y).

choose_move(GameState,  3, Player, ((X, Y), Char)) :-
    medium_bot_move(GameState, Player, Char, X, Y).