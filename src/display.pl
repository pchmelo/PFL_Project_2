/*
    parameters: GameState
    return: none
    this predicate displays the game state in the console. Displays the turn, mode, score and the two boards 
*/
display_game(game_state(Turn, Score1, Score2, Board1, Rows1, Cols1, Board2, Rows2, Cols2, Mode, _, _)) :-
    write('DISPLAY GAME'), nl,
    format('Turn: Player ~w     Mode: ~w~n', [Turn, Mode]),
    format('Scores - P1: ~w  P2: ~w~n~n', [Score1, Score2]),
    
    write('  '),
    print_cols_numbers(Cols1),
    write(' || '),
    print_cols_numbers(Cols2),
    nl,
    
    print_boards(Board1, Board2, Rows1, Rows2).

/*
    parameters: List of the numbers of the columns
    return: none
    this predicate prints the numbers of the columns in the console by the usage of recursion
*/
print_cols_numbers([]).
print_cols_numbers([Col|Rest]) :-
    format(' ~w ', [Col]),
    print_cols_numbers(Rest).

/*
    parameters: Board1, Board2, List of the letters of the rows of the board1, List of the letters of the rows of the board2
    return: none
    this predicate is responsible for printing the boards in the console by row with the respective letters of the rows
*/
print_boards([], [], [], []).
print_boards([Row1|Rest1], [Row2|Rest2], [Letter1|Letters1], [Letter2|Letters2]) :-
    format('~w ', [Letter1]),
    print_row(Row1),
    write(' || '),
    print_row(Row2),
    format(' ~w', [Letter2]),
    nl,
    print_boards(Rest1, Rest2, Letters1, Letters2).

/*
    parameters: Row
    return: none
    this predicate returns the row in the console by the usage of recursion
*/
print_row([]).
print_row([Cell|Rest]) :-
    print_cell(Cell),
    print_row(Rest).

/*
    parameters: Cell(Char, in case of empty cell prints '.')
    return: none
    this predicate prints the cell in the console
*/
print_cell(empty) :-
    write(' . ').

print_cell(Cell) :-
    Cell \= empty,
    format(' ~w ', [Cell]).

/*
    parameters: GameState
    return: none
    this predicate displays all the parameters of the game state in the console
*/
display_state_of_game(game_state(Turn, Score1, Score2, Board1, Rows1, Cols1, Board2, Rows2, Cols2, Mode, Rows, Cols)):-
    nl,
    write('----- Game State -----'), nl,
    display_game(game_state(Turn, Score1, Score2, Board1, Rows1, Cols1, Board2, Rows2, Cols2, Mode, Rows, Cols)),
    
    write('Player 1:'), nl,
    format('  Rows: ~w~n', [Rows1]),
    format('  Cols: ~w~n', [Cols1]),
    write('Player 2:'), nl,
    format('  Rows: ~w~n', [Rows2]),
    format('  Cols: ~w~n', [Cols2]),
    nl,
    
    format('Game Mode: ~w~n', [Mode]),
    format('Board Size: ~w x ~w~n', [Rows, Cols]),
    nl.


/*
    parameters: none
    return: Value
    this predicate reads a number from the console and returns it
*/
read_number(Value) :-
    read_line(Input),
    number_codes(Value, Input).

/*
    parameters: Min, Max, Context
    return: Value
    this predicate receives a minimum value, a maximum value and a context to display in the console. It reads a number from the console and validates if it is between the minimum and maximum values
*/
get_option(Min, Max, Context, Value) :-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

/*
    parameters: none
    return: Rows
    this predicate get the number of rows from the console
*/
get_valid_rows(Rows) :-
    get_option(4, 10, 'Enter number of rows', Rows).

/*
    parameters: none
    return: Cols
    this predicate get the number of columns from the console
*/
get_valid_cols(Cols) :-
    get_option(4, 9, 'Enter number of columns', Cols).

/*
    parameters: Player
    return: Mode
    this predicate get the mode from the console for the player
*/
get_valid_mode(1, Mode) :-
    get_option(1, 3, 'Select Player 1 (1. Player, 2. Bot(Easy), 3. Bot(Medium))', Mode).

get_valid_mode(2, Mode) :-
    get_option(1, 3, 'Select Player 2 (1. Player, 2. Bot(Easy), 3. Bot(Medium))', Mode).

/*
    parameters: none
    return: GameState, ModeP1, ModeP2
    this predicate sets up the game by getting the number of rows, columns and the mode for the players. It initializes the game state and displays the game
*/
setup_game(GameState, ModeP1, ModeP2) :-
    write('Start Game'),nl,
    
    get_valid_rows(Rows),
    get_valid_cols(Cols),
    get_valid_mode(1, ModeP1),
    get_valid_mode(2, ModeP2),
    Mode is ModeP1 * 10 + ModeP2,
    
    initial_state([Rows, Cols, Mode], GameState),

    format('ModeP1: ~w ModeP2: ~w', [ModeP1, ModeP2]), nl,

    display_game(GameState).

/*
    parameters: Coords;
    return: none
    this predicate display a list of coordinates in the console
*/
display_coords(Coords) :-
    write('Coordinates: '), nl,
    display_formatted_coords(Coords).

/*
    parameters: List of coordinates
    return: none
    this predicate displays a coordinate from the list in the console by the usage of recursion
*/
display_formatted_coords([]).
display_formatted_coords([(Row, Col)|Rest]) :-
    format('(~w, ~w)', [Row, Col]), nl,
    display_formatted_coords(Rest).
    
/*
    parameters: Board, RowLetters, ColNumbers
    return: none
    this predicate displays the board in the console and the respective row letters and column numbers
*/
display_board(Board, RowLetters, ColNumbers) :-
    nl,
    write('  '),
    display_col_numbers(ColNumbers),
    nl,
    display_rows(Board, RowLetters, 1).

/*
    parameters: List of column numbers
    return: none
    this predicate displays the column numbers in the console by the usage of recursion
*/
display_col_numbers([]).
display_col_numbers([Col|RestCols]) :-
    format(' ~w', [Col]),
    display_col_numbers(RestCols).

/*
    parameters: Board, RowLetters, RowIndex
    return: none
    this predicate displays the rows of the board in the console by the usage of recursion
*/
display_rows([], _, _).
display_rows([Row|RestRows], RowLetters, RowIndex) :-
    nth1(RowIndex, RowLetters, RowLetter),
    format('~w ', [RowLetter]),
    display_row(Row),
    nl,
    NextRowIndex is RowIndex + 1,
    display_rows(RestRows, RowLetters, NextRowIndex).

/*
    parameters: Row
    return: none
    this predicate displays a row in the console by the usage of recursion
*/
display_row([]).
display_row([Cell|RestCells]) :-
    display_cell(Cell),
    display_row(RestCells).

/*
    parameters: Cell
    return: none
    this predicate displays a cell in the console
*/
display_cell(empty) :-
    write('. ').
display_cell(Cell) :-
    format('~w ', [Cell]).

/*
    parameters: RowLetter, Rows
    return: none
    this predicate validates if the row letter is within the possible range
*/
validate_row_letter(RowLetter, Rows) :-
    char_code(RowLetter, RowCode),
    char_code('A', ACode),
    MaxRowCode is ACode + Rows - 1,
    RowCode >= ACode,
    RowCode =< MaxRowCode.

/*
    parameters: ColNumber, Cols
    return: none
    this predicate validates if the column number is within the possible range
*/
validate_col_number(ColNumber, Cols) :-
    between(1, Cols, ColNumber).

/*
    parameters: Player, GameState, Char
    return: coords
    this predicate reads the user input from the console and validates it
*/
read_user_input(Player, game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), Char, (A1, B1)) :-
    format('Player ~w\'s turn', [Player]), nl,
    repeat,
    format('Choose the letter of the line to put the ~w: ', [Char]), nl,
    read_line(Line1),
    atom_codes(Line1Atom, Line1),
    atom_chars(Line1Atom, [A1|_]), % Extract the first character
    validate_row_letter(A1, Rows),
    format('Choose the number of the column to put the ~w: ', [Char]), nl,
    read_line(Line2),
    number_codes(B1, Line2),
    validate_col_number(B1, Cols),
    validate_move((A1, B1), game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)).

/*
    parameters: Coordinates, GameState
    return: none
    this predicate validate if the move is possible based on the coordinates 
*/
validate_move((A1, B1), game_state(_, _, _, Board1, RowLetters1, ColNumbers1, _, _, _, _, _, _)) :-
    nth1(RowIndex, RowLetters1, A1),
    nth1(ColIndex, ColNumbers1, B1),
    nth1(RowIndex, Board1, Row),
    nth1(ColIndex, Row, empty).

/*
    parameters: none
    return: Option
    this predicate reads the option from the console and validates it if the user wants to play again or exit
*/
replay_game(Option):-
    get_option(1, 2, '1- Replay  2- Exit', Option).
