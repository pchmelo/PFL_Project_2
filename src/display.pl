display_game(game_state(Turn, Score1, Score2, Board1, Rows1, Cols1, Board2, Rows2, Cols2, Mode, _, _)) :-
    write('DISPLAY GAME'), nl,
    % Display scores and turn
    format('Turn: Player ~w     Mode: ~w~n', [Turn, Mode]),
    format('Scores - P1: ~w  P2: ~w~n~n', [Score1, Score2]),
    
    % Print column numbers for both boards
    write('  '),
    print_cols_numbers(Cols1),
    write(' || '),
    print_cols_numbers(Cols2),
    nl,
    
    % Print boards side by side
    print_boards(Board1, Board2, Rows1, Rows2).

print_cols_numbers([]).
print_cols_numbers([Col|Rest]) :-
    format(' ~w ', [Col]),
    print_cols_numbers(Rest).

print_boards([], [], [], []).
print_boards([Row1|Rest1], [Row2|Rest2], [Letter1|Letters1], [Letter2|Letters2]) :-
    format('~w ', [Letter1]),
    print_row(Row1),
    write(' || '),
    print_row(Row2),
    format(' ~w', [Letter2]),
    nl,
    print_boards(Rest1, Rest2, Letters1, Letters2).

print_row([]).
print_row([Cell|Rest]) :-
    (Cell = empty -> write(' . ') ; format(' ~w ', [Cell])),
    print_row(Rest).

display_state_of_game(game_state(Turn, Score1, Score2, Board1, Rows1, Cols1, Board2, Rows2, Cols2, Mode, Rows, Cols)):-
    nl,
    write('----- Game State -----'), nl,
    display_game(game_state(Turn, Score1, Score2, Board1, Rows1, Cols1, Board2, Rows2, Cols2, Mode, Rows, Cols)),
    write('-------------------'), nl,
    
    % Display scrambled configurations
    write('Scrambled Configurations:'), nl,
    write('Player 1:'), nl,
    format('  Rows: ~w~n', [Rows1]),
    format('  Cols: ~w~n', [Cols1]),
    write('Player 2:'), nl,
    format('  Rows: ~w~n', [Rows2]),
    format('  Cols: ~w~n', [Cols2]),
    nl,
    
    % Game info
    format('Game Mode: ~w~n', [Mode]),
    format('Board Size: ~w x ~w~n', [Rows, Cols]),
    nl.


% Validation predicates
read_number(Value) :-
    read(Value),
    number(Value).

% Get option within a range
% get_option(+Min, +Max, +Context, -Value)
% Unifies Value with the value given by user input between Min and Max when asked about Context
get_option(Min, Max, Context, Value) :-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

% Get valid rows
get_valid_rows(Rows) :-
    get_option(4, 10, 'Enter number of rows', Rows).

% Get valid columns
get_valid_cols(Cols) :-
    get_option(4, 10, 'Enter number of columns', Cols).

% Get valid mode
get_valid_mode(Mode) :-
    get_option(1, 3, 'Select game mode (1. Player vs Player, 2. Player vs Bot (Easy), 3. Player vs Bot (Medium))', Mode).


setup_game(GameState, Mode) :-
    write('Start Game'),nl,
    
    get_valid_rows(Rows),
    get_valid_cols(Cols),
    get_valid_mode(Mode),
    
    initial_state([Rows, Cols, Mode], GameState),
    display_game(GameState).

% Display a list of coordinates
display_coords(Coords) :-
    write('Coordinates: '), nl,
    display_formatted_coords(Coords).

% Helper predicate to display formatted coordinates
display_formatted_coords([]).
display_formatted_coords([(Row, Col)|Rest]) :-
    format('(~w, ~w)', [Row, Col]), nl,
    display_formatted_coords(Rest).
    

display_board(Board, RowLetters, ColNumbers) :-
    nl,
    write('  '),
    display_col_numbers(ColNumbers),
    nl,
    display_rows(Board, RowLetters, 1).

% Display column numbers
display_col_numbers([]).
display_col_numbers([Col|RestCols]) :-
    format(' ~w', [Col]),
    display_col_numbers(RestCols).

% Display each row with row index
display_rows([], _, _).
display_rows([Row|RestRows], RowLetters, RowIndex) :-
    nth1(RowIndex, RowLetters, RowLetter),
    format('~w ', [RowLetter]),
    display_row(Row),
    nl,
    NextRowIndex is RowIndex + 1,
    display_rows(RestRows, RowLetters, NextRowIndex).

% Display each cell in a row
display_row([]).
display_row([Cell|RestCells]) :-
    display_cell(Cell),
    display_row(RestCells).

% Display a single cell
display_cell(empty) :-
    write('. ').
display_cell(Cell) :-
    format('~w ', [Cell]).

% Validate if the row letter is within the possible range
validate_row_letter(RowLetter, Rows) :-
    char_code(RowLetter, RowCode),
    char_code('A', ACode),
    MaxRowCode is ACode + Rows - 1,
    RowCode >= ACode,
    RowCode =< MaxRowCode.

% Validate if the column number is within the possible range
validate_col_number(ColNumber, Cols) :-
    between(1, Cols, ColNumber).

% Read user input and validate
read_user_input(Player, game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols), Char, (A1, B1)) :-
    format('Player ~w\'s turn', [Player]), nl,
    repeat,
    format('Choose the letter of the line to put the ~w: ', [Char]), nl,
    get_char(_), % Consume the newline character
    get_char(A1),
    validate_row_letter(A1, Rows),
    format('Choose the number of the column to put the ~w: ', [Char]), nl,
    read(B1),
    validate_col_number(B1, Cols),
    validate_move((A1, B1), game_state(Turn, Player1Score, Player2Score, Board1, RowLetters1, ColNumbers1, Board2, RowLetters2, ColNumbers2, Mode, Rows, Cols)),
    format('A1: ~w, B1: ~w', [A1, B1]), nl.

% Validate if the move is valid
validate_move((A1, B1), game_state(_, _, _, Board1, RowLetters1, ColNumbers1, _, _, _, _, _, _)) :-
    nth1(RowIndex, RowLetters1, A1),
    nth1(ColIndex, ColNumbers1, B1),
    nth1(RowIndex, Board1, Row),
    nth1(ColIndex, Row, empty).
