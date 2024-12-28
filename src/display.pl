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
validate_rows(Rows) :-
    Rows >= 4,
    Rows =< 10.

validate_cols(Cols) :-
    Cols >= 4,
    Cols =< 10.

validate_mode(Mode) :-
    Mode >= 1,
    Mode =< 3.

% Get valid rows - success case
get_valid_rows(Rows) :-
    write('Enter number of rows (4-10): '),
    read(Rows),
    validate_rows(Rows).

% Get valid rows - failure case
get_valid_rows(Rows) :-
    write('Invalid number of rows! Must be between 4 and 10.'), nl,
    get_valid_rows(Rows).

% Get valid columns - success case
get_valid_cols(Cols) :-
    write('Enter number of columns (4-10): '),
    read(Cols),
    validate_cols(Cols).

% Get valid columns - failure case
get_valid_cols(Cols) :-
    write('Invalid number of columns! Must be between 4 and 10.'), nl,
    get_valid_cols(Cols).

% Get valid mode - success case
get_valid_mode(Mode) :-
    write('Select game mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Bot (Easy)'), nl,
    write('3. Player vs Bot (Medium)'), nl,
    read(Mode),
    validate_mode(Mode).

% Get valid mode - failure case
get_valid_mode(Mode) :-
    write('Invalid mode! Must be 1, 2 or 3.'), nl,
    get_valid_mode(Mode).

setup_game(GameState) :-
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
