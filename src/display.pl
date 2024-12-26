display_game(game_state(Turn, Score1, Score2, Board1, Rows1, Cols1, Board2, Rows2, Cols2, Mode, _, _)) :-
    % Display scores and turn
    format('Turn: Player ~w     Mode: ~w~n', [Turn, Mode]),
    format('Scores - P1: ~w  P2: ~w~n~n', [Score1, Score2]),
    
    % Display player headers
    write('Player 1:                    Player 2:'), nl,
    
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