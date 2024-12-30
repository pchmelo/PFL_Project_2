check_all_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    First_Y is Y - 3,
    (check_horizontal(Board, Total_Points, X, First_Y, Char, Total_Points_1) ; Total_Points_1 = Total_Points),

    format('Total_Points_1: ~w', [Total_Points_1]), nl,

    Second_Y is Y - 2,
    (check_horizontal(Board, Total_Points_1, X, Second_Y, Char, Total_Points_2) ; Total_Points_2 = Total_Points_1),

    format('Total_Points_2: ~w', [Total_Points_2]), nl,

    Third_Y is Y - 1,
    (check_horizontal(Board, Total_Points_2, X, Third_Y, Char, Total_Points_3) ; Total_Points_3 = Total_Points_2),

    format('Total_Points_3: ~w', [Total_Points_3]), nl,

    (check_horizontal(Board, Total_Points_3, X, Y, Char, Next_Total_Points) ; Next_Total_Points = Total_Points_3),

    format('Next_Total_Points: ~w', [Next_Total_Points]), nl.

% Check if there are 4 consecutive characters horizontally starting from given coordinates
check_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    nth0(X, Board, Row),
    check_horizontal_row(Row, Total_Points, Y, Char, 0, Next_Total_Points).

% Helper predicate to check 4 consecutive characters in a row
check_horizontal_row(_, Total_Points, _, _, 4, Next_Total_Points) :- % Base case: found 4 in a row
    Next_Total_Points is Total_Points + 1.

check_horizontal_row(Row, Total_Points, X, Char, Count, Next_Total_Points) :-
    nth0(X, Row, Char),
    NewCount is Count + 1,
    NX is X + 1,
    check_horizontal_row(Row, Total_Points, NX, Char, NewCount, Next_Total_Points).


% Check all vertical positions
check_all_vertical(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    First_X is X - 3,
    (check_vertical(Board, Total_Points, First_X, Y, Char, Total_Points_1) ; Total_Points_1 = Total_Points),

    format('Total_Points_1: ~w', [Total_Points_1]), nl,

    Second_X is X - 2,
    (check_vertical(Board, Total_Points_1, Second_X, Y, Char, Total_Points_2) ; Total_Points_2 = Total_Points_1),

    format('Total_Points_2: ~w', [Total_Points_2]), nl,

    Third_X is X - 1,
    (check_vertical(Board, Total_Points_2, Third_X, Y, Char, Total_Points_3) ; Total_Points_3 = Total_Points_2),

    format('Total_Points_3: ~w', [Total_Points_3]), nl,

    (check_vertical(Board, Total_Points_3, X, Y, Char, Next_Total_Points) ; Next_Total_Points = Total_Points_3),

    format('Next_Total_Points: ~w', [Next_Total_Points]), nl.


% Check if there are 4 consecutive characters vertically starting from given coordinates
check_vertical(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_vertical_column(Board, Total_Points, X, Y, Char, 0, Next_Total_Points).

% Helper predicate to check 4 consecutive characters in a column
check_vertical_column(_, Total_Points, _, _, _, 4, Next_Total_Points) :- % Base case: found 4 in a row
    Next_Total_Points is Total_Points + 1.

check_vertical_column(Board, Total_Points, X, Y, Char, Count, Next_Total_Points) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Char),
    NewCount is Count + 1,
    NX is X + 1,
    check_vertical_column(Board, Total_Points, NX, Y, Char, NewCount, Next_Total_Points).



sample_board([
    [empty, x, x, x, x, x],
    [empty, x, empty, o, empty, empty],
    [o, x, o, empty, empty, empty],
    [empty, x, empty, empty, empty, empty],
    [empty, x, empty, empty, x, empty],
    [x, empty, empty, empty, empty, empty]
]).


