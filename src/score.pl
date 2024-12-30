check_all_score(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    (check_all_horizontal(Board, Total_Points, X, Y, Char, Total_Points_1) ; Total_Points_1 = Total_Points),
    format('Total_Points_1: ~w', [Total_Points_1]), nl,

    (check_all_vertical(Board, Total_Points_1, X, Y, Char, Total_Points_2) ; Total_Points_2 = Total_Points_1),
    format('Total_Points_2: ~w', [Total_Points_2]), nl,

    (check_all_diagonal(Board, Total_Points_2, X, Y, Char, Total_Points_3) ; Total_Points_3 = Total_Points_2),
    format('Total_Points_3: ~w', [Total_Points_3]), nl,

    (check_all_squares(Board, Total_Points_3, X, Y, Char, Next_Total_Points) ; Next_Total_Points = Total_Points_3),
    format('Next_Total_Points: ~w', [Next_Total_Points]), nl.


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


% Check if there are 4 consecutive characters diagonally (down-right) starting from given coordinates
check_diagonal_down_right(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_diagonal_down_right_row(Board, Total_Points, X, Y, Char, 0, Next_Total_Points).

% Helper predicate to check 4 consecutive characters in a diagonal (down-right)
check_diagonal_down_right_row(_, Total_Points, _, _, _, 4, Next_Total_Points) :- % Base case: found 4 in a row
    Next_Total_Points is Total_Points + 1.

check_diagonal_down_right_row(Board, Total_Points, X, Y, Char, Count, Next_Total_Points) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Char),
    NewCount is Count + 1,
    NX is X + 1,
    NY is Y + 1,
    check_diagonal_down_right_row(Board, Total_Points, NX, NY, Char, NewCount, Next_Total_Points).


% Check if there are 4 consecutive characters diagonally (up-right) starting from given coordinates
check_diagonal_up_right(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_diagonal_up_right_row(Board, Total_Points, X, Y, Char, 0, Next_Total_Points).

% Helper predicate to check 4 consecutive characters in a diagonal (up-right)
check_diagonal_up_right_row(_, Total_Points, _, _, _, 4, Next_Total_Points) :- % Base case: found 4 in a row
    Next_Total_Points is Total_Points + 1.

check_diagonal_up_right_row(Board, Total_Points, X, Y, Char, Count, Next_Total_Points) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Char),
    NewCount is Count + 1,
    NX is X + 1,
    NY is Y - 1,
    check_diagonal_up_right_row(Board, Total_Points, NX, NY, Char, NewCount, Next_Total_Points).

% Check all diagonal positions
check_all_diagonal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    First_X is X - 3,
    First_Y is Y - 3,
    (check_diagonal_down_right(Board, Total_Points, First_X, First_Y, Char, Total_Points_1) ; Total_Points_1 = Total_Points),
    format('Total_Points_1: ~w', [Total_Points_1]), nl,

    Second_X is X - 2,
    Second_Y is Y - 2,
    (check_diagonal_down_right(Board, Total_Points_1, Second_X, Second_Y, Char, Total_Points_2) ; Total_Points_2 = Total_Points_1),
    format('Total_Points_2: ~w', [Total_Points_2]), nl,

    Third_X is X - 1,
    Third_Y is Y - 1,
    (check_diagonal_down_right(Board, Total_Points_2, Third_X, Third_Y, Char, Total_Points_3) ; Total_Points_3 = Total_Points_2),
    format('Total_Points_3: ~w', [Total_Points_3]), nl,

    (check_diagonal_down_right(Board, Total_Points_3, X, Y, Char, Next_Total_Points_1) ; Next_Total_Points_1 = Total_Points_3),
    format('Next_Total_Points_1: ~w', [Next_Total_Points_1]), nl,

    First_X_Up is X - 3,
    First_Y_Up is Y + 3,
    (check_diagonal_up_right(Board, Next_Total_Points_1, First_X_Up, First_Y_Up, Char, Total_Points_4) ; Total_Points_4 = Next_Total_Points_1),
    format('Total_Points_4: ~w', [Total_Points_4]), nl,

    Second_X_Up is X - 2,
    Second_Y_Up is Y + 2,
    (check_diagonal_up_right(Board, Total_Points_4, Second_X_Up, Second_Y_Up, Char, Total_Points_5) ; Total_Points_5 = Total_Points_4),
    format('Total_Points_5: ~w', [Total_Points_5]), nl,

    Third_X_Up is X - 1,
    Third_Y_Up is Y + 1,
    (check_diagonal_up_right(Board, Total_Points_5, Third_X_Up, Third_Y_Up, Char, Total_Points_6) ; Total_Points_6 = Total_Points_5),
    format('Total_Points_6: ~w', [Total_Points_6]), nl,

    (check_diagonal_up_right(Board, Total_Points_6, X, Y, Char, Next_Total_Points) ; Next_Total_Points = Total_Points_6),
    format('Next_Total_Points: ~w', [Next_Total_Points]), nl.

% Check if there is a 2x2 square of the same character starting from given coordinates
check_square(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    NX is X + 1,
    NY is Y + 1,
    nth0(X, Board, Row),
    nth0(Y, Row, Char),
    nth0(NX, Board, NextRow),
    nth0(Y, NextRow, Char),
    nth0(X, Board, Row2),
    nth0(NY, Row2, Char),
    nth0(NX, Board, NextRow2),
    nth0(NY, NextRow2, Char),
    Next_Total_Points is Total_Points + 1.

% Check all square positions
check_all_squares(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    First_X is X - 1,
    First_Y is Y - 1,
    (check_square(Board, Total_Points, First_X, First_Y, Char, Total_Points_1) ; Total_Points_1 = Total_Points),
    format('Total_Points_1: ~w', [Total_Points_1]), nl,

    Second_X is X,
    Second_Y is Y - 1,
    (check_square(Board, Total_Points_1, Second_X, Second_Y, Char, Total_Points_2) ; Total_Points_2 = Total_Points_1),
    format('Total_Points_2: ~w', [Total_Points_2]), nl,

    Third_X is X - 1,
    Third_Y is Y,
    (check_square(Board, Total_Points_2, Third_X, Third_Y, Char, Total_Points_3) ; Total_Points_3 = Total_Points_2),
    format('Total_Points_3: ~w', [Total_Points_3]), nl,

    (check_square(Board, Total_Points_3, X, Y, Char, Next_Total_Points) ; Next_Total_Points = Total_Points_3),
    format('Next_Total_Points: ~w', [Next_Total_Points]), nl.


sample_board([
    [x, x, x, e, x, e],
    [x, x, x, x, e, e],
    [x, x, x, o, e, e],
    [e, x, e, x, e, e],
    [x, e, e, e, x, e],
    [e, e, e, e, e, e]
]).


