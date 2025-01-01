check_all_score(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_score_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points).

% Helper predicate to check all horizontal, vertical, diagonal, and square positions
check_all_score_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_horizontal(Board, Total_Points, X, Y, Char, Total_Points_1),
    check_all_score_vertical(Board, Total_Points_1, X, Y, Char, Next_Total_Points).

check_all_score_vertical(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_vertical(Board, Total_Points, X, Y, Char, Total_Points_2),
    check_all_score_diagonal(Board, Total_Points_2, X, Y, Char, Next_Total_Points).

check_all_score_diagonal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_diagonal(Board, Total_Points, X, Y, Char, Total_Points_3),
    check_all_score_square(Board, Total_Points_3, X, Y, Char, Next_Total_Points).

check_all_score_square(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_square(Board, Total_Points, X, Y, Char, Next_Total_Points).

% Check all horizontal positions
check_all_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_horizontal_range(Board, Total_Points, X, Y, Char, -3, Next_Total_Points).

% Helper predicate to check a range of horizontal lines
check_horizontal_range(Board, Total_Points, X, Y, Char, Offset, Next_Total_Points) :-
    Offset =< 0,
    New_Y is Y + Offset,
    check_horizontal(Board, Total_Points, X, New_Y, Char, Updated_Points),
    Next_Offset is Offset + 1,
    check_horizontal_range(Board, Updated_Points, X, Y, Char, Next_Offset, Next_Total_Points).

check_horizontal_range(_, Total_Points, _, _, _, 1, Total_Points).

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

check_horizontal_row(_, Total_Points, _, _, _, Total_Points).

% Check all vertical positions
check_all_vertical(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_vertical_range(Board, Total_Points, X, Y, Char, -3, Next_Total_Points).

% Helper predicate to check a range of vertical lines
check_vertical_range(Board, Total_Points, X, Y, Char, Offset, Next_Total_Points) :-
    Offset =< 0,
    New_X is X + Offset,
    check_vertical(Board, Total_Points, New_X, Y, Char, Updated_Points),
    Next_Offset is Offset + 1,
    check_vertical_range(Board, Updated_Points, X, Y, Char, Next_Offset, Next_Total_Points).

check_vertical_range(_, Total_Points, _, _, _, 1, Total_Points).

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

check_vertical_column(_, Total_Points, _, _, _, _, Total_Points).

% Check all diagonal positions
check_all_diagonal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_diagonal_range(Board, Total_Points, X, Y, Char, -3, Next_Total_Points).

% Helper predicate to check a range of diagonal lines
check_diagonal_range(Board, Total_Points, X, Y, Char, Offset, Next_Total_Points) :-
    Offset =< 0,
    New_X_Down is X + Offset,
    New_Y_Down is Y + Offset,
    check_diagonal_down_right(Board, Total_Points, New_X_Down, New_Y_Down, Char, Updated_Points_1),
    New_X_Up is X + Offset,
    New_Y_Up is Y - Offset,
    check_diagonal_up_right(Board, Updated_Points_1, New_X_Up, New_Y_Up, Char, Updated_Points_2),
    Next_Offset is Offset + 1,
    check_diagonal_range(Board, Updated_Points_2, X, Y, Char, Next_Offset, Next_Total_Points).

check_diagonal_range(_, Total_Points, _, _, _, 1, Total_Points).

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

check_diagonal_down_right_row(_, Total_Points, _, _, _, _, Total_Points).

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

check_diagonal_up_right_row(_, Total_Points, _, _, _, _, Total_Points).

% Check all square positions
check_all_square(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_square_range(Board, Total_Points, X, Y, Char, -1, -1, Next_Total_Points).

% Helper predicate to check a range of square positions
check_square_range(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points) :-
    OffsetX =< 0,
    OffsetY =< 0,
    New_X is X + OffsetX,
    New_Y is Y + OffsetY,
    check_square(Board, Total_Points, New_X, New_Y, Char, Updated_Points),
    Next_OffsetY is OffsetY + 1,
    check_square_range_y(Board, Updated_Points, X, Y, Char, OffsetX, Next_OffsetY, Next_Total_Points).

check_square_range_y(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points) :-
    OffsetY =< 0,
    check_square_range(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points).

check_square_range_y(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points) :-
    OffsetY > 0,
    Next_OffsetX is OffsetX + 1,
    check_square_range(Board, Total_Points, X, Y, Char, Next_OffsetX, -1, Next_Total_Points).

check_square_range(_, Total_Points, _, _, _, 1, _, Total_Points).
check_square_range(_, Total_Points, _, _, _, _, 1, Total_Points).

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

check_square(_, Total_Points, _, _, _, Total_Points).

sample_board([
    [x, x, x, o, x, o],
    [x, x, x, o, x, o],
    [x, x, x, x, o, o],
    [o, o, x, x, o, o],
    [x, x, o, o, o, o],
    [o, o, o, o, o, x]
]).
