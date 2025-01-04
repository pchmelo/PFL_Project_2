/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all horizontal, vertical, diagonal, and square positions that may be affected by the move
*/
check_all_score(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_score_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all horizontal positions that may be affected by the move and then call the predicate to check all vertical positions
*/
check_all_score_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_horizontal(Board, Total_Points, X, Y, Char, Total_Points_1),
    check_all_score_vertical(Board, Total_Points_1, X, Y, Char, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all vertical positions that may be affected by the move and then call the predicate to check all diagonal positions
*/
check_all_score_vertical(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_vertical(Board, Total_Points, X, Y, Char, Total_Points_2),
    check_all_score_diagonal(Board, Total_Points_2, X, Y, Char, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all diagonal positions that may be affected by the move and then call the predicate to check all squares positions
*/
check_all_score_diagonal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_diagonal(Board, Total_Points, X, Y, Char, Total_Points_3),
    check_all_score_square(Board, Total_Points_3, X, Y, Char, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all square positions that may be affected by the move
*/
check_all_score_square(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_all_square(Board, Total_Points, X, Y, Char, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all horizontal positions that may be affected by the move. First checks the range of horizontal lines and then checks if there are 4 consecutive characters horizontally starting from given coordinates 
*/
check_all_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_horizontal_range(Board, Total_Points, X, Y, Char, -3, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, Offset
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking a range of horizontal lines that may be affected by the move. First checks if there are 4 consecutive characters horizontally starting from given coordinates and then calls the predicate to check the next horizontal line and then calls itself to check the next horizontal line until the offset is 1. For each horizontal line, it updates the total points. 
*/
check_horizontal_range(Board, Total_Points, X, Y, Char, Offset, Next_Total_Points) :-
    Offset =< 0,
    New_Y is Y + Offset,
    check_horizontal(Board, Total_Points, X, New_Y, Char, Updated_Points),
    Next_Offset is Offset + 1,
    check_horizontal_range(Board, Updated_Points, X, Y, Char, Next_Offset, Next_Total_Points).

check_horizontal_range(_, Total_Points, _, _, _, 1, Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the row in the board and then calls the predicate to check if there are 4 consecutive characters horizontally starting from given coordinates
*/
check_horizontal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    nth0(X, Board, Row),
    check_horizontal_row(Row, Total_Points, Y, Char, 0, Next_Total_Points).

/*
    parameters: Row, Total_Points, X, Char, Count
    returns: Next_Total_Points
    this predicate checks if there are 4 consecutive characters horizontally starting from given coordinates. If it finds 4 consecutive characters, it updates the total points. It uses recursion to check the next character in the row. 
*/
check_horizontal_row(_, Total_Points, _, _, 4, Next_Total_Points) :- % Base case: found 4 in a row
    Next_Total_Points is Total_Points + 1.

check_horizontal_row(Row, Total_Points, X, Char, Count, Next_Total_Points) :-
    nth0(X, Row, Char),
    NewCount is Count + 1,
    NX is X + 1,
    check_horizontal_row(Row, Total_Points, NX, Char, NewCount, Next_Total_Points).

check_horizontal_row(_, Total_Points, _, _, _, Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all vertical positions that may be affected by the move. First checks the range of vertical lines and then checks if there are 4 consecutive characters vertically starting from given coordinates 
*/
check_all_vertical(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_vertical_range(Board, Total_Points, X, Y, Char, -3, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, Offset
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking a range of vertical lines that may be affected by the move. First checks if there are 4 consecutive characters vertically starting from given coordinates and then calls the predicate to check the next vertical line and then calls itself to check the next vertical line until the offset is 1. For each vertical line, it updates the total points. 
*/
check_vertical_range(Board, Total_Points, X, Y, Char, Offset, Next_Total_Points) :-
    Offset =< 0,
    New_X is X + Offset,
    check_vertical(Board, Total_Points, New_X, Y, Char, Updated_Points),
    Next_Offset is Offset + 1,
    check_vertical_range(Board, Updated_Points, X, Y, Char, Next_Offset, Next_Total_Points).

check_vertical_range(_, Total_Points, _, _, _, 1, Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the column in the board and then calls the predicate to check if there are 4 consecutive characters vertically starting from given coordinates
*/
check_vertical(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_vertical_column(Board, Total_Points, X, Y, Char, 0, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, Count
    returns: Next_Total_Points
    this predicate checks if there are 4 consecutive characters vertically starting from given coordinates. If it finds 4 consecutive characters, it updates the total points. It uses recursion to check the next character in the column. 
*/
check_vertical_column(_, Total_Points, _, _, _, 4, Next_Total_Points) :- % Base case: found 4 in a row
    Next_Total_Points is Total_Points + 1.

check_vertical_column(Board, Total_Points, X, Y, Char, Count, Next_Total_Points) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Char),
    NewCount is Count + 1,
    NX is X + 1,
    check_vertical_column(Board, Total_Points, NX, Y, Char, NewCount, Next_Total_Points).

check_vertical_column(_, Total_Points, _, _, _, _, Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all diagonal positions that may be affected by the move. First checks the range of diagonal lines and then checks if there are 4 consecutive characters diagonally starting from given coordinates 
*/
check_all_diagonal(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_diagonal_range(Board, Total_Points, X, Y, Char, -3, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, Offset
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking a range of diagonal lines that may be affected by the move. First checks if there are 4 consecutive characters diagonally starting from given coordinates and then calls itself to check the next diagonal line until the offset is 1. For each diagonal line, it updates the total points. It checks the two diagonals, the down right and the up right
*/
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

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate call the predicate to check if there are 4 consecutive characters diagonally (down right) starting from given coordinates
*/
check_diagonal_down_right(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_diagonal_down_right_row(Board, Total_Points, X, Y, Char, 0, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, Count
    returns: Next_Total_Points
    this predicate checks if there are 4 consecutive characters diagonally (down right) starting from given coordinates. If it finds 4 consecutive characters, it updates the total points. It uses recursion to check the next character in the diagonal. 
*/
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

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate call the predicate to check if there are 4 consecutive characters diagonally (up right) starting from given coordinates
*/
check_diagonal_up_right(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_diagonal_up_right_row(Board, Total_Points, X, Y, Char, 0, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, Count
    returns: Next_Total_Points
    this predicate checks if there are 4 consecutive characters diagonally (up right) starting from given coordinates. If it finds 4 consecutive characters, it updates the total points. It uses recursion to check the next character in the diagonal. 
*/
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

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate find the total points obtained by the move ((X, Y), Char) in the board by checking all squares positions that may be affected by the move. First checks the range of squares and then checks if there are characters from given coordinates that form a square  
*/
check_all_square(Board, Total_Points, X, Y, Char, Next_Total_Points) :-
    check_square_range(Board, Total_Points, X, Y, Char, -1, -1, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, OffsetX, OffsetY
    returns: Next_Total_Points
    This predicate checks for squares starting from the given coordinates (X, Y) with the specified character (Char) and updates the total points. It iterates through the board using the offsets OffsetX and OffsetY.
*/
check_square_range(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points) :-
    OffsetX =< 0,
    OffsetY =< 0,
    New_X is X + OffsetX,
    New_Y is Y + OffsetY,
    check_square(Board, Total_Points, New_X, New_Y, Char, Updated_Points),
    Next_OffsetY is OffsetY + 1,
    check_square_range_y(Board, Updated_Points, X, Y, Char, OffsetX, Next_OffsetY, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, OffsetX, OffsetY
    returns: Next_Total_Points
    This predicate handles the iteration through the Y offsets. If OffsetY is less than or equal to 0, it continues checking squares in the current X offset.
*/
check_square_range_y(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points) :-
    OffsetY =< 0,
    check_square_range(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, OffsetX, OffsetY
    returns: Next_Total_Points
    This predicate handles the iteration through the Y offsets. If OffsetY is greater than 0, it increments the X offset and resets the Y offset to -1 to continue checking squares.
*/
check_square_range_y(Board, Total_Points, X, Y, Char, OffsetX, OffsetY, Next_Total_Points) :-
    OffsetY > 0,
    Next_OffsetX is OffsetX + 1,
    check_square_range(Board, Total_Points, X, Y, Char, Next_OffsetX, -1, Next_Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, OffsetX, OffsetY
    returns: Total_Points
    Base case for the recursion. If OffsetX is 1, it returns the current total points.
*/
check_square_range(_, Total_Points, _, _, _, 1, _, Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char, OffsetX, OffsetY
    returns: Total_Points
    Base case for the recursion. If OffsetY is 1, it returns the current total points.
*/
check_square_range(_, Total_Points, _, _, _, _, 1, Total_Points).

/*
    parameters: Board, Total_Points, X, Y, Char
    returns: Next_Total_Points
    this predicate checks if there is a square of 4 characters starting from given coordinates. If it finds 4 characters, it updates the total points.
*/
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
