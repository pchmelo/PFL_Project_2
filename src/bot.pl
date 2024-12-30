

easy_bot_move(GameState, X, Y) :-
    valid_moves(GameState,  ListOfMoves),
    random_member((X, Y), ListOfMoves).



