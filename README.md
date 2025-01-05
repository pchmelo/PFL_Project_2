# Doblin Group 4

## Group Members

 - Gabriel da Costa Filipe Carvalho (202208939)
 - Vasco Ferreira da Rocha Melo (202207564)

## Contribution

### Gabriel Carvalho

Worked on:

 - game rules for score calculation
 - input and validation
 - management and refactoring of the code to ensure it complied with established rules

**Overall Contribution - 50%**

****

### Vasco Melo

Worked on:

 - main game rules and setup
 - display game and symbol placement
 - easy and medium bot

**Overall Contribution - 50%**

## Installation and Execution

In order to play the game, do the following:

> 1. Download `PFL_TP2_T12_Doblin_04.zip` and extract its contents
> 2. Open **SICStus Prolog** and consult the file `game.pl` inside the `src` directory
> 3. Write `play.` and the game will start

## Brief Description

Doblin is a unique two-player board game played on a pair of boards. At the beginning of the game, the rows and columns of both boards are randomly scrambled, creating different starting positions for each player.

During their turn, each player must place both an `x` and an `o` on their own board. These same symbols are then automatically placed in the corresponding positions on their opponent's board.

Points are scored when a player creates a line of four matching symbols (either x's or o's) on their board. These lines can be horizontal, vertical, or diagonal. Importantly, players want to avoid scoring points, as the goal is to end up with the lowest score.

The game continues until all cells on both boards have been filled. Once the boards are complete, the player with the fewest points is declared the winner.

### References used:

 - [Oficial Page](https://boardgamegeek.com/boardgame/308153/doblin)
 - [Game Rules](https://www.reddit.com/media?url=https%3A%2F%2Fpreview.redd.it%2F7qs2n14jszu41.png%3Fwidth%3D8910%26format%3Dpng%26auto%3Dwebp%26s%3Db85ce17764d414ba7cefeaeae0a9298aae8b8b50)

## Game Extensions

When exploring ways to enhance the game's complexity, we initially contemplated adding more boards for extra players as shown in the game rules reference. However, since the project requirements limited us to two players, we took a different approach.

Instead, we implemented two other changes, also stated in the game rules in case of wanted complexity: First, we allowed players to customize their board size by selecting the number of rows and collums. Second, we introduced a new scoring rule where forming a 2x2 square of matching symbols (all x's or all o's) would also award one point, in addition to the existing line-based scoring system.

## Game Logic

### Game Configuration Representation

The game configuration is represented by a list containing the number of rows, columns, and the game mode. This configuration is used by the `initial_state/2` predicate to set up the initial game state. The `initial_state/2` predicate generates empty boards for both players, assigns random row letters and column numbers, and initializes the game state with the current player, scores, and other necessary information.

 - Generating Empty Boards:

The `generate_empty_board/3` predicate creates an empty board with the specified number of rows and columns.

``` Prolog
generate_empty_board(Rows, Cols, Board) :-
    length(Board, Rows),
    maplist(generate_empty_row(Cols), Board).

generate_empty_row(Cols, Row) :-
    length(Row, Cols),
    maplist(=(empty), Row).
```

**generate_empty_board/3**: Creates a list of lists, where each sublist represents a row filled with `empty` atoms.

**generate_empty_row/2:** Fills a row with `empty` atoms.

 - Generating Row Letters and Column Numbers:

The `generate_row_letters/2` and `generate_col_numbers/2` predicates generate labels for the rows and columns through the use of **random_permutation**.

``` Prolog
generate_row_letters(N, Letters) :-
    findall(L, (between(1, N, X), 
                char_code('A', Start), 
                Code is Start + X - 1, 
                char_code(L, Code)), 
            Alphabet),
    random_permutation(Alphabet, Letters).

generate_col_numbers(N, Numbers) :-
    numlist(1, N, List),
    random_permutation(List, Numbers).
```

 - Example Configuration:

``` Prolog
initial_state([5, 6, 12], GameState)
```

This represents a game configuration with 5 rows, 6 columns, and game mode 12 (Human vs Bot Easy).

### Internal Game State Representation

The game state is represented by a compound term `game_state/12` which includes the following information:

 - Current player: `integer` (1 or 2)
 - Player 1's score: `integer`
 - Player 2's score `integer`
 - Player 1's board `list of lists`
 - Player 1's row letters `list of atoms`
 - Player 1's column numbers `list of integers`
 - Player 2's board `list of lists`
 - Player 2's row letters `list of atoms`
 - Player 2's column numbers `list of integers`
 - Game mode: `integer` (a two digit number, each indicating the game mode for the respective player, e.g., 13 - Human vs Bot Medium)
 - Number of rows: `integer`
 - Number of columns: `integer`

Each board is represented as a list of lists, where each sublist represents a row of the board. Empty cells are represented by the atom `empty`, and player pieces are represented by `x` or `o`.

**Example of Game State Representations:**

 - Initial State:

``` Prolog
game_state(
    CurrentPlayer = 1,
    ScorePlayer1 = 0, 
    ScorePlayer2 = 0,
    Board1 = [
        [empty, empty, empty, empty],
        [empty, empty, empty, empty],
        [empty, empty, empty, empty],
        [empty, empty, empty, empty]
    ],
    RowLetters1 = ['A', 'D', 'C', 'B'],
    ColNumbers1 = [1, 2, 3, 4],
    Board2 = [
        [empty, empty, empty, empty],
        [empty, empty, empty, empty],
        [empty, empty, empty, empty],
        [empty, empty, empty, empty]
    ],
    RowLetters2 = ['B', 'A', 'C', 'D'],
    ColNumbers2 = [2, 3, 1, 4],
    Mode = 33 (Bot Medium vs Bot Medium)
    Rows = 4,
    Cols = 4,
    )
```

 - Intermediate State

``` Prolog
game_state(
    CurrentPlayer = 1,
    ScorePlayer1 = 0, 
    ScorePlayer2 = 1,
    Board1 = [
        [empty, o, x, empty],
        [o, o, empty, x],
        [x, empty, empty, empty],
        [o, x, empty, x]
    ],
    RowLetters1 = ['A', 'D', 'C', 'B'],
    ColNumbers1 = [1, 2, 3, 4],
    Board2 = [
        [x, empty, o, x],
        [o, x, empty, empty],
        [empty, empty, x, empty],
        [o, empty, o, x]
    ],
    RowLetters2 = ['B', 'A', 'C', 'D'],
    ColNumbers2 = [2, 3, 1, 4],
    Mode = 33 (Bot Medium vs Bot Medium)
    Rows = 4,
    Cols = 4,
    )
```

 - Final State

``` Prolog
game_state(
    CurrentPlayer = 1,
    ScorePlayer1 = 2, 
    ScorePlayer2 = 1,
    Board1 = [
        [o, o, x, o],
        [o, o, o, x],
        [x, x, x, o],
        [o, x, x, x]
    ],
    RowLetters1 = ['A', 'D', 'C', 'B'],
    ColNumbers1 = [1, 2, 3, 4],
    Board2 = [
        [x, x, o, x],
        [o, x, o, o],
        [x, x, x, o],
        [o, o, o, x]
    ],
    RowLetters2 = ['B', 'A', 'C', 'D'],
    ColNumbers2 = [2, 3, 1, 4],
    Mode = 33 (Bot Medium vs Bot Medium)
    Rows = 4,
    Cols = 4,
    )
```

### Move Representation

A move is represented by a pair of coordinates and a character, such as `((Row, Col), Char)`. The coordinates specify the location on the board, and the character (`x` or `o`) represents the player's piece. The `move/3` predicate uses this representation to update the game state by placing the piece on the board and checking for valid moves.

- Move Example:

``` Prolog
move(GameState, (('B', 3), x), NewGameState)
```

The move predicate will convert ('B', 3) into a valid coordinate with `letter_number_to_coord/4` which will be then passed along to `update_board/7` which will place 'x' in the coordinate ('B', 3).

### User Interaction

The game menu system allows players to set up the game by selecting the number of rows, columns, and game mode. Interaction with the user is performed through prompts and input validation. For example, when reading a move, the system ensures that the input coordinates are within the valid range and that the selected cell is empty. The `read_user_input/4` predicate handles user input and validates the move before updating the game state.

## Conclusions

This project strengthened our understanding of logic programming in Prolog by applying declarative programming principles to solve various challenges throughout development.

We encountered some notable limitations during implementation. Performance decreased slightly with larger board sizes due to the computationally intensive process of searching for valid moves and checking scores. Additionally, the restrictions on using imperative programming constructs like if-then-else statements made it difficult to implement robust error handling for invalid inputs, which could result in program crashes or unexpected behavior.

Our group identified several potential improvements for future iterations. With fewer restrictions, we could enhance error handling and implement more intuitive input methods, such as allowing players to specify moves using chess-like notation (e.g., "B3") rather than separate row and column numbers. We also envisioned expanding the game to include a solo mode or supporting multiple players with additional boards, as mentioned in the game rules reference.

## Bibliography

 - [SICStus Prolog Documentation](https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html/Prolog-Intro.html#Prolog-Intro)
