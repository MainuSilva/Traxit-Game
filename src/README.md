# Traxit - SICStus Prolog Implementation
> Text-based implementation of the Traxit game in SICStusProlog for the course Logic and Functional Programming atFEUP

## Traxit_2

- Maria Leonor Ribeiro Laranjeira - up202004453
- Manuel Maria Faria de Sousa e Silva - up202108874

## Contributions

|**Name**|**Contribution**|
|---|---|
| Maria Laranjeira | 30% |
| Manuel Silva | 70% |

## Installation and Execution

No extra installation or configuration is required other than the standard  installation of SICStus Prolog 4.8. To run the game, it is only necessary to consult and compile the file `main.pl` and run the predicate `play.`

## Description of the Game

Traxit is an abstract strategy board game, where the goal is to be as close to the top as possible, at the right moment. 

The board consists of an 8 by 8 grid, with 4 levels, each 1 square wide, forming a pyramid. Each player has 2 pawns, and 10 unique path tiles. Both players start with their pawns on the lower level on the third and third-to-last positions of the outermost row, on opposite sides of the board.

The players must follow the path tiles avaliable, with the goal of moving their pawns to a higher level. The catch, however, is that it is the opponent who chooses which path the player must use. It is up to the player to rotate the given path in order to find the best move, giving the player up to 8 different moves per pawn for assymetrical paths. Once a path tile is used it is discarded and cannot be used again. The player cannot move through other pawns, nor outside the board, and if the player cannot move at all, the opponent takes one of the player's pawns and moves it to one of the corners in the lowest level. This move is called a Traxit, which is latin for being pulled.

In terms of scores, at the end of every 4 rounds (4, 8, 12 and 16) the players score points for the position of their pawns, depending on which level they are (0, 25, 50 or 100, for levels 1 to 4, respectively).

**Video Explanation**: https://www.youtube.com/watch?v=icDGTU7UdBw&ab_channel=Danspil 

**Traxit Overview**: https://boardgamegeek.com/boardgame/392652/traxit

**Traxit Instructions Manual***: Traxit_refler.pdf
    
**Note: The manual is in Danish, as we unfortunatelly weren't able to find an English version.*

## Game Logic

### Internal Game State Representation:

The overall game state is represented by the list **GameState** that contains the following 7 elements: <ins>Current player</ins>, <ins>Current board</ins>, <ins>Round</ins>, <ins>White Cards</ins>, <ins>Black Cards</ins>, <ins>White Score</ins> and <ins>Black Score</ins>, in this order.
- **Current Player**: An atom that represents the player that is currently playing. Is either white (w) or black (b)
- **Current Board**: A two dimensional list (a list of lists, therefore) that represents the top view of the board. Portrays the blank spaces ('o'), the pawns positions ( two 'w' and two 'b') and the blocked spaces ('x').
- **Round**: An integer that represents the current round of the game.
- **White Cards**: A list of the white player's remaining cards.
- **Black Cards**: A list of the black player's remaining cards.
- **White Score**: An integer that represents the white player's current score.
- **Black Score**: An integer that represents the black player's current score.

### Initial Game State
#### The Board
<pre><code>
  |  A B C D E F G H  |
  +-------------------+
8 |  o o b o o b o o  |
  +-------------------+
7 |  o o o o o o o o  |
  +-------------------+
6 |  o o x o o x o o  |
  +-------------------+
5 |  o o o o o o o o  |
  +-------------------+
4 |  o o o o o o o o  |
  +-------------------+
3 |  o o x o o x o o  |
  +-------------------+
2 |  o o o o o o o o  |
  +-------------------+
1 |  o o w o o w o o  |
  +-------------------+
</code></pre>

#### The Cards
<pre><code>
______________________________________________________________________
|      1      |      2      |      3      |      4      |      5      |
|     ___     |   _______   |         _   |  _________  |   _______   |
|    |.|.|    |  |.|_|_|_|  |   _____|.|  | |.|_|_|_|.| |  |_|_|_|_|  |
|             |        |.|  |  |.|_|_|_|  |             |  |.|   |.|  |
|_____________|_____________|_____________|_____________|_____________|
|      6      |      7      |      8      |      9      |      10     |
|    _____    |        _    |    _____    |   _______   |        _    |
|   |.|_|_|   |    ___|.|   |   |.|_|.|   |  |.|_|_|_|  |      _|.|   |
|       |.|   |   |.|_|_|   |             |        |_|  |     |.|     |
|_____________|_____________|_____________|________|.|__|_____________|
</code></pre>

### Mid-Game State
#### The Board
<pre><code>
  |  A B C D E F G H  |
  +-------------------+
8 |  o o o o o o b o  |
  +-------------------+
7 |  o o b o o o o o  |
  +-------------------+
6 |  o o x o o x o o  |
  +-------------------+
5 |  o o o o o o o o  |
  +-------------------+
4 |  o o o o w o o o  |
  +-------------------+
3 |  o o x w o x o o  |
  +-------------------+
2 |  o o o o o o o o  |
  +-------------------+
1 |  o o o o o o o o  |
  +-------------------+
</code></pre>

#### The Cards
<pre><code>
______________________________________________________________________ 
|      -      |      2      |      -      |      4      |      -      |
|     ___     |   _______   |         _   |  _________  |   _______   |
|    |.|.|    |  |.|_|_|_|  |   _____|.|  | |.|_|_|_|.| |  |_|_|_|_|  |
|             |        |.|  |  |.|_|_|_|  |             |  |.|   |.|  |
|_____________|_____________|_____________|_____________|_____________|
|      6      |      7      |      -      |      -      |      10     |
|    _____    |        _    |    _____    |   _______   |        _    |
|   |.|_|_|   |    ___|.|   |   |.|_|.|   |  |.|_|_|_|  |      _|.|   |
|       |.|   |   |.|_|_|   |             |        |_|  |     |.|     |
|_____________|_____________|_____________|________|.|__|_____________|
</code></pre>

### End-Game State
#### The Board

<pre><code>
  |  A B C D E F G H  |
  +-------------------+
8 |  o o o o o o o o  |
  +-------------------+
7 |  o o o o o o o o  |
  +-------------------+
6 |  o o x b o x o o  |
  +-------------------+
5 |  o o o o b o o o  |
  +-------------------+
4 |  o o o o w o o o  |
  +-------------------+
3 |  o o x o o x o o  |
  +-------------------+
2 |  o o o w o o o o  |
  +-------------------+
1 |  o o o o o o o o  |
  +-------------------+
</code></pre>

#### The Cards

<pre><code>
______________________________________________________________________ 
|      -      |      -      |      -      |      -      |      -      |
|     ___     |   _______   |         _   |  _________  |   _______   |
|    |.|.|    |  |.|_|_|_|  |   _____|.|  | |.|_|_|_|.| |  |_|_|_|_|  |
|             |        |.|  |  |.|_|_|_|  |             |  |.|   |.|  |
|_____________|_____________|_____________|_____________|_____________|
|      6      |      -      |      -      |      -      |      10     |
|    _____    |        _    |    _____    |   _______   |        _    |
|   |.|_|_|   |    ___|.|   |   |.|_|.|   |  |.|_|_|_|  |      _|.|   |
|       |.|   |   |.|_|_|   |             |        |_|  |     |.|     |
|_____________|_____________|_____________|________|.|__|_____________|

CONGRATULATIONs, White has won the game! Final Score: 450 - 225
</code></pre>

### Game State Visualization

> All the predicates concerning the Game UI and Display are located in the file `view.pl`. The predicates regarding user inputs, on the other hand, are located in the `main.pl` file. 

#### Input

User input is necessary before the game starts, in order to choose the gamemode and determine which player starts first. Afterwards, during the game, it is needed to choose path numbers and to choose pawn movements. These reading actions are performed by the predicates with prefix `get_` 
whose general structure consists of calling the predicate `read` and then checking if the input is valid. These predicates, in turn, are called by the predicates `menu` for the first two pre-game input situations, and `play_round` for the remaining.

The user must end all inputs with a "." and press <kbd>Enter</kbd>, so that it gets parsed.

> `get_player_mode`, `get_current_player`, `get_card`, `get_traxit_move`, `get_move`

#### Output

Every predicate in charge of displaying information to the user has the prefix `display_`. In order to display information, they make use of the predicates `write` and `format`. The elements printed in the terminal can be static, such as the game logo, or dynamic such as the game board. The predicates responsible for calling the display predicates are `menu`, for the logo and menu options and `play_game` (which calls `play_round` that of itself also calls display predicates) for the board, cards and game messages (input requests, the traxit notification and the winning notification). `play_game` and `menu` are in turn both called in the main predicate `play`. Input predicates are also in charge of displaying error messages in case of invalid input.
 Here are some examples:

**Menu UI**
> `display_logo`, `display_player_modes` ,`display_first_player`
<pre><code>
  _____                           _____ 
 |_   _|                         |_   _| 
   | |    __   __ _     __   __ _  | | 
   | | |   _ \ / _` | | '_ \ / _| | 
   | |_| | | | (_| | | | | | (_| | | |
   |___|_| |_|\__,_| |_| |_|\__,___|
Welcome to Traxit!

 ( Input the option number to select it ) 
 Choose the type of player for White: 
       1. Human Player            
       2. Easy Computer           
       3. Hard Computer           
|: 

 ( Input the option number to select it ) 
 Choose the type of player for Black: 
       1. Human Player            
       2. Easy Computer           
       3. Hard Computer           
|: 

Who plays first? (w/b): 
</code></pre>

**Input Requests**

> `display_choose_card`, `display_choose_move`, `display_traxit`
<pre><code>
Black player, choose a card for the white player: |: 1.

Choose a move in algebraic notation using card 1: e.g., c1-d3

White player, make your move: |: c1-b1.
</code></pre>

<pre><code>White player choose one of your adversary's pawns to move to one of the corners in algebraic notation (corners: a1, h1, a8, h8): 
</code></pre>

**Game Notifications**
> `display_period_score`, `display_traxit`, `display_winner`, `display_move`

<pre><code>END OF ROUND 1: Score: White: 100 - Black: 50</code></pre>
<pre><code>TRAXIT!</code></pre>
<pre><code>CONGRATULATIONs, White has won the game! Final Score: 450 - 225</code></pre>
<pre><code>Player White chooses the card 1</code></pre>


**Error Messages**
> `display_invalid_player`, and the input predicates

<pre><code>Invalid move!</code></pre>
<pre><code>Invalid algebraic notation. Try again</code></pre>
<pre><code>Invalid Mode. Try again</code></pre>
<pre><code>Invalid input. Defaulting to white.</code></pre>
<pre><code>Invalid card number. Try Again</code></pre>

The board and cards display, as showcased previously in the game states, are dynamic and change according to the game state. The board display is performed by the predicate `display_board` which calls the predicate `display_rows` for each row of the board. The pieces are placed on the board and displayed by the predicate `display_piece`.

In case of a game against the computer, the computer's card choice is displayed, as shown in the last element of "**Game Notifications**". After a card is attributed to the computer, a new board is printed with the computer chosen move already performed.

### Move Validation and Execution

Standard Moves are executed by the predicate `move`, which given a Game State and a Move (specified in the form of "NumberCard-CurrentPosition-EndPosition") returns the resulting new Game State.
A new move is only executed, however, firstly after being syntactically validated and parsed by `get_move` and then after the move in question is evaluated by the predicate that `move` calls, `can_move`.
`can_move` fetches all the theoretical moves for the attributed tile path, and filters them, checking if there are no blockages in the path and storing all the possible ones in a list. Then, it checks if the move the player chose is in the list of possible moves. If it is, the move is executed, otherwise, the player is asked to choose another move.
After the chosen move goes through `can_move` it is then realized by the predicate `replace_nested`.
In a Traxit situation, a special move is executed, different from the standard, that does no obey any paths, and therefore there are two special predicates in charge of validating and executing it: `can_move_traxit` and `move_traxit`.  They both have the same function as their twin predicates, except that they are only called when a Traxit situation occurs. can_move_traxit checks if the chosen corner is free, and move_traxit moves the chosen pawn to the chosen corner.

### List of Valid Moves

Specially for the game in question, it is important to always keep track of the list of valid moves for each player, which has to be verified each round. That is because if that list is empty, the player is said to be in Traxit, a very characteristic situation of this game. In order to do so, every round, the predicate `verify_traxit` is called. This predicate calls `valid_moves`, which generates all valid paths for the two player's pawns using the specified card and current game board, and then verifies if the list of the generated valid paths is empty.

### End of Game

The game ends when a call to `game_over` is made. When round 16 is reached, the predicate `play_game` makes that call.
`game_over` takes the final Game State and with the player's scores that the latter provides, determines who has more points and returns that player as the winner. If the scores are equal, it calls a tie. After `game_over`, the predicate `display_winner` is then called, which displays the winner or the tie, and the final score of each player, as displayed in the third element of the "**Game Notifications**" section.

### Game State Evaluation

Each round, the used path cards are discarded and the current player scores are stored.
In order to calculate a player's points based on the sum of the points amounted by the position of their 2 pawns, the predicate `value` is called each round. Afterwards, `change_round_score` amounts the score obtained by `value` to the player's total score. A score is calculated by evalutating the coordinates of each pawn and concluding in which level they are. The higher level, the higher points. Each pawn's score is then calculated (by `position_score`) and summed up to the player's total round score (by `calculate_score`).
In order to discard a card, once `choose_card` is called, the predicate `remove_card` is called, which removes the chosen card from the player's hand and returns the new hand. `choose_card` takes part of the predicate `play_round`.

### Computer Moves

Everything concerning Computer Moves is located in the `bot.pl` file.
Computer move choices are performed by the predicate `choose_move`, which calls either `best_move` or `random_move` depending on the computer's chosen difficulty. `best_move` is in charge of choosing the best move for the computer. It gathers all the possible valid moves for each pawn and calculates which of those results' ending position translates into more points. That is the move chosen.
`random _move`, on the other hand, works almost the same as `best_move`, except that it chooses a random move from the list of possible moves, rather than the best one. Therefore, best_move corresponds to the Hard Computer, and `random_move` to the Easy Computer.

## Conclusion

In conclusion, the game developed has successfully implemented the core mechanics and rules. It provides a robust and interactive platform for users to play the game, with both human and computer opponents. The game logic is well-structured and the code is modular, which makes it easy to understand and maintain.

However, there are a few limitations to the current version of the program. The AI, while functional, could be improved. The best_move function, while it does choose the most beneficial move at the moment, does not take into account future rounds or the opponent's potential moves. This can lead to suboptimal play in certain situations.

As for the roadmap, there are several improvements that could be made:

Improve the AI: The AI could be improved by implementing a more advanced algorithm, such as Minimax or Alpha-Beta pruning, which would allow the computer to plan several moves ahead.
Multiplayer Support: Adding support for more than two players could make the game more dynamic and interesting.
Game Replay: Implementing a game replay feature would allow players to review their games and learn from their mistakes.

Overall, while the game has room for improvement, it serves as a solid foundation for future development and enhancements.

## Bibliography

- Prolog Documentation. (n.d.). https://www.swi-prolog.org/pldoc/doc_for?object=manual
- Stack Overflow. (n.d.). https://stackoverflow.com/
- Geeks for Geeks. (n.d.). https://www.geeksforgeeks.org/




