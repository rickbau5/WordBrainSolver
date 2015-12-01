# WordBrainSolver
WordBrain is a mobile game that presents the player with a board of letters and some number of words with specific lengths to find. Words can only be formed by choosing letters that are adjacent to the current position. Upon finding a word, the player selects it, and it is removed from the board and the remaining letters "fall" down and the rest of the words must be found. 

This program solves these boards. The solutions can be found online, however it's much more fun to come up with the programmatic solution to the problem :) 

## Game Flow
The user is prompted with a board, for instance
```
+~~~~~~~+
|e n r d|
|l o c o|
|h b a t|
|r t r e|
+~~~~~~~+
```
and a number of words to find with specific lengths, for instance three words with length 6, 5, and 5, respectively. For each board, there is only one correct solution, so even though you may find words in the board of the correct length, they may not necessarily be solutions to this board.

In this board, the solutions are `table`, `record`, and `north` and the order, and choice of letters for each word matters. After selecting `table`, the new board will look as follows:
```
+~~~~~~~+
|- - - -|
|- n r d|
|h o c o|
|r t r e|
+~~~~~~~+
```
Notice that the columns have all settled downward in the board. This allows us to select sucessfully enter the remaining words. If, however, we entered `table` with the other t first, we would be left with the board
```
+~~~~~~~+
|- - - d|
|- - r o|
|h n c t|
|r o r e|
+~~~~~~~+
```
and would be unable to complete the complete the remaining words.

## Solver Logic
This works by taking the board and for each word specification (word that must be found) searching for every possible dictionary word that can be solved in the board, and for each found, evaluating the rest of the possible words from the resulting board and remaining specifications.

After this is done, it looks through the trees that are formed, trimming all the branches that are not the correct length (i.e. the branches that didn't find the correct number of words). 

What's left is a tree for each original spec containing all the traversals that give a complete solution

This method solves the board often for much more than a single solution, so it's up to the user to select the words that the game accepts for this particular board.

## Disclaimer
This is a quick implementation derived by me playing around with Scala and trying to better familiarize myself with it. It's not necessarily the most efficient, practical, or sexy solution, but it was a fun side project. 

This uses the dictionary on Unix machines under `/usr/share/dict/words`. I'm not sure what systems it'll work on out of the box, but Windows is clearly off the table.
