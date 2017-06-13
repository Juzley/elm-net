# Elm-Net

🔌 This is an implementation of the Net puzzle game, written in [Elm](http://elm-lang.org/). You can play the game [here](https://juzley.github.io/elm-net/).

## How to play

The aim of the game is to connect all of the "network elements" to the "source" in the center of the board. This is achieved by clicking on individual tiles to rotate them; clicking on the right hand side of a tile rotates it clockwise, and vice versa.

Your score is based on how quickly you complete the game, and in how many moves - you will be penalized for taking more moves than the minimum required. Note that a "move" is counted every time you rotate a different tile: multiple consecutive rotations of a single tile will only count as one move, whereas moving a tile, moving another tile, and then moving the initial tile again will count as 3 moves.

To help you remember which tiles have already been rotated into position, it is possible to lock a tile to prevent it being rotated. To switch between "locking" and "rotating" modes, you can either press the "toggle locking" button on the left hand side of the board, or use the "L" key on the keyboard.
