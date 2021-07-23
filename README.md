# Gin Rummy Game

An implementation of a two-player Gin Rummy game in Haskell, applying relevant functional programming concepts.

Gin Rummy is a game where the objective is to score 100 points before your opponent. 
Points are awarded to the player whose hand has the lowest value at the end of a round. 
The value of a hand depends on how you decide to organise your cards – called “forming melds.”
Visit https://cardgames.io/rummy/ for an idea of how the game is played.

In this project a Game AI player is implemented to play a game of Gin Rummy based on a 
heuristic strategy with memory management. For a detailed explanation on the game strategy chosen
and application of functional programming concepts, refer to this [pdf](https://github.com/J-asy/Rummy-game/blob/main/Explanation.pdf).

## Demo
To compile: `stack build` <br/>
To execute: `stack exec staticgame` <br/><br/>

The following is a demo for compiling and executing the game.
Note that in the demo shown a player is playing against another instance of itself. 
![demo gif](https://github.com/J-asy/Rummy-game/blob/main/demo/demo-gif.gif)

## Credits

Code base by [Tim Dwyer](https://ialab.it.monash.edu/~dwyer/).
Code in [Player.hs](https://github.com/J-asy/Rummy-game/blob/main/staticgame/Player.hs) is work by Joanne Ang.
