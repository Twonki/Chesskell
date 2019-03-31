# Chesskell
**A Game of Chess - made in Haskell** 

This repository contains my attempt on programming chess with Haskell.

With the first steps I want to make a normal game of chess for two players on a single machine. 
Further steps should include AI with the help of alpha-beta-pruning.

This game is never made to be in a computational-competetive enviroment. It's rather about producing nice haskellcode and having some fun with the most expressive language :)
## Structure
The file *Game.hs* contains the toplevel entrypoint for playing the game and checking all it`s features. 

The file *AllTests.hs* contains the toplevel entrypoint for running all tests at once. 

The folder *Tests* contain all tests, usually grouped by their functionality (not their src-file!). 
The tests are written with HUnit and there is an additional *TestSuite.hs* which simply puts all required visibilites in one import and enriches some helper functions. 

The folder *src*: 
* *Figures*: Contains the datatypes and some very basic operations on these datatypes. 
* *CoreMovement*: Contains simple, board-unaware movements. The results of these movements will be later filtered. 
* *Movement*: Contains advanced, board-aware movements. Also in this file is the *allMoves* Method and the *check* + *checkMate*
* *Metrics*: Contains different metrics to measure the value of a board
* *AI*(ToBeDone): Contains the alpha-beta pruning
* *Game*(ToBeDone): Contains the game logic, putting all the pieces together and IO with the player.

## Contribution
You're contribution is welcome! There are several topics you can help with:

* Chess related: I'm not totally sure about the Game itself, so if there is any flaw in possible movements, or other features of the real-life-chess missing, please open an issue!
* Code-Review: Code can always improve - and mine can sure improve a lot! When you find an ugly piece of code, or know a better solution, tell me so!
* Coding: *The more the merrier* - join the programming and help me out!

If you want to help me via code, please refer to the [Contribution Guidelines](CONTRIBUTING.md).
