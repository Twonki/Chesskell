# Chesskell

## A Game of Chess - made in Haskell

This repository contains my attempt on programming chess with Haskell.

With the first steps I want to make a normal game of chess for two players on a single machine. 
Further steps should include AI with the help of alpha-beta-pruning.

This game is never made to be in a computational-competetive enviroment. It's rather about producing nice haskellcode and having some fun with the most expressive language :)

## Build, Run and Test

### Interactive

To run the code, go to /Src and start your GHCI. 

`You@GHCI> :load Game.hs`

Should compile and load the Game. You can start the game with `You@Game> main`. 

This will let you play the game. To manually inspect features, I recommend to go to /Test and load the UnitTests. 
Part of the Unit-Tests is the *TestSuite.hs* which has a lot of useful functions and grants visibility on nearly every exposed item. 

### With Cabal

You can build, test and run the game with Cabal. For more Information on the setup, see [the cabal file](Chesskell.cabal).

```
$ cabal new-configure --enable-tests
$ cabal new-build --enable tests
$ cabal new-test
$ cabal new-install
```

For the installation you need to have symlinks configured for your cabal. After that, you can invoke the game from anywhere on your machine. 

If you're using windows, I highly recommend to change that.

**Important: Use the new-command**-Syntax - otherwise it doesn't work at all.

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

**Where to start reading?**

I think the best way to start reading is

Figures.hs -> CoreMovement.hs -> Movement.hs -> Game.hs

This way it gets more complex and the required things in movement are known.

## Contribution

You're contribution is welcome! There are several topics you can help with:

* Chess related: I'm not totally sure about the Game itself, so if there is any flaw in possible movements, or other features of the real-life-chess missing, please open an issue!
* Code-Review: Code can always improve - and mine can sure improve a lot! When you find an ugly piece of code, or know a better solution, tell me so!
* Coding: *The more the merrier* - join the programming and help me out!

If you want to help me via code, please refer to the [Contribution Guidelines](CONTRIBUTING.md).
