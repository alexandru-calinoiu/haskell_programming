module Main where

import           Hangman    (Puzzle (Puzzle), alreadyGuessed, fillInCharacter,
                             freshPuzzle, handleGuess)
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "fillInCharacter" $ do
        it "will add char to discovered array when guessed" $ do
            let puzzle = freshPuzzle "Test"
            let Puzzle _ discovered _ = fillInCharacter puzzle 'e'
            discovered `shouldBe` [Nothing, Just 'e', Nothing, Nothing]

        it "will not add char to discoverred array when not guessed" $ do
            let puzzle = freshPuzzle "Test"
            let Puzzle _ discovered _ = fillInCharacter puzzle 'x'
            discovered `shouldBe` [Nothing, Nothing, Nothing, Nothing]

        it "will add char to guess list wghen guessed" $ do
            let puzzle = freshPuzzle "Test"
            let Puzzle _ _ alreadyGuessed = fillInCharacter puzzle 'e'
            alreadyGuessed `shouldBe` ['e']

        it "will add char to guess list when not guessed" $ do
            let puzzle = freshPuzzle "Test"
            let Puzzle _ _ alreadyGuessed = fillInCharacter puzzle 'x'
            alreadyGuessed `shouldBe` ['x']

        it "will append chars to guess list everytime" $ do
            let puzzle = freshPuzzle "Test"
            let Puzzle _ _ alreadyGuessed = fillInCharacter (fillInCharacter puzzle 'x') 'y'
            alreadyGuessed `shouldBe` ['y', 'x']

    describe "handleGuess" $ do
        it "will not change the puzzle for a already guessed char" $ do
            puzzle <- handleGuess (freshPuzzle "Test") 'e'
            expectedPuzzle <- handleGuess puzzle 'e'
            puzzle `shouldBe` expectedPuzzle

        it "will add the char to the guessed list when it matches a char in the word" $ do
            (Puzzle _ guessed _) <- handleGuess (freshPuzzle "Test") 'e'
            guessed `shouldBe` [Nothing, Just 'e', Nothing, Nothing]

        it "will add the char to the already guessed list when it matches a char in the word" $ do
            (Puzzle _ _ alreadyGuessed) <- handleGuess (freshPuzzle "Test") 'e'
            alreadyGuessed `shouldBe` ['e']

        it "will add the char to the already guessed list when it does not match a char in the word" $ do
            puzzle <- handleGuess (freshPuzzle "Test") 'e'
            (Puzzle _ _ alreadyGuessed) <- handleGuess puzzle 'x'
            alreadyGuessed `shouldBe` ['x', 'e']
