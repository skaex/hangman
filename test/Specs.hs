module Specs where

import Test.Hspec
import Main (Puzzle(..), fillInCharacter, handleGuess)

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    let word = "puzzle"
    let puzzle = Puzzle word (map (const Nothing) word) [] 0

    it "doesn't add wrong guesses and adds guess to tries" $ do
      let expected = Puzzle word (map (const Nothing) word) ['x'] 0
      fillInCharacter puzzle 'x' `shouldBe` expected

    it "adds right guesses properly and adds guess to tries" $ do
      let expected = Puzzle word (Just 'p' : tail (map (const Nothing) word)) ['p'] 0
      fillInCharacter puzzle 'p' `shouldBe` expected

  describe "handleGuess" $ do
    let word = "puzzle"
    let puzzle = Puzzle word (map (const Nothing) word) [] 1
    
    it "deduct guess count when guess is wrong and stores guess to tries" $ do
      let expected = Puzzle word (map (const Nothing) word) ['x'] 0
      result <- handleGuess puzzle 'x'
      result `shouldBe` expected

    it "only deduct guess count when guess is already guessed" $ do
      let specialPuzzle = Puzzle word (Just 'p' : tail (map (const Nothing) word)) ['p'] 1
      let expected = Puzzle word (Just 'p' : tail (map (const Nothing) word)) ['p'] 0
      result <- handleGuess specialPuzzle 'p'
      result `shouldBe` expected

    it "does not deduct guess count on right guess and stores guess properly" $ do
      let expected = Puzzle word (Just 'p' : tail (map (const Nothing) word)) ['p'] 1
      result <- handleGuess puzzle 'p'
      result `shouldBe` expected
 
      

